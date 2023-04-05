"! <p class="shorttext synchronized" lang="en">Search scope implementation</p>
CLASS zcl_adcoset_search_scope DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_adcoset_search_scope.

    METHODS:
      constructor
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_min_parl_package_size TYPE i VALUE 10,
      c_max_parl_package_size TYPE i VALUE 2500,
      c_serial_package_size   TYPE i VALUE 10000.

    DATA:
      search_ranges                  TYPE zif_adcoset_ty_global=>ty_search_scope_ranges,
      "! Restricts the maximum number of objects to select for the search
      max_objects                    TYPE i,
      "! Holds the object count depending on whether the scope was loaded from the
      "! database or not
      obj_count_for_package_building TYPE i,
      "! This holds the object count for the current scope
      object_count                   TYPE i,
      current_offset                 TYPE i,
      all_packages_read              TYPE abap_bool,
      package_size                   TYPE i VALUE c_serial_package_size,
      is_more_objects_available      TYPE abap_bool,
      dyn_from_clause                TYPE string,
      tags_dyn_where_cond            TYPE string,
      appl_comp_dyn_where_cond       TYPE string.

    METHODS:
      determine_count,
      resolve_packages,
      init_scope_from_db
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope,
      init_scope
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope,
      increase_scope_expiration
        IMPORTING
          scope_id TYPE sysuuid_x16,
      config_dyn_where_clauses.
ENDCLASS.



CLASS zcl_adcoset_search_scope IMPLEMENTATION.


  METHOD constructor.
    IF search_scope-scope_id IS NOT INITIAL.
      init_scope_from_db( search_scope ).
    ELSE.
      init_scope( search_scope ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~count.
    result = object_count.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~has_next_package.
    result = xsdbool( all_packages_read = abap_false AND current_offset < object_count ).
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~next_package.
    DATA(max_rows) = package_size.
    IF current_offset IS INITIAL AND
        ( object_count  < package_size OR package_size = 0 ).
      max_rows = object_count.
    ENDIF.

    SELECT obj~object_type AS type,
           obj~object_name AS name,
           obj~owner,
           obj~devclass AS package_name
      FROM (dyn_from_clause)
      WHERE (tags_dyn_where_cond)
        AND obj~object_type IN @search_ranges-object_type_range
        AND obj~object_name IN @search_ranges-object_name_range
        AND obj~devclass IN @search_ranges-package_range
        AND obj~owner IN @search_ranges-owner_range
        AND obj~created_date IN @search_ranges-created_on_range
        AND (appl_comp_dyn_where_cond)
      ORDER BY obj~pgmid
      INTO CORRESPONDING FIELDS OF TABLE @result
      UP TO @max_rows ROWS
      OFFSET @current_offset.

    DATA(package_result_count) = lines( result ).
    current_offset = current_offset + package_result_count.

    IF package_result_count < max_rows.
      all_packages_read = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~more_objects_in_scope.
    result = is_more_objects_available.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~get_scope_ranges.
    result = search_ranges.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~configure_package_size.
    CHECK max_task_count > 0.

    IF max_objects IS NOT INITIAL.
      " update max objects number
      me->max_objects =
        package_size = max_objects.
      " set fixed object count
      object_count = max_objects + current_offset.
      obj_count_for_package_building = max_objects.
    ENDIF.

    DATA(determined_pack_size) = obj_count_for_package_building / max_task_count.

    IF determined_pack_size < c_min_parl_package_size.
      package_size = c_min_parl_package_size.
    ELSEIF determined_pack_size > c_max_parl_package_size.
      package_size = c_max_parl_package_size.
    ELSE.
      package_size = determined_pack_size.
    ENDIF.

  ENDMETHOD.


  METHOD determine_count.
    DATA(selection_limit) = COND i(
      WHEN max_objects > 0 THEN max_objects + 1
                           ELSE 0 ).

    SELECT COUNT(*)
      FROM (dyn_from_clause)
      WHERE (tags_dyn_where_cond)
        AND obj~object_type IN @search_ranges-object_type_range
        AND obj~object_name IN @search_ranges-object_name_range
        AND obj~devclass IN @search_ranges-package_range
        AND obj~owner IN @search_ranges-owner_range
        AND obj~created_date IN @search_ranges-created_on_range
        AND (appl_comp_dyn_where_cond)
      INTO @object_count
      UP TO @selection_limit ROWS.

    IF object_count = selection_limit.
      object_count = max_objects.
      is_more_objects_available = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD resolve_packages.
    DATA: include_package_range TYPE zif_adcoset_ty_global=>ty_package_name_range,
          exclude_package_range TYPE zif_adcoset_ty_global=>ty_package_name_range.

    FIELD-SYMBOLS: <package_range> TYPE LINE OF zif_adcoset_ty_global=>ty_package_name_range.

    CHECK search_ranges-package_range IS NOT INITIAL.

    " only determine sub packages from ranges with option EQ
    LOOP AT search_ranges-package_range ASSIGNING <package_range> WHERE option = 'EQ'.
      IF <package_range>-sign = 'I'.
        include_package_range = VALUE #( BASE include_package_range ( <package_range> ) ).
      ELSEIF <package_range>-sign = 'E'.
        exclude_package_range = VALUE #( BASE exclude_package_range ( sign = 'I' option = 'EQ' low = <package_range>-low ) ).
      ENDIF.
      DELETE search_ranges-package_range.
    ENDLOOP.

    " collect sub packages of packages that should be excluded
    exclude_package_range = VALUE #( BASE exclude_package_range
      ( LINES OF zcl_adcoset_devc_reader=>get_subpackages_by_range( exclude_package_range ) ) ).

    " convert sign back to 'E' for the excluded packages
    LOOP AT exclude_package_range ASSIGNING <package_range>.
      <package_range>-sign = 'E'.
    ENDLOOP.

    search_ranges-package_range = VALUE #( BASE search_ranges-package_range
      ( LINES OF include_package_range )
      ( LINES OF zcl_adcoset_devc_reader=>get_subpackages_by_range( include_package_range ) )
      ( LINES OF exclude_package_range ) ).

    SORT search_ranges-package_range BY sign option low high.
    DELETE ADJACENT DUPLICATES FROM search_ranges-package_range COMPARING sign option low high.
  ENDMETHOD.


  METHOD init_scope_from_db.
    SELECT SINGLE *
      FROM zadcoset_csscope
      WHERE id = @search_scope-scope_id
      INTO @DATA(scope_db).

    IF sy-subrc <> 0.
      object_count = 0.
      RETURN.
    ENDIF.

    increase_scope_expiration( scope_id = search_scope-scope_id ).

    current_offset = search_scope-current_offset.
    max_objects =
      package_size = search_scope-max_objects.
    " set fixed object count
    object_count = max_objects + current_offset.

    obj_count_for_package_building = max_objects.

    IF scope_db-ranges_data IS NOT INITIAL.
      CALL TRANSFORMATION id
        SOURCE XML scope_db-ranges_data
        RESULT data = search_ranges.
    ENDIF.

    config_dyn_where_clauses( ).

  ENDMETHOD.


  METHOD init_scope.
    max_objects = search_scope-max_objects.
    search_ranges = search_scope-ranges.

    config_dyn_where_clauses( ).
    resolve_packages( ).
    determine_count( ).

    obj_count_for_package_building = object_count.
  ENDMETHOD.


  METHOD increase_scope_expiration.
    DATA: expiration TYPE zadcoset_csscope-expiration_datetime.

    GET TIME STAMP FIELD expiration.
    expiration = cl_abap_tstmp=>add(
      tstmp = expiration
      secs  = zif_adcoset_c_global=>c_default_scope_expiration ).

    UPDATE zadcoset_csscope SET expiration_datetime = expiration
                            WHERE id = scope_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD config_dyn_where_clauses.
    dyn_from_clause = `ZADCOSET_SRCCOBJ AS obj `.

    IF search_ranges-tag_id_range IS NOT INITIAL.
      tags_dyn_where_cond = `tgobj~tag_id IN @search_ranges-tag_id_range`.

      " HINT: An object could be tagged twice and then it would appear
      "       more than once in the result -> this would result in possibly processing
      "       an object twice
      "       --> add group by clause if tags are supplied (possibly the only solution)
      dyn_from_clause = dyn_from_clause &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } AS tgobj | &&
        `ON  obj~object_name = tgobj~object_name ` &&
        `AND obj~object_type = tgobj~object_type `.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      dyn_from_clause = dyn_from_clause &&
        `INNER JOIN tdevc AS pack ` &&
        `ON obj~devclass = pack~devclass ` &&
        `INNER JOIN df14l AS appl ` &&
        `ON pack~component = appl~fctr_id `.

      appl_comp_dyn_where_cond = `appl~ps_posid IN @search_ranges-appl_comp_range`.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
