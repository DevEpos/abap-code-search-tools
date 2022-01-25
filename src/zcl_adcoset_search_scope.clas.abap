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
      max_objects                    TYPE i,
      "! Holds the object count depending on whether the scope was loaded from the
      "! database or not
      obj_count_for_package_building TYPE i,
      object_count                   TYPE i,
      current_offset                 TYPE i,
      all_packages_read              TYPE abap_bool,
      package_size                   TYPE i VALUE c_serial_package_size,
      is_more_objects_available      TYPE abap_bool.

    METHODS:
      determine_count,
      resolve_packages,
      init_scope_from_db
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope,
      init_scope
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.
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

    SELECT object_type AS type,
           object_name AS name,
           owner,
           devclass AS package_name
      FROM zadcoset_repoobj
      WHERE object_type IN @search_ranges-object_type_range
        AND object_name IN @search_ranges-object_name_range
        AND ps_posid IN @search_ranges-appl_comp_range
        AND devclass IN @search_ranges-package_range
        AND owner IN @search_ranges-owner_range
        AND created_date IN @search_ranges-created_on_range
      ORDER BY object_name
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
      FROM zadcoset_repoobj
      WHERE object_type IN @search_ranges-object_type_range
        AND object_name IN @search_ranges-object_name_range
        AND ps_posid IN @search_ranges-appl_comp_range
        AND devclass IN @search_ranges-package_range
        AND owner IN @search_ranges-owner_range
        AND created_date IN @search_ranges-created_on_range
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

  ENDMETHOD.


  METHOD init_scope.
    max_objects = search_scope-max_objects.
    search_ranges = search_scope-ranges.
    resolve_packages( ).
    determine_count( ).

    obj_count_for_package_building = object_count.
  ENDMETHOD.

ENDCLASS.
