"! <p class="shorttext synchronized">Search scope implementation</p>
CLASS zcl_adcoset_search_scope DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_search_scope_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS zif_adcoset_search_scope~next_package REDEFINITION.

  PROTECTED SECTION.
    METHODS determine_count REDEFINITION.
    METHODS init_from_db    REDEFINITION.

  PRIVATE SECTION.
    DATA dyn_from_clause TYPE string.
    DATA tags_dyn_where_cond TYPE string.
    DATA appl_comp_dyn_where_cond TYPE string.

    METHODS config_dyn_where_clauses.
ENDCLASS.


CLASS zcl_adcoset_search_scope IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    DATA(max_rows) = get_max_rows( ).

    SELECT obj~objecttype         AS type,
           obj~objectname         AS name,
           obj~owner,
           obj~developmentpackage AS package_name
      FROM (dyn_from_clause)
      WHERE (tags_dyn_where_cond)
        AND obj~objecttype IN @search_ranges-object_type_range
        AND obj~objectname IN @search_ranges-object_name_range
        AND obj~developmentpackage IN @search_ranges-package_range
        AND obj~owner IN @search_ranges-owner_range
        AND obj~createddate IN @search_ranges-created_on_range
        AND (appl_comp_dyn_where_cond)
      ORDER BY obj~programid
      INTO CORRESPONDING FIELDS OF TABLE @result-objects
      UP TO @max_rows ROWS
      OFFSET @current_offset.

    result-count = lines( result-objects ).
    current_offset = current_offset + result-count.

    IF result-count < max_rows.
      all_packages_read = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD determine_count.
    config_dyn_where_clauses( ).
    resolve_packages( ).

    DATA(selection_limit) = COND i( WHEN max_objects > 0
                                    THEN max_objects + 1
                                    ELSE 0 ).

    SELECT COUNT(*)
      FROM (dyn_from_clause)
      WHERE (tags_dyn_where_cond)
        AND obj~objecttype  IN @search_ranges-object_type_range
        AND obj~objectname  IN @search_ranges-object_name_range
        AND obj~developmentpackage IN @search_ranges-package_range
        AND obj~owner       IN @search_ranges-owner_range
        AND obj~createddate IN @search_ranges-created_on_range
        AND (appl_comp_dyn_where_cond)
      INTO @object_count
      UP TO @selection_limit ROWS.

    IF object_count = selection_limit.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD init_from_db.
    super->init_from_db( search_scope = search_scope  ).
    config_dyn_where_clauses( ).
  ENDMETHOD.

  METHOD config_dyn_where_clauses.
    dyn_from_clause = `ZADCOSET_I_SourceCodeObject AS obj `.

    IF search_ranges-tag_id_range IS NOT INITIAL.
      tags_dyn_where_cond = `tgobj~tag_id IN @search_ranges-tag_id_range`.

      " HINT: An object could be tagged twice and then it would appear
      "       more than once in the result -> this would result in possibly processing
      "       an object twice
      "       --> add group by clause if tags are supplied (possibly the only solution)
      dyn_from_clause = dyn_from_clause &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } AS tgobj | &&
        |ON  obj~ObjectName = tgobj~object_name | &&
        |AND obj~OriginalType = tgobj~object_type |.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      dyn_from_clause = dyn_from_clause &&
        |INNER JOIN tdevc AS pack | &&
        |ON obj~DevelopmentPackage = pack~devclass | &&
        |INNER JOIN df14l AS appl | &&
        |ON pack~component = appl~fctr_id |.

      appl_comp_dyn_where_cond = `appl~ps_posid IN @search_ranges-appl_comp_range`.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
