"! <p class="shorttext synchronized">Search scope for transport request</p>
CLASS zcl_adcoset_search_scope_tr DEFINITION
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

  PRIVATE SECTION.
    DATA limu_processor TYPE REF TO lcl_limu_processor.

    "! Read Source Code Objects from Transport Requests
    METHODS get_tr_objects
      IMPORTING
        max_rows      TYPE i
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tr_request_objects.

    "! Enhance the type filter with corresponding LIMU types
    METHODS add_subobj_type_to_filter.

    METHODS extract_r3tr_objects
      IMPORTING
        tr_objects    TYPE zif_adcoset_ty_global=>ty_tr_request_objects
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects.

    METHODS resolve_tr_request.

ENDCLASS.


CLASS zcl_adcoset_search_scope_tr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    DATA(max_rows) = package_size.
    IF     current_offset IS INITIAL
       AND ( object_count < package_size OR package_size = 0 ).
      max_rows = object_count.
    ENDIF.

    DATA(tr_objects) = get_tr_objects( max_rows ).

    IF line_exists( tr_objects[ pgmid = zif_adcoset_c_global=>c_program_id-limu ] ).
      result = VALUE #( count   = lines( tr_objects )
                        objects = NEW lcl_limu_processor(
                                          tr_objects          = tr_objects
                                          filter_object_types = search_ranges-object_type_range )->run( ) ).
    ELSE.
      result = VALUE #( count   = lines( tr_objects )
                        objects = extract_r3tr_objects( tr_objects ) ).
    ENDIF.

    current_offset = current_offset + result-count.

    IF current_offset < max_rows.
      all_packages_read = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD determine_count.
    resolve_tr_request( ).

    DATA(selection_limit) = COND i(
    WHEN max_objects > 0
    THEN max_objects + 1
    ELSE 0 ).

    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    add_subobj_type_to_filter( ).

    WITH +e071_aggr AS (
       SELECT DISTINCT programid  AS pgmid,
                       objecttype AS obj_type,
                       objectname AS obj_name
         FROM zadcoset_transportsourcecodobj
         WHERE objecttype IN @search_ranges-object_type_range
           AND objectname IN @search_ranges-object_name_range
           AND request    IN @search_ranges-tr_request_range )

    SELECT COUNT(*) FROM +e071_aggr
      INTO @object_count
      UP TO @selection_limit ROWS.

    IF object_count = selection_limit.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_tr_objects.
    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT programid  AS pgmid,
                    objecttype AS obj_type,
                    objectname AS obj_name
      FROM zadcoset_transportsourcecodobj
      WHERE objecttype IN @search_ranges-object_type_range
        AND objectname IN @search_ranges-object_name_range
        AND request    IN @search_ranges-tr_request_range
      ORDER BY obj_name,
               pgmid,
               obj_type
      INTO CORRESPONDING FIELDS OF TABLE @result
      UP TO @max_rows ROWS
      OFFSET @current_offset.
  ENDMETHOD.

  METHOD add_subobj_type_to_filter.
    IF line_exists( search_ranges-object_type_range[ low = zif_adcoset_c_global=>c_source_code_type-class ] ).
      search_ranges-object_type_range = VALUE #(
          BASE search_ranges-object_type_range
          sign   = search_ranges-object_type_range[ low = zif_adcoset_c_global=>c_source_code_type-class ]-sign
          option = 'EQ'
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_definition )
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_include )
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_private_section )
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section )
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_public_section )
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-method ) ).

    ENDIF.

    IF line_exists( search_ranges-object_type_range[ low = zif_adcoset_c_global=>c_source_code_type-function_group ] ).
      search_ranges-object_type_range = VALUE #(
          BASE search_ranges-object_type_range
          sign   = search_ranges-object_type_range[ low = zif_adcoset_c_global=>c_source_code_type-function_group ]-sign
          option = 'EQ'
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-function_module ) ).
    ENDIF.

    IF    line_exists( search_ranges-object_type_range[
                           sign = 'I'
                           low  = zif_adcoset_c_global=>c_source_code_type-function_group ] )
       OR line_exists( search_ranges-object_type_range[ sign = 'I'
                                                        low  = zif_adcoset_c_global=>c_source_code_type-program ] ).
      search_ranges-object_type_range = VALUE #(
          BASE search_ranges-object_type_range
          sign   = 'I'
          option = 'EQ'
          ( low =  zif_adcoset_c_global=>c_source_code_limu_type-report_source_code ) ).
    ENDIF.
  ENDMETHOD.

  METHOD extract_r3tr_objects.
    LOOP AT tr_objects ASSIGNING FIELD-SYMBOL(<tr_object>)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-r3tr.
      result = VALUE #( BASE result
                        ( type = <tr_object>-obj_type
                          name = <tr_object>-obj_name ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD resolve_tr_request.
    CHECK search_ranges-tr_request_range IS NOT INITIAL.

    SELECT trkorr FROM e070
      WHERE strkorr IN @search_ranges-tr_request_range
      INTO TABLE @DATA(tr_tasks).

    search_ranges-tr_request_range = VALUE #( BASE search_ranges-tr_request_range FOR task IN tr_tasks
                                              ( sign = 'I' option = 'EQ' low = task ) ).
  ENDMETHOD.
ENDCLASS.
