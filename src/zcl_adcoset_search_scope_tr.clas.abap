"! <p class="shorttext synchronized">Search scope for transport request</p>
CLASS zcl_adcoset_search_scope_tr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_scope.

    METHODS constructor
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

  PRIVATE SECTION.
    CONSTANTS c_min_parl_package_size TYPE i VALUE 10.
    CONSTANTS c_max_parl_package_size TYPE i VALUE 2500.
    CONSTANTS c_serial_package_size TYPE i VALUE 10000.

    DATA limu_processor TYPE REF TO lcl_limu_processor.
    DATA search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    "! Restricts the maximum number of objects to select for the search
    DATA max_objects TYPE i.
    "! This holds the object count for the current scope
    DATA object_count TYPE i.
    DATA trs_processed TYPE abap_bool.
    DATA current_offset TYPE i.
    DATA package_size TYPE i VALUE c_serial_package_size.
    DATA is_more_objects_available TYPE abap_bool.
    DATA all_packages_read TYPE abap_bool.
    "! Holds the object count depending on whether the scope was loaded from the
    "! database or not
    DATA obj_count_for_package_building TYPE i.

    METHODS determine_count.

    METHODS init_scope
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS init_scope_from_db
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS increase_scope_expiration
      IMPORTING
        scope_id TYPE sysuuid_x16.

    METHODS get_tr_objects
      IMPORTING
        max_rows          TYPE i
      RETURNING
        VALUE(tr_objects) TYPE zif_adcoset_ty_global=>ty_tr_request_objects.

    METHODS determine_tadir_obj_for_limu
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

ENDCLASS.


CLASS zcl_adcoset_search_scope_tr IMPLEMENTATION.
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
    DATA result_main_objects TYPE zif_adcoset_ty_global=>ty_tadir_objects_deep.

    DATA(max_rows) = package_size.
    IF     current_offset IS INITIAL
       AND ( object_count < package_size OR package_size = 0 ).
      max_rows = object_count.
    ENDIF.

    DATA(tr_objects) = get_tr_objects( max_rows ).

    LOOP AT tr_objects ASSIGNING FIELD-SYMBOL(<tr_r3tr_object>)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-r3tr.
      result_main_objects = VALUE #( BASE result_main_objects
                                     ( type                 = <tr_r3tr_object>-obj_type
                                       name                 = <tr_r3tr_object>-obj_name
                                       searched_objs_count  = 1
                                       complete_main_object = abap_true ) ).
    ENDLOOP.

    limu_processor = NEW lcl_limu_processor( result_main_objects ).
    LOOP AT tr_objects ASSIGNING FIELD-SYMBOL(<tr_limu_object>)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-limu.
      determine_tadir_obj_for_limu( tr_object = <tr_limu_object> ).
    ENDLOOP.

    result = VALUE #( BASE result ( LINES OF limu_processor->result ) ).

    DATA(package_result_count) = 0.
    LOOP AT result ASSIGNING FIELD-SYMBOL(<result_line>).
      package_result_count = package_result_count + <result_line>-searched_objs_count.
    ENDLOOP.

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
      package_size = max_objects.
      me->max_objects = max_objects.
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
    WHEN max_objects > 0
    THEN max_objects + 1
    ELSE 0 ).

    " ToDo Ludwig
*    SELECT COUNT(*)
*      FROM zadcoset_transportsourcecodobj
*         WHERE objecttype IN @search_ranges-object_type_range
*           AND objectname IN @search_ranges-object_name_range
*           AND request    IN @search_ranges-tr_request_range
*         INTO @object_count
*         UP TO @selection_limit ROWS.
    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT
        programid AS pgmid,
        objecttype AS obj_type,
        objectname AS obj_name
      FROM zadcoset_transportsourcecodobj
      WHERE objecttype IN @search_ranges-object_type_range
        AND objectname IN @search_ranges-object_name_range
        AND request    IN @search_ranges-tr_request_range
      ORDER BY obj_name, pgmid, obj_type
      INTO TABLE @DATA(objects)
      UP TO @selection_limit ROWS.

    object_count = lines( objects ).

    IF object_count = selection_limit.
      object_count = max_objects.
      is_more_objects_available = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD init_scope.
    max_objects = search_scope-max_objects.
    search_ranges = search_scope-ranges.

    determine_count( ).

    obj_count_for_package_building = object_count.
  ENDMETHOD.

  METHOD increase_scope_expiration.
    DATA expiration TYPE zadcoset_csscope-expiration_datetime.

    GET TIME STAMP FIELD expiration.
    expiration = cl_abap_tstmp=>add( tstmp = expiration
                                     secs  = zif_adcoset_c_global=>c_default_scope_expiration ).

    UPDATE zadcoset_csscope SET expiration_datetime = expiration
                            WHERE id = scope_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
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
    package_size = search_scope-max_objects.
    max_objects = search_scope-max_objects.
    " set fixed object count
    object_count = max_objects + current_offset.

    obj_count_for_package_building = max_objects.

    IF scope_db-ranges_data IS NOT INITIAL.
      CALL TRANSFORMATION id
           SOURCE XML scope_db-ranges_data
           RESULT data = search_ranges.
    ENDIF.
  ENDMETHOD.

  METHOD get_tr_objects.
    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT
        programid AS pgmid,
        objecttype AS obj_type,
        objectname AS obj_name
      FROM zadcoset_transportsourcecodobj
      WHERE objecttype IN @search_ranges-object_type_range
        AND objectname IN @search_ranges-object_name_range
        AND request    IN @search_ranges-tr_request_range
      ORDER BY obj_name, pgmid, obj_type
      INTO CORRESPONDING FIELDS OF TABLE @tr_objects
      UP TO @max_rows ROWS
      OFFSET @current_offset.
  ENDMETHOD.

  METHOD determine_tadir_obj_for_limu.
    CASE tr_object-obj_type.
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-function_module.
        limu_processor->handle_function_module( tr_object = tr_object  ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-report_source_code.
        limu_processor->handle_report_source_code( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-method.
        limu_processor->handle_class_method( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_include.
        limu_processor->handle_class_include( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_private_section.
        limu_processor->handle_class_private_section( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_public_section.
        limu_processor->handle_class_public_section( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section.
        limu_processor->handle_class_protected_section( tr_object = tr_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_definition.
        limu_processor->handle_class_definition( tr_object = tr_object ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
