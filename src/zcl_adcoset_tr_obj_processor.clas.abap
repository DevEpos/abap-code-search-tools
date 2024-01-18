CLASS zcl_adcoset_tr_obj_processor DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        tr_objects    TYPE zif_adcoset_ty_global=>ty_tr_request_objects_srt
        search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    METHODS run
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects.

  PRIVATE SECTION.
    DATA objects TYPE zif_adcoset_ty_global=>ty_tadir_objects.
    DATA tr_objects TYPE zif_adcoset_ty_global=>ty_tr_request_objects_srt.
    DATA search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    METHODS handle_limu_object
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_function_module
      IMPORTING
        func_name TYPE trobj_name.

    METHODS handle_report_source_code
      IMPORTING
        reps_name TYPE trobj_name.

    METHODS handle_class_method
      IMPORTING
        class_name  TYPE sobj_name
        method_name TYPE seocpdname.

    METHODS handle_class_include
      IMPORTING
        class_name      TYPE sobj_name
        class_incl_name TYPE seocpdname.

    METHODS handle_class_section
      IMPORTING
        section_include TYPE program
        class_incl_type TYPE trobjtype
        class_name      TYPE seoclsname.

    METHODS handle_class_private_section
      IMPORTING
        class_name TYPE seoclsname.

    METHODS handle_class_protected_section
      IMPORTING
        class_name TYPE seoclsname.

    METHODS handle_class_public_section
      IMPORTING
        class_name TYPE seoclsname.

    METHODS handle_class_definition
      IMPORTING
        class_name TYPE seoclsname.

    METHODS add_subobject
      IMPORTING
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype
        subobjects       TYPE zif_adcoset_ty_global=>ty_limu_objects_srt
      RAISING
        cx_sy_itab_line_not_found.

    METHODS add_result
      IMPORTING
        limu_object_name TYPE seocpdname
        limu_object_type TYPE trobjtype
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype.

    METHODS add_result_cl_definition
      IMPORTING
        class_name TYPE seoclsname.

    METHODS post_filter.

    METHODS extract_r3tr_objects
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects.

ENDCLASS.


CLASS zcl_adcoset_tr_obj_processor IMPLEMENTATION.
  METHOD constructor.
    me->tr_objects    = tr_objects.
    me->search_ranges = search_ranges.
  ENDMETHOD.

  METHOD run.
    objects = extract_r3tr_objects( ).

    LOOP AT tr_objects REFERENCE INTO DATA(limu_object)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-limu.
      handle_limu_object( limu_object->* ).
    ENDLOOP.

    IF sy-subrc <> 0. " no limu objects found, post filter not needed
      result = objects. " return r3tr objects
      RETURN.
    ENDIF.

    post_filter( ).
    result = objects.
  ENDMETHOD.

  METHOD handle_limu_object.
    CASE limu_object-obj_type.
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-function_module.
        handle_function_module( limu_object-obj_name ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-report_source_code.
        handle_report_source_code( limu_object-obj_name ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_definition.
        handle_class_definition( cl_oo_classname_service=>get_clsname_by_include( |{ limu_object-obj_name }| ) ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_private_section.
        handle_class_private_section( cl_oo_classname_service=>get_clsname_by_include( |{ limu_object-obj_name }| ) ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_public_section.
        handle_class_public_section( cl_oo_classname_service=>get_clsname_by_include( |{ limu_object-obj_name }| ) ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section.
        handle_class_protected_section( cl_oo_classname_service=>get_clsname_by_include( |{ limu_object-obj_name }| ) ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-method.
        handle_class_method( class_name  = |{ limu_object-obj_name(30) }|
                             method_name = |{ limu_object-obj_name+30(61) }| ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_include.
        handle_class_include(
            class_name      = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
            class_incl_name = |{ limu_object-obj_name }| ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_function_module.
    DATA func_group_name TYPE rs38l_area.

    DATA(funcname) = CONV rs38l_fnam( func_name ).
    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      CHANGING   funcname            = funcname
                 group               = func_group_name
      EXCEPTIONS function_not_exists = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        add_subobject( main_object_name = CONV #( func_group_name )
                       main_object_type = zif_adcoset_c_global=>c_source_code_type-function_group
                       subobjects       = VALUE #(
                           ( name = func_name
                             type = zif_adcoset_c_global=>c_source_code_limu_type-function_module ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object_name = CONV #( func_name )
                    limu_object_type = zif_adcoset_c_global=>c_source_code_limu_type-function_module
                    main_object_name = CONV #( func_group_name )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-function_group ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_result.
    objects = VALUE #(
        BASE objects
        ( type         = main_object_type
          name         = main_object_name
          limu_objects = VALUE zif_adcoset_ty_global=>ty_limu_objects_srt( ( type = limu_object_type
                                                                             name = limu_object_name ) ) ) ).
  ENDMETHOD.

  METHOD add_result_cl_definition.
    objects = VALUE #(
        BASE objects
        ( type         = zif_adcoset_c_global=>c_source_code_type-class
          name         = class_name
          limu_objects = VALUE #( ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                                    name = cl_oo_classname_service=>get_pubsec_name( class_name ) )
                                  ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                                    name = cl_oo_classname_service=>get_prisec_name( class_name ) )
                                  ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                                    name = cl_oo_classname_service=>get_prosec_name( class_name ) ) ) ) ).
  ENDMETHOD.

  METHOD add_subobject.
    DATA(main_object) = REF #( objects[ type = main_object_type
                                        name = main_object_name ] ).
    " if main object already exists as a complete object, sub objects are not needed anymore because the whole object will be
    " used for the search
    IF NOT line_exists( tr_objects[ pgmid    = zif_adcoset_c_global=>c_program_id-r3tr
                                    obj_type = main_object_type
                                    obj_name = main_object_name ] ).
      LOOP AT subobjects REFERENCE INTO DATA(subobject).
        INSERT subobject->* INTO TABLE main_object->limu_objects.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_report_source_code.
    DATA main_obj TYPE tadir.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING wi_e071  = VALUE e071( pgmid    = zif_adcoset_c_global=>c_program_id-limu
                                       object   = zif_adcoset_c_global=>c_source_code_limu_type-report_source_code
                                       obj_name = reps_name )
      IMPORTING we_tadir = main_obj.

    " REPS object can come from different main types
    " if the determined main type does not match filters, object can be ignored
    IF main_obj-object NOT IN search_ranges-object_type_range.
      RETURN.
    ENDIF.

    TRY.
        add_subobject(
            main_object_name = main_obj-obj_name
            main_object_type = main_obj-object
            subobjects       = VALUE #( ( name = reps_name
                                          type = zif_adcoset_c_global=>c_source_code_limu_type-report_source_code ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object_name = CONV #( reps_name )
                    limu_object_type = zif_adcoset_c_global=>c_source_code_limu_type-report_source_code
                    main_object_name = main_obj-obj_name
                    main_object_type = main_obj-object ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_method.
    TRY.
        add_subobject(
            main_object_name = class_name
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = method_name type = zif_adcoset_c_global=>c_source_code_limu_type-method ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object_name = method_name
                    limu_object_type = zif_adcoset_c_global=>c_source_code_limu_type-method
                    main_object_name = class_name
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_include.
    TRY.
        add_subobject(
            main_object_name = class_name
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = class_incl_name type = zif_adcoset_c_global=>c_source_code_limu_type-class_include ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object_name = class_incl_name
                    limu_object_type = zif_adcoset_c_global=>c_source_code_limu_type-class_include
                    main_object_name = class_name
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_section.
    TRY.
        add_subobject( main_object_name = |{ class_name }|
                       main_object_type = zif_adcoset_c_global=>c_source_code_type-class
                       subobjects       = VALUE #( ( name = section_include
                                                     type = class_incl_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object_name = CONV #( section_include )
                    limu_object_type = class_incl_type
                    main_object_name = |{ class_name }|
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_private_section.
    handle_class_section( class_incl_type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                          section_include = cl_oo_classname_service=>get_prisec_name( class_name )
                          class_name      = class_name ).
  ENDMETHOD.

  METHOD handle_class_protected_section.
    handle_class_section( class_incl_type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                          section_include = cl_oo_classname_service=>get_prosec_name( class_name )
                          class_name      = class_name ).
  ENDMETHOD.

  METHOD handle_class_public_section.
    handle_class_section( class_incl_type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                          section_include = cl_oo_classname_service=>get_pubsec_name( class_name )
                          class_name      = class_name ).
  ENDMETHOD.

  METHOD handle_class_definition.
    TRY.
        add_subobject(
            main_object_name = |{ class_name }|
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                                          name = cl_oo_classname_service=>get_pubsec_name( class_name ) )
                                        ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                                          name = cl_oo_classname_service=>get_prisec_name( class_name ) )
                                        ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                                          name = cl_oo_classname_service=>get_prosec_name( class_name ) ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result_cl_definition( class_name = class_name ).
    ENDTRY.
  ENDMETHOD.

  METHOD post_filter.
    DATA tadir_object_details TYPE zif_adcoset_ty_global=>ty_tadir_object_infos_srt.

    DATA(tadir_objects_no_details) = objects.
    DELETE tadir_objects_no_details WHERE package_name IS NOT INITIAL.

    IF tadir_objects_no_details IS INITIAL.
      RETURN.
    ENDIF.

    SELECT objectname         AS name,
           objecttype         AS type,
           developmentpackage AS package_name,
           owner
      FROM zadcoset_i_sourcecodeobject
      FOR ALL ENTRIES IN @tadir_objects_no_details
      WHERE objectname          = @tadir_objects_no_details-name
        AND objecttype          = @tadir_objects_no_details-type
        AND createddate        IN @search_ranges-created_on_range
        AND developmentpackage IN @search_ranges-package_range
        AND owner              IN @search_ranges-owner_range
      INTO CORRESPONDING FIELDS OF TABLE @tadir_object_details.

    LOOP AT objects REFERENCE INTO DATA(tadir_object)
         WHERE package_name IS INITIAL.
      TRY.
          DATA(detailed_tadir_object) = tadir_object_details[ type = tadir_object->type
                                                              name = tadir_object->name ].
          tadir_object->package_name = detailed_tadir_object-package_name.
          tadir_object->owner        = detailed_tadir_object-owner.
        CATCH cx_sy_itab_line_not_found.
          DELETE objects. " the current line does not meet the filter for created date
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_r3tr_objects.
    LOOP AT tr_objects REFERENCE INTO DATA(tr_object)
         WHERE     pgmid         = zif_adcoset_c_global=>c_program_id-r3tr
               AND created_date IN search_ranges-created_on_range
               AND package_name IN search_ranges-package_range
               AND owner        IN search_ranges-owner_range.
      result = VALUE #( BASE result
                        ( type         = tr_object->obj_type
                          name         = tr_object->obj_name
                          package_name = tr_object->package_name
                          owner        = tr_object->owner ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
