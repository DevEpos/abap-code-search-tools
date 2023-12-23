*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_limu_processor IMPLEMENTATION.
  METHOD constructor.
    me->tr_objects          = tr_objects.
    me->filter_object_types = filter_object_types.
  ENDMETHOD.

  METHOD run.
    LOOP AT tr_objects ASSIGNING FIELD-SYMBOL(<r3tr_object>)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-r3tr.
      objects = VALUE #( BASE objects
                         ( type = <r3tr_object>-obj_type
                           name = <r3tr_object>-obj_name ) ).
    ENDLOOP.
    LOOP AT tr_objects ASSIGNING FIELD-SYMBOL(<limu_object>)
         WHERE pgmid = zif_adcoset_c_global=>c_program_id-limu.
      handle_limu_object( <limu_object> ).
    ENDLOOP.
    result = objects.
  ENDMETHOD.

  METHOD handle_limu_object.
    CASE limu_object-obj_type.
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-function_module.
        handle_function_module( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-report_source_code.
        handle_report_source_code( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-method.
        handle_class_method( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_include.
        handle_class_include( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_private_section.
        handle_class_private_section( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_public_section.
        handle_class_public_section( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section.
        handle_class_protected_section( limu_object ).
      WHEN zif_adcoset_c_global=>c_source_code_limu_type-class_definition.
        handle_class_definition( limu_object ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_function_module.
    DATA func_group_name TYPE rs38l_area.

    DATA(funcname) = CONV rs38l_fnam( limu_object-obj_name ).
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
                       subobjects       = VALUE #( ( name = limu_object-obj_name type = limu_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object      = limu_object
                    main_object_name = CONV #( func_group_name )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-function_group ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_result.
    objects = VALUE #( BASE objects
                       ( type       = main_object_type
                         name       = main_object_name
                         subobjects = VALUE zif_adcoset_ty_global=>ty_tadir_object_infos(
                                                ( type = limu_object-obj_type
                                                  name = limu_object-obj_name ) ) ) ).
  ENDMETHOD.

  METHOD add_result_cl_definition.
    objects = VALUE #(
        BASE objects
        ( type       = main_object_type
          name       = main_object_name
          subobjects = VALUE #(
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                name = cl_oo_classname_service=>get_pubsec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) )
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                name = cl_oo_classname_service=>get_prisec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) )
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                name = cl_oo_classname_service=>get_prosec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD add_subobject.
    DATA(main_object) = REF #( objects[ type = main_object_type
                                        name = main_object_name ] ).
    " if main object already exists as a complete object, sub objects are not needed anymore because the whole object will be
    " used for the search
    IF NOT line_exists( tr_objects[ pgmid    = zif_adcoset_c_global=>c_program_id-r3tr
                                    obj_type = main_object_type
                                    obj_name = main_object_name ] ).
      LOOP AT subobjects ASSIGNING FIELD-SYMBOL(<subobject>).
        INSERT <subobject> INTO TABLE main_object->subobjects.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_report_source_code.
    DATA main_obj TYPE tadir.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING wi_e071  = CORRESPONDING e071( limu_object MAPPING object = obj_type )
      IMPORTING we_tadir = main_obj.

    " REPS object can come from different main types
    " if the determined main type does not match filters, object can be ignored
    IF main_obj-object NOT IN filter_object_types.
      RETURN.
    ENDIF.

    TRY.
        add_subobject( main_object_name = main_obj-obj_name
                       main_object_type = main_obj-object
                       subobjects       = VALUE #( ( name = limu_object-obj_name type = limu_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object      = limu_object
                    main_object_name = main_obj-obj_name
                    main_object_type = main_obj-object ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_method.
    TRY.
        add_subobject(
            main_object_name = CONV #( limu_object-obj_name(30) ) " class_name.
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = limu_object-obj_name+30(30) type = limu_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object      = VALUE #( trkorr   = limu_object-trkorr
                                                pgmid    = limu_object-pgmid
                                                obj_type = limu_object-obj_type
                                                obj_name = limu_object-obj_name+30(30) )
                    main_object_name = CONV #( limu_object-obj_name(30) ) " class_name.
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_include.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = limu_object-obj_name type = limu_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object      = limu_object
                    main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_section.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = section_include
                                          type = limu_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( limu_object      = VALUE #( obj_name = section_include
                                                obj_type = limu_object-obj_type )
                    main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_private_section.
    handle_class_section(
        limu_object     = limu_object
        section_include = cl_oo_classname_service=>get_prisec_name(
            CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_protected_section.
    handle_class_section(
        limu_object     = limu_object
        section_include = cl_oo_classname_service=>get_prosec_name(
            CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_public_section.
    handle_class_section(
        limu_object     = limu_object
        section_include = cl_oo_classname_service=>get_pubsec_name(
            CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_definition.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #(
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                  name = cl_oo_classname_service=>get_pubsec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) )
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                  name = cl_oo_classname_service=>get_prisec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) )
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                  name = cl_oo_classname_service=>get_prosec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) ) ) ) ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result_cl_definition(
            limu_object      = limu_object
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( limu_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
