*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_limu_processor IMPLEMENTATION.
  METHOD constructor.
    me->objects             = objects.
    me->filter_object_types = filter_object_types.
  ENDMETHOD.

  METHOD handle_function_module.
    DATA func_group_name TYPE rs38l_area.

    DATA(funcname) = CONV rs38l_fnam( tr_object-obj_name ).
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
                       subobjects       = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object        = tr_object
                    main_object_name = CONV #( func_group_name )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-function_group ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_result.
    objects = VALUE #( BASE objects
                       ( type       = main_object_type
                         name       = main_object_name
                         subobjects = COND #( WHEN tr_object-obj_name IS NOT INITIAL
                                              THEN VALUE zif_adcoset_ty_global=>ty_tadir_object_infos(
                                                             ( type = tr_object-obj_type
                                                               name = tr_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD add_result_cl_definition.
    objects = VALUE #(
        BASE objects
        ( type       = main_object_type
          name       = main_object_name
          subobjects = VALUE #(
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                name = cl_oo_classname_service=>get_pubsec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) )
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                name = cl_oo_classname_service=>get_prisec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) )
              ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                name = cl_oo_classname_service=>get_prosec_name(
                           CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD add_subobject.
    DATA(main_object) = REF #( objects[ type = main_object_type
                                        name = main_object_name ] ).
    " if main object already exists as a complete object, sub objects are not needed anymore because the whole object will be
    " used for the search
    IF main_object->complete_main_object = abap_false.
      LOOP AT subobjects ASSIGNING FIELD-SYMBOL(<subobject>).
        INSERT <subobject> INTO TABLE main_object->subobjects.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD handle_report_source_code.
    DATA main_obj TYPE tadir.

    " todo - test deleted function include
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING wi_e071  = VALUE e071( trkorr   = tr_object-trkorr
                                       pgmid    = tr_object-pgmid
                                       object   = tr_object-obj_type
                                       obj_name = tr_object-obj_name )
      IMPORTING we_tadir = main_obj.

    " REPS object can come from different main types, if the determined main type is not needed in the search, object can be ignored
    IF         filter_object_types IS NOT INITIAL
       AND NOT line_exists( filter_object_types[ low = main_obj-object ] ).
      RETURN.
    ENDIF.

    TRY.
        add_subobject( main_object_name = main_obj-obj_name
                       main_object_type = main_obj-object
                       subobjects       = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object        = tr_object
                    main_object_name = main_obj-obj_name
                    main_object_type = main_obj-object ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_method.
    TRY.
        add_subobject( main_object_name = CONV #( tr_object-obj_name(30) ) " class_name.
                       main_object_type = zif_adcoset_c_global=>c_source_code_type-class
                       subobjects       = VALUE #( ( name = tr_object-obj_name+30(30) type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object        = VALUE #( trkorr   = tr_object-trkorr
                                                pgmid    = tr_object-pgmid
                                                obj_type = tr_object-obj_type
                                                obj_name = tr_object-obj_name+30(30) )
                    main_object_name = CONV #( tr_object-obj_name(30) ) " class_name.
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_include.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object        = tr_object
                    main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_section.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #( ( name = section_include
                                          type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object        = VALUE #( obj_name = section_include
                                                obj_type = tr_object-obj_type )
                    main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
                    main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_private_section.
    handle_class_section(
        tr_object       = tr_object
        section_include = cl_oo_classname_service=>get_prisec_name(
                              CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_protected_section.
    handle_class_section(
        tr_object       = tr_object
        section_include = cl_oo_classname_service=>get_prosec_name(
                              CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_public_section.
    handle_class_section(
        tr_object       = tr_object
        section_include = cl_oo_classname_service=>get_pubsec_name(
                              CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD handle_class_definition.
    TRY.
        add_subobject(
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class
            subobjects       = VALUE #(
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_public_section
                  name = cl_oo_classname_service=>get_pubsec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) )
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_private_section
                  name = cl_oo_classname_service=>get_prisec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) )
                ( type = zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section
                  name = cl_oo_classname_service=>get_prosec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) ) ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result_cl_definition(
            tr_object        = tr_object
            main_object_name = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type = zif_adcoset_c_global=>c_source_code_type-class ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
