*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_limu_processor IMPLEMENTATION.
  METHOD constructor.
    me->result = result.
  ENDMETHOD.

  METHOD handle_function_module.
    DATA func_group_name TYPE rs38l_area.

    DATA(funcname) = CONV rs38l_fnam( tr_object-obj_name ).
    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      CHANGING   funcname            = funcname
                 group               = func_group_name
      EXCEPTIONS function_not_exists = 1.

    DATA(function_exists) = xsdbool( sy-subrc = 0 ).
    TRY.
        add_subobject( main_object_name       = COND #( WHEN function_exists = abap_true THEN func_group_name )
                       main_object_type       = zif_adcoset_c_global=>c_source_code_type-function_group
                       has_deleted_subobjects = xsdbool( function_exists = abap_false )
                       subobjects             = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = tr_object
                    main_object_name       = COND #( WHEN function_exists = abap_true THEN func_group_name )
                    main_object_type       = zif_adcoset_c_global=>c_source_code_type-function_group
                    has_deleted_subobjects = xsdbool( function_exists = abap_false ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_result.
    result = VALUE #( BASE result
                      ( type                   = main_object_type
                        name                   = main_object_name
                        searched_objs_count    = 1
                        has_deleted_subobjects = has_deleted_subobjects
                        subobjects             = COND #( WHEN tr_object-obj_name IS NOT INITIAL
                                                         THEN VALUE zif_adcoset_ty_global=>ty_tadir_objects(
                                                                        ( type = tr_object-obj_type
                                                                          name = tr_object-obj_name ) ) ) ) ).
  ENDMETHOD.

  METHOD add_result_cl_definition.
    result = VALUE #(
        BASE result
        ( type                   = main_object_type
          name                   = main_object_name
          searched_objs_count    = 1
          has_deleted_subobjects = has_deleted_subobjects
          subobjects             = VALUE #(
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
    ASSIGN result[ type = main_object_type
                   name = main_object_name ] TO FIELD-SYMBOL(<main_object>).
    IF sy-subrc = 0.
      IF <main_object>-complete_main_object = abap_false.
        " main object with subobjects already exists. In case searched_objs_count = 0 the main object without subobjects
        " is already part of the transport request, subobjects are not needed anymore because the whole object will be
        " used for the search
        LOOP AT subobjects ASSIGNING FIELD-SYMBOL(<subobject>).
          IF NOT line_exists( <main_object>-subobjects[ type = <subobject>-type name = <subobject>-name ] ).
            <main_object>-subobjects = VALUE #( BASE <main_object>-subobjects ( <subobject> ) ).
          ENDIF.
        ENDLOOP.
        <main_object>-has_deleted_subobjects = has_deleted_subobjects.
      ENDIF.
      " the object count has to be increased to reach the scope even though the object is not explicitly included in the search
      <main_object>-searched_objs_count = <main_object>-searched_objs_count + 1.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD handle_report_source_code.
    DATA main_obj TYPE tadir.

    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING wi_e071  = VALUE e071( trkorr   = tr_object-trkorr
                                       pgmid    = tr_object-pgmid
                                       object   = tr_object-obj_type
                                       obj_name = tr_object-obj_name )
      IMPORTING we_tadir = main_obj.
    TRY.
        add_subobject( main_object_name       = main_obj-obj_name
                       main_object_type       = main_obj-object
                       has_deleted_subobjects = abap_false
                       subobjects             = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = tr_object
                    main_object_name       = main_obj-obj_name
                    main_object_type       = main_obj-object
                    has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_method.
    TRY.
        add_subobject(
            main_object_name       = CONV #( tr_object-obj_name(30) ) " class_name.
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #( ( name = tr_object-obj_name+30(30) type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = VALUE #( trkorr   = tr_object-trkorr
                                                      pgmid    = tr_object-pgmid
                                                      obj_type = tr_object-obj_type
                                                      obj_name = tr_object-obj_name+30(30) )
                    main_object_name       = CONV #( tr_object-obj_name(30) ) " class_name.
                    main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
                    has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_include.
    TRY.
        add_subobject(
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #( ( name = tr_object-obj_name type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result(
            tr_object              = tr_object
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_private_section.
    TRY.
        add_subobject(
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #(
                ( name = cl_oo_classname_service=>get_prisec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                  type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = tr_object
                    main_object_name       = cl_oo_classname_service=>get_clsname_by_include(
                        CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                    main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
                    has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_protected_section.
    TRY.
        add_subobject(
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #(
                ( name = cl_oo_classname_service=>get_prosec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                  type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = tr_object
                    main_object_name       = cl_oo_classname_service=>get_clsname_by_include(
                        CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                    main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
                    has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_public_section.
    TRY.
        add_subobject(
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #(
                ( name = cl_oo_classname_service=>get_pubsec_name(
                             CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                  type = tr_object-obj_type ) ) ).

      CATCH cx_sy_itab_line_not_found.
        add_result( tr_object              = tr_object
                    main_object_name       = cl_oo_classname_service=>get_clsname_by_include(
                        CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
                    main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
                    has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.

  METHOD handle_class_definition.
    TRY.
        add_subobject(
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false
            subobjects             = VALUE #(
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
            tr_object              = tr_object
            main_object_name       = cl_oo_classname_service=>get_clsname_by_include(
                CONV #( cl_oo_classname_service=>get_clsname_by_include( CONV #( tr_object-obj_name ) ) ) )
            main_object_type       = zif_adcoset_c_global=>c_source_code_type-class
            has_deleted_subobjects = abap_false ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
