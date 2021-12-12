*&---------------------------------------------------------------------*
*& ABAP Code search:
*&  Searches source code in the following repository types:
*&  - Classes
*&  - Interfaces
*&  - Programs (Reports / Includes)
*&  - Function Groups
*&  - Type Groups
*&  - Simple Transformations
*&  - Data Definitions
*&  - Access Controls
*&  - Metadata Extensions
*&  - Behavior Definitions
*&---------------------------------------------------------------------*
REPORT zadcoset_search.

CLASS lcl_report DEFINITION DEFERRED.

DATA: pattern_var TYPE text255.

DATA: BEGIN OF scope_vars,
        object_name TYPE sobj_name,
        object_type TYPE trobjtype,
        package     TYPE devclass,
        appl_comp   TYPE df14l-ps_posid,
        created_on  TYPE tadir-created_on,
        owner       TYPE tadir-author,
      END OF scope_vars.

SELECTION-SCREEN BEGIN OF BLOCK pattern WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS: s_patt FOR pattern_var NO INTERVALS.
  PARAMETERS:
    p_ignc  TYPE abap_bool AS CHECKBOX DEFAULT 'X',
    p_regex TYPE abap_bool AS CHECKBOX,
    p_pcre  TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK pattern.

SELECTION-SCREEN BEGIN OF BLOCK scope WITH FRAME TITLE TEXT-b02.
  SELECT-OPTIONS:
    s_objn FOR scope_vars-object_name.

  PARAMETERS:
    p_typal TYPE abap_bool RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND obj_type_sel,
    p_typsp TYPE abap_bool RADIOBUTTON GROUP rb1.

  SELECTION-SCREEN BEGIN OF BLOCK types WITH FRAME TITLE TEXT-b05.
    PARAMETERS:
      p_class TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_intf  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_xslt  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_prog  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_fugr  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_type  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_ddls  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_dcls  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_ddlx  TYPE abap_bool AS CHECKBOX MODIF ID tch,
      p_bdef  TYPE abap_bool AS CHECKBOX MODIF ID tch.
  SELECTION-SCREEN END OF BLOCK types.

  SELECT-OPTIONS:
    s_auth FOR scope_vars-owner,
    s_crtd FOR scope_vars-created_on,
    s_pack FOR scope_vars-package,
    s_appl FOR scope_vars-appl_comp.
  PARAMETERS: p_maxo TYPE n LENGTH 5 DEFAULT 500 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK scope.

SELECTION-SCREEN BEGIN OF BLOCK settings WITH FRAME TITLE TEXT-b03.
  PARAMETERS:
    p_igncom TYPE abap_bool AS CHECKBOX,
    p_multil TYPE abap_bool AS CHECKBOX,
    p_matcha TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK settings.

SELECTION-SCREEN BEGIN OF BLOCK parallel_processing WITH FRAME TITLE TEXT-b04.
  PARAMETERS:
    p_parlp TYPE abap_bool AS CHECKBOX,
    p_servg TYPE rzlli_apcl.
SELECTION-SCREEN END OF BLOCK parallel_processing.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS:
      execute,
      pbo.
  PRIVATE SECTION.
    DATA:
      results TYPE zif_adcoset_ty_global=>ty_search_matches.
    METHODS:
      run_search
        RAISING
          zcx_adcoset_static_error,
      display_results,
      get_object_types
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_scope-object_type_range
        RAISING
          zcx_adcoset_static_error.
ENDCLASS.

INITIALIZATION.
  DATA(report) = NEW lcl_report( ).


START-OF-SELECTION.
  report->execute( ).

AT SELECTION-SCREEN OUTPUT.
  report->pbo( ).

CLASS lcl_report IMPLEMENTATION.

  METHOD pbo.
    DATA(type_checks_enabled) = COND #( WHEN p_typal = abap_true THEN '0' ELSE '1' ).

    LOOP AT SCREEN.
      IF screen-group1 = 'TCH'.
        screen-input = type_checks_enabled.
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_IGNCOM' OR
             screen-name = 'P_MATCHA'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD execute.
    TRY.
        run_search( ).

        IF results IS INITIAL.
          MESSAGE 'No matches found' TYPE 'S'.
        ELSE.
          display_results( ).
        ENDIF.
      CATCH zcx_adcoset_static_error INTO DATA(error).
        MESSAGE error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD display_results.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(salv)
          CHANGING
            t_table      = results ).

        DATA(columns) = salv->get_columns( ).
        columns->set_optimize( ).

        DATA(columns_table) = columns->get( ).

        LOOP AT columns_table ASSIGNING FIELD-SYMBOL(<col>).

          CASE <col>-columnname.

            WHEN 'DISPLAY_NAME'.
              <col>-r_column->set_long_text( 'Method / Function / Section' ).

            WHEN 'MAIN_INCLUDE'.
              <col>-r_column->set_technical( ).

            WHEN 'START_LINE'.
              <col>-r_column->set_medium_text( 'Start Line' ).

            WHEN 'START_COLUMN'.
              <col>-r_column->set_medium_text( 'Start Column' ).

            WHEN 'END_LINE'.
              <col>-r_column->set_medium_text( 'End Line' ).

            WHEN 'END_COLUMN'.
              <col>-r_column->set_medium_text( 'End Column' ).

            WHEN 'SNIPPET'.
              <col>-r_column->set_medium_text( 'Snippet' ).
              <col>-r_column->set_output_length( 100 ).

          ENDCASE.
        ENDLOOP.

        salv->get_functions( )->set_default( ).
        salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        salv->get_display_settings( )->set_list_header( |Search Results { lines( results ) NUMBER = USER }| ).
        salv->display( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.


  METHOD run_search.
    DATA(search_config) = VALUE zif_adcoset_ty_global=>ty_search_settings_external(
      line_feed            = |\r\n|
      ignore_comment_lines = p_igncom
      match_all_patterns   = p_matcha
      multiline_search     = p_multil
      ignore_case          = p_ignc
      pattern_range        = VALUE #( FOR pattern IN s_patt[] ( sign = pattern-sign option = 'EQ' low = pattern-low ) )
      parallel_processing  = VALUE #( enabled = p_parlp server_group = p_servg )
      search_scope         = VALUE #(
        object_name_range = s_objn[]
        object_type_range = get_object_types(  )
        created_on_range  = s_crtd[]
        owner_range       = s_auth[]
        package_range     = s_pack[]
        max_objects       = COND #( WHEN p_maxo > 10000 THEN 10000 ELSE p_maxo ) ) ).

    IF p_pcre = abap_true.
      IF zcl_adcoset_matcher_factory=>is_pcre_supported( ).
        search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSEIF p_regex = abap_true.
      search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-posix_regex.
    ELSE.
      search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

    results = zcl_adcoset_search_engine=>get_instance( )->search_code( search_config ).

  ENDMETHOD.


  METHOD get_object_types.
    IF p_typal = abap_true.
      RETURN.
    ENDIF.

    IF p_class = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-class ) ).
    ENDIF.

    IF p_intf = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-interface ) ).
    ENDIF.

    IF p_xslt = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-simple_transformation ) ).
    ENDIF.

    IF p_prog = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-program ) ).
    ENDIF.

    IF p_fugr = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-function_group ) ).
    ENDIF.

    IF p_type = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-type_group ) ).
    ENDIF.

    IF p_ddls = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-data_definition ) ).
    ENDIF.

    IF p_dcls = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-access_control ) ).
    ENDIF.

    IF p_ddlx = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-metadata_extension ) ).
    ENDIF.

    IF p_bdef = abap_true.
      result = VALUE #( BASE result
        ( sign = 'I' option = 'EQ' low = zif_adcoset_c_global=>c_source_code_type-behavior_definition ) ).
    ENDIF.

    IF result IS INITIAL.
      MESSAGE e001(00) WITH 'You have to select at least one object type' INTO DATA(msg).
      RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
