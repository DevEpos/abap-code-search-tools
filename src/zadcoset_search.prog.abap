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

TABLES sscrfields.

CLASS lcl_report DEFINITION DEFERRED.

DATA pattern_var TYPE text255.

DATA: BEGIN OF scope_vars,
        object_name TYPE sobj_name,
        object_type TYPE trobjtype,
        package     TYPE devclass,
        appl_comp   TYPE df14l-ps_posid,
        created_on  TYPE tadir-created_on,
        owner       TYPE tadir-author,
      END OF scope_vars.

SELECTION-SCREEN BEGIN OF BLOCK pattern WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS s_patt FOR pattern_var NO INTERVALS.
  PARAMETERS:
    p_ignc  TYPE abap_bool AS CHECKBOX DEFAULT 'X',
    p_regex TYPE abap_bool AS CHECKBOX USER-COMMAND regex.
SELECTION-SCREEN END OF BLOCK pattern.

SELECTION-SCREEN BEGIN OF BLOCK scope WITH FRAME TITLE TEXT-b02.
  SELECT-OPTIONS:
    s_objn FOR scope_vars-object_name,
    s_auth FOR scope_vars-owner,
    s_crtd FOR scope_vars-created_on,
    s_pack FOR scope_vars-package,
    s_appl FOR scope_vars-appl_comp.
  PARAMETERS:
    p_typal TYPE abap_bool RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND obj_type_sel,
    p_typsp TYPE abap_bool RADIOBUTTON GROUP rb1.

  SELECTION-SCREEN BEGIN OF BLOCK types WITH FRAME TITLE TEXT-b05.
    SELECTION-SCREEN PUSHBUTTON /1(6) pb_tsela USER-COMMAND all_types MODIF ID tch VISIBLE LENGTH 2.
    SELECTION-SCREEN PUSHBUTTON 5(6) pb_tseln USER-COMMAND no_types MODIF ID tch VISIBLE LENGTH 2.
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

SELECTION-SCREEN END OF BLOCK scope.

SELECTION-SCREEN BEGIN OF BLOCK limitations WITH FRAME TITLE TEXT-b07.
  PARAMETERS p_maxo TYPE n LENGTH 6 DEFAULT 5000.
SELECTION-SCREEN END OF BLOCK limitations.

SELECTION-SCREEN BEGIN OF BLOCK settings WITH FRAME TITLE TEXT-b03.
  PARAMETERS:
    p_igncom TYPE abap_bool AS CHECKBOX,
    p_matcha TYPE abap_bool AS CHECKBOX,
    p_singpm TYPE abap_bool AS CHECKBOX USER-COMMAND single_pattern_mode,
    p_multil TYPE abap_bool AS CHECKBOX USER-COMMAND multiline,
    p_seqma  TYPE abap_bool AS CHECKBOX USER-COMMAND sequential_matching.
SELECTION-SCREEN END OF BLOCK settings.

SELECTION-SCREEN BEGIN OF BLOCK parallel_processing WITH FRAME TITLE TEXT-b04.
  PARAMETERS:
    p_parlp TYPE abap_bool AS CHECKBOX,
    p_servg TYPE rzlli_apcl.
SELECTION-SCREEN END OF BLOCK parallel_processing.

SELECTION-SCREEN BEGIN OF BLOCK additional_settings WITH FRAME TITLE TEXT-b06.
  PARAMETERS p_adtn TYPE abap_bool AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK additional_settings.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS execute.
    METHODS pbo.
    METHODS pai.

  PRIVATE SECTION.
    TYPES  BEGIN OF ty_search_match.
    TYPES:   object_name  TYPE sobj_name,
             object_type  TYPE trobjtype,
             owner        TYPE responsibl,
             package_name TYPE devclass.
             INCLUDE TYPE zif_adcoset_ty_global=>ty_search_match.
    TYPES  END OF ty_search_match.

    DATA results TYPE TABLE OF ty_search_match.
    DATA type_check_refs TYPE TABLE OF REF TO abap_bool.
    DATA search_count TYPE i.
    DATA duration TYPE string.
    DATA pcre_available TYPE abap_bool.

    METHODS run_search
      RAISING
        zcx_adcoset_static_error.

    METHODS display_results.

    METHODS get_object_types
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_scope-object_type_range
      RAISING
        zcx_adcoset_static_error.

    METHODS get_patterns
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_patterns
      RAISING
        zcx_adcoset_static_error.

    METHODS set_icon
      IMPORTING
        icon_name TYPE any
      EXPORTING
        !target   TYPE any.

    METHODS set_type_check_state
      IMPORTING
        checked TYPE abap_bool.

    METHODS on_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !column
        !row.

    METHODS navigate_to_adt
      IMPORTING
        !match TYPE ty_search_match.

    METHODS validate_params
      RAISING
        zcx_adcoset_static_error.
ENDCLASS.

INITIALIZATION.
  IF NOT zcl_adcoset_db_support_util=>is_db_supported( ).
    MESSAGE |DB { sy-dbsys } is not supported by the Code Search| TYPE 'A'.
  ENDIF.
  DATA(report) = NEW lcl_report( ).

START-OF-SELECTION.
  report->execute( ).

AT SELECTION-SCREEN OUTPUT.
  report->pbo( ).

AT SELECTION-SCREEN.
  report->pai( ).

CLASS lcl_report IMPLEMENTATION.
  METHOD constructor.
    pcre_available = zcl_adcoset_pcre_util=>is_pcre_supported( ).
    set_icon( EXPORTING icon_name = 'ICON_SELECT_ALL'
              IMPORTING target    = pb_tsela ).

    set_icon( EXPORTING icon_name = 'ICON_DESELECT_ALL'
              IMPORTING target    = pb_tseln ).

    type_check_refs = VALUE #( ( REF #( p_class ) )
                               ( REF #( p_intf  ) )
                               ( REF #( p_xslt  ) )
                               ( REF #( p_prog  ) )
                               ( REF #( p_fugr  ) )
                               ( REF #( p_type  ) )
                               ( REF #( p_ddls  ) )
                               ( REF #( p_dcls  ) )
                               ( REF #( p_ddlx  ) )
                               ( REF #( p_bdef  ) ) ).
  ENDMETHOD.

  METHOD pbo.
    LOOP AT SCREEN.
      IF screen-group1 = 'TCH'.
        screen-input = COND #( WHEN p_typal = abap_true THEN '0' ELSE '1' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'S_PATT-LOW'.
        screen-required = '2'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_PCRE'.
        screen-input = COND #(
          WHEN p_singpm       = abap_false
           AND pcre_available = abap_true
          THEN '1'
          ELSE '0' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_REGEX'.
        screen-input = COND #( WHEN p_singpm = abap_false THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_SINGPM'.
        screen-input = COND #( WHEN p_regex = abap_false
                                AND p_seqma = abap_false
                               THEN '1'
                               ELSE '0' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_MULTIL'.
        screen-input = COND #( WHEN p_singpm = abap_true
                                 OR p_seqma  = abap_true
                               THEN '0'
                               ELSE '1' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_MATCHA'.
        screen-input = COND #( WHEN p_singpm = abap_true
                                 OR p_seqma  = abap_true
                               THEN '0'
                               ELSE '1' ).
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_SEQMA'.
        screen-input = COND #( WHEN p_singpm = abap_true
                                 OR p_multil = abap_true
                               THEN '0'
                               ELSE '1' ).
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    CASE sscrfields-ucomm.

      WHEN 'ALL_TYPES'.
        set_type_check_state( abap_true ).

      WHEN 'NO_TYPES'.
        set_type_check_state( abap_false ).

      WHEN 'REGEX'.
        IF p_regex = abap_true.
          p_singpm = abap_false.
        ENDIF.

      WHEN 'SEQUENTIAL_MATCHING'.
        IF p_seqma = abap_true.
          p_singpm = abap_false.
          p_matcha = abap_true.
          p_multil = abap_false.
        ENDIF.

      WHEN 'SINGLE_PATTERN_MODE'.
        IF p_singpm = abap_true.
          p_multil = abap_true.
          p_matcha = abap_false.
          p_seqma = abap_false.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD execute.
    TRY.
        validate_params( ).
        run_search( ).

        IF results IS INITIAL.
          MESSAGE |No matches found, Searched Objects: { search_count NUMBER = USER }, Duration: { duration }| TYPE 'S'.
        ELSE.
          display_results( ).
        ENDIF.
      CATCH zcx_adcoset_static_error INTO DATA(error).
        MESSAGE error->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD display_results.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv)
                                CHANGING  t_table      = results ).

        DATA(columns) = salv->get_columns( ).
        columns->set_optimize( ).

        DATA(columns_table) = columns->get( ).

        columns->set_column_position( columnname = 'SNIPPET'
                                      position   = 1 ).

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
              CAST cl_salv_column_table( <col>-r_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).

          ENDCASE.
        ENDLOOP.

        salv->get_functions( )->set_default( ).
        salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        salv->get_display_settings( )->set_list_header(
            |Search Results { lines( results ) NUMBER = USER }, Searched Objects: { search_count NUMBER = USER }, Duration: { duration }| ).

        SET HANDLER on_link_click FOR salv->get_event( ).

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
        sequential_matching  = p_seqma
        ignore_case          = p_ignc
        patterns             = get_patterns( )
        parallel_processing  = VALUE #( enabled = p_parlp server_group = p_servg )
        custom_settings      = VALUE #( class = VALUE #( include_flags = VALUE #( public_section    = abap_true
                                                                                  protected_section = abap_true
                                                                                  private_section   = abap_true
                                                                                  methods           = abap_true
                                                                                  main              = abap_true
                                                                                  test              = abap_true
                                                                                  macro             = abap_true
                                                                                  local_def         = abap_true
                                                                                  local_impl        = abap_true ) )
                                        fugr  = VALUE #( include_flags = VALUE #( function     = abap_true
                                                                                  non_function = abap_true ) ) )
        search_scope         = VALUE #( object_name_range = s_objn[]
                                        object_type_range = get_object_types( )
                                        created_on_range  = s_crtd[]
                                        owner_range       = s_auth[]
                                        package_range     = s_pack[]
                                        max_objects       = p_maxo ) ).

    IF p_regex = abap_true.
      IF zcl_adcoset_pcre_util=>is_pcre_supported( ).
        search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSE.
      search_config-matcher_type = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

    DATA(search_result) = zcl_adcoset_search_engine=>get_instance( )->search_code( search_config ).

    LOOP AT search_result-results ASSIGNING FIELD-SYMBOL(<result_object>).
      DATA(flat_match) = CORRESPONDING ty_search_match( <result_object>-object
        MAPPING object_name = name
                object_type = type ).

      LOOP AT <result_object>-text_matches ASSIGNING FIELD-SYMBOL(<text_match>).
        flat_match = CORRESPONDING #( BASE ( flat_match ) <text_match> ).
        results = VALUE #( BASE results ( flat_match ) ).
      ENDLOOP.

    ENDLOOP.

    duration = |{ CONV zif_adcoset_ty_global=>ty_duration_in_s(
      search_result-duration_in_ms / 1000 ) NUMBER = USER DECIMALS = 2 } s|.

    search_count = search_result-searched_objects_count.
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
      result = VALUE #(
          BASE result
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
      " TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)
      MESSAGE e001(00) WITH 'You have to select at least one object type' INTO DATA(msg).
      SET CURSOR FIELD p_class.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDIF.
  ENDMETHOD.

  METHOD get_patterns.
    IF p_singpm = abap_true.

      DATA text_table TYPE TABLE OF string.
      text_table = VALUE #( FOR pattern IN s_patt[] ( CONV #( pattern-low ) ) ).
      DATA(single_pattern) = concat_lines_of( table = text_table sep = |\r\n| ).
      IF single_pattern IS NOT INITIAL.
        result = VALUE #( ( content = single_pattern ) ).
      ENDIF.
    ELSE.
      result = VALUE #( FOR pattern IN s_patt[] ( content = pattern-low ) ).
    ENDIF.

    IF p_seqma = abap_true.
      result = zcl_adcoset_pattern_util=>parse_pattern_sequence( result ).
    ENDIF.
  ENDMETHOD.

  METHOD set_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING  name       = icon_name
                 text       = ''
                 add_stdinf = space
      IMPORTING  result     = target
      EXCEPTIONS OTHERS     = 1 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD set_type_check_state.
    LOOP AT type_check_refs INTO DATA(type_check).
      type_check->* = checked.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_link_click.
    ASSIGN results[ row ] TO FIELD-SYMBOL(<selected_row>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF p_adtn = abap_true.
      navigate_to_adt( match = <selected_row> ).
      RETURN.
    ENDIF.

    CASE <selected_row>-object_type.

      WHEN zif_adcoset_c_global=>c_source_code_type-class OR
           zif_adcoset_c_global=>c_source_code_type-interface OR
           zif_adcoset_c_global=>c_source_code_type-function_group OR
           zif_adcoset_c_global=>c_source_code_type-program OR
           zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
           zif_adcoset_c_global=>c_source_code_type-type_group.

        CALL FUNCTION 'EDITOR_PROGRAM'
          EXPORTING  appid   = 'PG'
                     display = abap_true
                     program = <selected_row>-include
                     line    = <selected_row>-start_line
                     topline = <selected_row>-start_line
          EXCEPTIONS OTHERS  = 0.

      WHEN zif_adcoset_c_global=>c_source_code_type-data_definition OR
           zif_adcoset_c_global=>c_source_code_type-access_control OR
           zif_adcoset_c_global=>c_source_code_type-metadata_extension OR
           zif_adcoset_c_global=>c_source_code_type-behavior_definition.

        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING  operation           = 'SHOW'
                     object_name         = <selected_row>-object_name
                     object_type         = <selected_row>-object_type
                     include             = <selected_row>-include
                     position            = <selected_row>-start_line
          EXCEPTIONS not_executed        = 1
                     invalid_object_type = 2
                     OTHERS              = 3.

    ENDCASE.
  ENDMETHOD.

  METHOD navigate_to_adt.
    DATA adt_obj TYPE sadt_object_reference.

    DATA(adt_obj_factory) = zcl_adcoset_adt_obj_factory=>get_instance( ).

    TRY.
        CASE match-object_type.

          WHEN zif_adcoset_c_global=>c_source_code_type-class OR
               zif_adcoset_c_global=>c_source_code_type-function_group.

            adt_obj = adt_obj_factory->get_object_ref_for_include( main_program      = match-object_name
                                                                   include           = match-include
                                                                   start_line        = match-start_line
                                                                   start_line_offset = match-start_column
                                                                   end_line          = match-end_line
                                                                   end_line_offset   = match-end_column ).

          WHEN zif_adcoset_c_global=>c_source_code_type-interface OR
               zif_adcoset_c_global=>c_source_code_type-access_control OR
               zif_adcoset_c_global=>c_source_code_type-behavior_definition OR
               zif_adcoset_c_global=>c_source_code_type-data_definition OR
               zif_adcoset_c_global=>c_source_code_type-type_group OR
               zif_adcoset_c_global=>c_source_code_type-metadata_extension OR
               zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
               zif_adcoset_c_global=>c_source_code_type-program.

            adt_obj = adt_obj_factory->get_object_ref_for_trobj( type                   = match-object_type
                                                                 name                   = match-object_name
                                                                 append_source_uri_path = abap_true ).

            adt_obj_factory->add_position_fragment( EXPORTING start_line   = match-start_line
                                                              start_column = match-start_column
                                                              end_line     = match-end_line
                                                              end_column   = match-end_column
                                                    CHANGING  link         = adt_obj-uri ).
        ENDCASE.

        IF adt_obj-uri IS NOT INITIAL.
          cl_gui_frontend_services=>execute( EXPORTING  document = |adt://{ sy-sysid }{ adt_obj-uri }|
                                             EXCEPTIONS OTHERS   = 1 ).
          IF sy-subrc <> 0.
            MESSAGE 'ADT Navigation error' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      CATCH zcx_adcoset_static_error.
        MESSAGE 'ADT Navigation error' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_params.
    IF s_patt-low IS INITIAL.
      SET CURSOR FIELD s_patt-low.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = 'You have to provide at least 1 pattern'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
