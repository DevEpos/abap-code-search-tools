"! <p class="shorttext synchronized" lang="en">Search provider for Function Groups</p>
CLASS zcl_adcoset_csp_fugr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of a function group search provider</p>
      constructor
        IMPORTING
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_is_function_include_pattern TYPE string VALUE `^(/\w{1,10}/)?L.*U\d{2}$`.

    DATA:
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings,
      matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    TYPES:
      BEGIN OF ty_fugr_incl,
        name                TYPE progname,
        is_function_include TYPE abap_bool,
        func_name           TYPE rs38l_fnam,
      END OF ty_fugr_incl,
      ty_fugr_includes TYPE SORTED TABLE OF ty_fugr_incl WITH UNIQUE KEY name.

    METHODS:
      get_fugr_includes
        IMPORTING
          fugr_program_name TYPE progname
        RETURNING
          VALUE(result)     TYPE ty_fugr_includes,
      assign_objects_to_matches
        IMPORTING
          unassigned_matches TYPE zif_adcoset_ty_global=>ty_search_matches
          object             TYPE zif_adcoset_ty_global=>ty_tadir_object
          include            TYPE ty_fugr_incl
        CHANGING
          all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches,
      get_function_includes
        IMPORTING
          fugr_program_name TYPE progname
        RETURNING
          VALUE(result)     TYPE zcl_adcoset_csp_fugr=>ty_fugr_includes,
      get_fugr_include_name
        IMPORTING
          fugr_name     TYPE rs38l_area
        RETURNING
          VALUE(result) TYPE progname,
      get_all_includes
        IMPORTING
          fugr_program_name TYPE progname
        RETURNING
          VALUE(result)     TYPE ty_fugr_includes,
      mixin_function_names
        IMPORTING
          fugr_program_name TYPE progname
        CHANGING
          includes          TYPE ty_fugr_includes.
ENDCLASS.



CLASS zcl_adcoset_csp_fugr IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->matchers = matchers.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.
    DATA: function_names_loaded TYPE abap_bool.

    DATA(fugr_name) = CONV rs38l_area( object-name ).
    DATA(fugr_program_name) = get_fugr_include_name( fugr_name ).
    IF fugr_program_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(includes) = get_fugr_includes( fugr_program_name ).
    IF includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include>).
      TRY.
          DATA(source_code) = src_code_reader->get_source_code( name = <include>-name ).
          DATA(matches) = source_code->find_matches(
            matchers             = matchers
            match_all            = search_settings-match_all_patterns
            ignore_comment_lines = search_settings-ignore_comment_lines ).

          CHECK matches IS NOT INITIAL.

          IF <include>-is_function_include = abap_true AND function_names_loaded = abap_false.
            mixin_function_names(
              EXPORTING
                fugr_program_name = fugr_program_name
              CHANGING
                includes          = includes ).
            function_names_loaded = abap_true.
          ENDIF.

          assign_objects_to_matches(
            EXPORTING
              unassigned_matches = matches
              object             = object
              include            = <include>
            CHANGING
              all_matches        = result ).
        CATCH zcx_adcoset_src_code_read ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_fugr_includes.

    LOOP AT get_all_includes( fugr_program_name ) ASSIGNING FIELD-SYMBOL(<include>).
      IF matches( val = <include>-name regex = c_is_function_include_pattern ).
        <include>-is_function_include = abap_true.
      ENDIF.
      result = VALUE #( BASE result ( <include> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD assign_objects_to_matches.

    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
      APPEND <match_without_source> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-include = include-name.

      " set the display name
      IF include-func_name IS NOT INITIAL.
        <match>-display_name = include-func_name.
      ELSE.
        <match>-display_name = include-name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_function_includes.
    DATA: namespace TYPE namespace,
          fugr_name TYPE rs38l_area.

    "get_function_includes
    SELECT funcname AS func_name,
           include
      FROM tfdir
      WHERE pname = @fugr_program_name
      INTO TABLE @DATA(function_includes).

    LOOP AT function_includes ASSIGNING FIELD-SYMBOL(<function_include>).
      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
          program   = fugr_program_name
        IMPORTING
          namespace = namespace
          group     = fugr_name
        EXCEPTIONS ##FM_SUBRC_OK
          OTHERS    = 6.

      IF sy-subrc = 0.
        result = VALUE #( BASE result
          ( name      = |{ namespace }L{ fugr_name }U{ <function_include>-include }|
            func_name = <function_include>-func_name ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_fugr_include_name.
    DATA(group_name) = fugr_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      IMPORTING
        pname  = result
      CHANGING
        group  = group_name
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.


  METHOD get_all_includes.
    DATA includes TYPE TABLE OF sobj_name.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = fugr_program_name
      TABLES
        includetab   = includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.

    result = VALUE #( FOR incl IN includes ( name = incl ) ).
  ENDMETHOD.


  METHOD mixin_function_names.
    DATA(function_includes) = get_function_includes( fugr_program_name ).
    IF function_includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include>).
      IF <include>-is_function_include = abap_true.
        <include>-func_name = VALUE #( function_includes[ name = <include>-name ]-func_name OPTIONAL ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
