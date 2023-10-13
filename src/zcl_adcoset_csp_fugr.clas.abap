"! <p class="shorttext synchronized">Search provider for Function Groups</p>
CLASS zcl_adcoset_csp_fugr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    "! <p class="shorttext synchronized">Creates new instance of a function group search provider</p>
    METHODS constructor
      IMPORTING
        custom_settings TYPE zif_adcoset_ty_global=>ty_fugr_cs_settings.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_include_types,
        function TYPE string VALUE 'FUGR/FF',
        include  TYPE string VALUE 'FUGR/I',
      END OF c_include_types.

    DATA custom_settings TYPE zif_adcoset_ty_global=>ty_fugr_cs_settings.
    DATA matchers TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    TYPES:
      BEGIN OF ty_fugr_incl,
        name                TYPE progname,
        is_function_include TYPE abap_bool,
        func_name           TYPE rs38l_fnam,
        adt_type            TYPE string,
      END OF ty_fugr_incl,
      ty_fugr_includes TYPE SORTED TABLE OF ty_fugr_incl WITH UNIQUE KEY name.

    METHODS get_fugr_includes
      IMPORTING
        include_prefix TYPE progname
      RETURNING
        VALUE(result)  TYPE ty_fugr_includes.

    METHODS get_fugr_include_info
      IMPORTING
        fugr_name                  TYPE rs38l_area
      EXPORTING
        VALUE(fugr_main_prog)      TYPE progname
        VALUE(fugr_include_prefix) TYPE progname.

    METHODS assign_objects_to_matches
      IMPORTING
        unassigned_matches TYPE zif_adcoset_ty_global=>ty_search_matches
        !object            TYPE zif_adcoset_ty_global=>ty_tadir_object
        !include           TYPE ty_fugr_incl
      CHANGING
        all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches.

    METHODS get_function_includes
      IMPORTING
        fugr_program_name TYPE progname
      RETURNING
        VALUE(result)     TYPE ty_fugr_includes.

    METHODS mixin_function_names
      IMPORTING
        fugr_program_name TYPE progname
      CHANGING
        !includes         TYPE ty_fugr_includes.
ENDCLASS.


CLASS zcl_adcoset_csp_fugr IMPLEMENTATION.
  METHOD constructor.
    me->custom_settings = custom_settings.
  ENDMETHOD.

  METHOD zif_adcoset_code_search_prov~search.
    DATA function_names_loaded TYPE abap_bool.

    DATA(fugr_name) = CONV rs38l_area( object-name ).
    get_fugr_include_info( EXPORTING fugr_name           = fugr_name
                           IMPORTING fugr_main_prog      = DATA(fugr_include)
                                     fugr_include_prefix = DATA(fugr_include_prefix) ).
    IF fugr_include IS INITIAL.
      RETURN.
    ENDIF.

    DATA(includes) = get_fugr_includes( fugr_include_prefix ).
    IF includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include>).
      TRY.
          DATA(source_code) = src_code_reader->get_source_code( name = <include>-name ).
          DATA(matches) = src_code_searcher->search( source_code ).
          CHECK matches IS NOT INITIAL.

          IF <include>-is_function_include = abap_true AND function_names_loaded = abap_false.
            mixin_function_names( EXPORTING fugr_program_name = fugr_include
                                  CHANGING  includes          = includes ).
            function_names_loaded = abap_true.
          ENDIF.

          assign_objects_to_matches( EXPORTING unassigned_matches = matches
                                               object             = object
                                               include            = <include>
                                     CHANGING  all_matches        = result ).
        CATCH zcx_adcoset_src_code_read ##NO_HANDLER.
      ENDTRY.

    ENDLOOP.

    zcl_adcoset_search_protocol=>increase_searchd_sources_count( lines( includes ) ).
  ENDMETHOD.

  METHOD get_fugr_includes.
    DATA is_reserved_name TYPE abap_bool.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA is_no_func_include TYPE abap_bool.
    DATA is_no_func_module TYPE abap_bool.
    DATA include_suffix TYPE c LENGTH 3.

    DATA(include_pattern) = CONV progname( |{ include_prefix }___| ).

    SELECT name
      FROM trdir
      WHERE name LIKE @include_pattern
      INTO TABLE @DATA(includes).

    IF includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include_key>).
      DATA(include_name) = <include_key>-name.
      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        IMPORTING  no_function_include = is_no_func_include
                   no_function_module  = is_no_func_module
                   reserved_name       = is_reserved_name
                   suffix              = include_suffix
        CHANGING   include             = include_name
        EXCEPTIONS OTHERS              = 1.
      IF sy-subrc <> 0.
        CONTINUE.
      ELSEIF is_reserved_name = abap_true AND ( is_no_func_module = abap_true ).
        CONTINUE.
      ENDIF.

      DATA(new_include) = VALUE ty_fugr_incl( name = <include_key>-name ).

      IF include_suffix(1) = 'U'.
        CHECK custom_settings-include_flags-function = abap_true.
        new_include-is_function_include = abap_true.
        new_include-adt_type            = c_include_types-function.
      ELSE.
        CHECK custom_settings-include_flags-non_function = abap_true.
        new_include-adt_type = c_include_types-include.
      ENDIF.

      result = VALUE #( BASE result ( new_include ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_fugr_include_info.
    DATA group_wo_namespace TYPE rs38l_area.
    DATA namespace TYPE namespace.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING  complete_area = fugr_name
      IMPORTING  namespace     = namespace
                 group         = group_wo_namespace
      CHANGING   include       = fugr_main_prog
      EXCEPTIONS OTHERS        = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    fugr_main_prog = |{ namespace }SAPL{ group_wo_namespace }|.
    fugr_include_prefix = |{ namespace }L{ group_wo_namespace }|.
  ENDMETHOD.

  METHOD assign_objects_to_matches.
    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
      APPEND <match_without_source> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-include          = include-name.
      <match>-adt_include_type = include-adt_type.

      " set the display name
      IF include-func_name IS NOT INITIAL.
        <match>-display_name = include-func_name.
      ELSE.
        <match>-display_name = include-name.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_function_includes.
    DATA namespace TYPE namespace.
    DATA fugr_name TYPE rs38l_area.

    SELECT funcname AS func_name,
           include
      FROM tfdir
      WHERE pname = @fugr_program_name
      INTO TABLE @DATA(function_includes).

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING  program   = fugr_program_name
      IMPORTING  namespace = namespace
                 group     = fugr_name
      EXCEPTIONS OTHERS    = 1.

    LOOP AT function_includes ASSIGNING FIELD-SYMBOL(<function_include>).
      INSERT VALUE #( name      = |{ namespace }L{ fugr_name }U{ <function_include>-include }|
                      func_name = <function_include>-func_name
                      adt_type  = c_include_types-function ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.

  METHOD mixin_function_names.
    DATA(function_includes) = get_function_includes( fugr_program_name ).
    IF function_includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include>).
      IF <include>-is_function_include = abap_true.
        DATA(function_include) = REF #( function_includes[ name = <include>-name ] OPTIONAL ).
        IF function_include IS NOT INITIAL.
          <include>-func_name = function_include->func_name.
          <include>-adt_type  = function_include->adt_type.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
