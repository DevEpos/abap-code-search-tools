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
    DATA:
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings,
      matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    TYPES:
      BEGIN OF ty_fugr_incl,
        name      TYPE ris_v_prog_tadir-program_name,
        func_name TYPE ris_v_prog_tadir-func_name,
      END OF ty_fugr_incl,
      ty_fugr_includes TYPE STANDARD TABLE OF ty_fugr_incl WITH KEY name.

    METHODS:
      get_fugr_includes
        IMPORTING
          name          TYPE sobj_name
        RETURNING
          VALUE(result) TYPE ty_fugr_includes,
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
        CHANGING
          includes          TYPE zcl_adcoset_csp_fugr=>ty_fugr_includes,
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
      is_reserved_include
        IMPORTING
          include_name  TYPE progname
        RETURNING
          VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_csp_fugr IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->matchers = matchers.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.

    DATA(fugr_includes) = get_fugr_includes( name = object-name ).
    IF fugr_includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT fugr_includes ASSIGNING FIELD-SYMBOL(<include>).
      TRY.
          DATA(source_code) = src_code_reader->get_source_code( name = <include>-name ).
          DATA(matches) = source_code->find_matches(
            matchers             = matchers
            match_all            = search_settings-match_all_patterns
            ignore_comment_lines = search_settings-ignore_comment_lines ).

          IF matches IS NOT INITIAL.
            assign_objects_to_matches(
              EXPORTING
                unassigned_matches = matches
                object             = object
                include            = <include>
              CHANGING
                all_matches        = result ).
          ENDIF.
        CATCH zcx_adcoset_src_code_read.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_fugr_includes.

    IF sy-dbsys = 'HDB'.
      SELECT program_name AS name,
             func_name
        FROM ris_v_prog_tadir
        WHERE object_name = @name
          AND object_type = @zif_adcoset_c_global=>c_source_code_type-function_group
        INTO CORRESPONDING FIELDS OF TABLE @result.

      " remove reserved includes (e.g. LCMS_BDT$08)
      LOOP AT result ASSIGNING FIELD-SYMBOL(<include>) WHERE func_name IS INITIAL.
        IF is_reserved_include( <include>-name ).
          DELETE result.
        ENDIF.
      ENDLOOP.

    ELSE.
      DATA(fugr_name) = CONV rs38l_area( name ).
      DATA(fugr_program_name) = get_fugr_include_name( fugr_name ).
      IF fugr_program_name IS INITIAL.
        RETURN.
      ENDIF.

      get_function_includes(
        EXPORTING
          fugr_program_name = fugr_program_name
        CHANGING
          includes          = result ).

      result = value #( base result ( lines of get_all_includes( fugr_program_name ) ) ).

      " remove duplicate function includes - they are also included from RS_GET_ALL_INCLUDES
      SORT result BY name ASCENDING func_name DESCENDING.
      DELETE ADJACENT DUPLICATES FROM result COMPARING name.
    ENDIF.
  ENDMETHOD.


  METHOD assign_objects_to_matches.

    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
      APPEND <match_without_source> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).
      <match>-object_name = object-name.
      <match>-object_type = object-type.

      <match>-include = include-name.

      " set the display name
      IF include-func_name IS NOT INITIAL.
        <match>-display_name = include-func_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_function_includes.

    "get_function_includes
    SELECT funcname AS func_name
      FROM tfdir
      WHERE pname = @fugr_program_name
      INTO CORRESPONDING FIELDS OF TABLE @includes.

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<function_include>).
      CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
        CHANGING
          funcname = <function_include>-func_name
          include  = <function_include>-name
        EXCEPTIONS
          OTHERS   = 1.

      IF <function_include>-name IS INITIAL.
        DELETE includes.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_fugr_include_name.
    DATA(group_name) = fugr_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
      IMPORTING
        pname               = result
      CHANGING
        group               = group_name
      EXCEPTIONS
        function_not_exists = 1
        include_not_exists  = 2
        group_not_exists    = 3
        no_selections       = 4
        no_function_include = 5
        OTHERS              = 6.
  ENDMETHOD.


  METHOD get_all_includes.
    data includes type table of sobj_name.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = fugr_program_name
      TABLES
        includetab   = includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.

        result = value #( for incl in includes ( name = incl ) ).
  ENDMETHOD.


  METHOD is_reserved_include.
    DATA: is_reserved_name TYPE abap_bool,
          is_hidden_name   TYPE abap_bool.

    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING
        progname_with_namespace = include_name
      IMPORTING
        fugr_is_reserved_name   = is_reserved_name
        fugr_is_hidden_name     = is_hidden_name
      EXCEPTIONS
        delimiter_error         = 1
        OTHERS                  = 2.

    result = xsdbool( sy-subrc <> 0 OR is_reserved_name = abap_true OR is_hidden_name = abap_true ).
  ENDMETHOD.

ENDCLASS.
