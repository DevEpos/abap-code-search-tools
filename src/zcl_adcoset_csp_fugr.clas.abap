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
          all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches.
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
          DATA(matches) = source_code->find_matches( matchers ).

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
    DATA: is_reserved_name TYPE abap_bool,
          is_hidden_name   TYPE abap_bool.

    SELECT program_name AS name,
           func_name
      FROM ris_v_prog_tadir
      WHERE object_name = @name
        AND object_type = @zif_adcoset_c_global=>c_source_code_type-function_group
      INTO CORRESPONDING FIELDS OF TABLE @result.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<include>) WHERE func_name IS INITIAL.
      CALL FUNCTION 'RS_PROGNAME_SPLIT'
        EXPORTING
          progname_with_namespace = <include>-name
        IMPORTING
          fugr_is_reserved_name   = is_reserved_name
          fugr_is_hidden_name     = is_hidden_name
        EXCEPTIONS
          delimiter_error         = 1
          OTHERS                  = 2.
      IF sy-subrc <> 0 OR is_reserved_name = abap_true OR is_hidden_name = abap_true.
        DELETE result.
      ENDIF.
    ENDLOOP.

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

ENDCLASS.
