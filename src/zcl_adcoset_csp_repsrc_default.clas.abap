"! <p class="shorttext synchronized" lang="en">Default search provider for repository source code search</p>
CLASS zcl_adcoset_csp_repsrc_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of a default search provider</p>
      constructor
        IMPORTING
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings,
      matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    METHODS:
      get_include
        IMPORTING
          object        TYPE zif_adcoset_ty_global=>ty_tadir_object
        RETURNING
          VALUE(result) TYPE progname.
ENDCLASS.



CLASS zcl_adcoset_csp_repsrc_default IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->matchers = matchers.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.
    DATA(include_name) = get_include( object ).

    IF include_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(source) = src_code_reader->get_source_code(
          name = include_name
          type = object-type ).

        LOOP AT source->find_matches(
            matchers             = matchers
            match_all            = search_settings-match_all_patterns
            ignore_comment_lines = search_settings-ignore_comment_lines ) ASSIGNING FIELD-SYMBOL(<match>).

          <match>-include = include_name.
          result = VALUE #( BASE result ( <match> ) ).
        ENDLOOP.

      CATCH zcx_adcoset_src_code_read.
        "handle exception
    ENDTRY.
  ENDMETHOD.



  METHOD get_include.
    DATA: include_suffix  TYPE string.

    CASE object-type.

      WHEN zif_adcoset_c_global=>c_source_code_type-interface.
        include_suffix = 'IU'.

      WHEN zif_adcoset_c_global=>c_source_code_type-behavior_definition.
        include_suffix = 'BD'.

      WHEN zif_adcoset_c_global=>c_source_code_type-simple_transformation.
        include_suffix = 'XT'.

      WHEN zif_adcoset_c_global=>c_source_code_type-type_group.
        " TODO: Move into custom code search provider?
        SELECT SINGLE program_name
          FROM ris_v_prog_tadir
          WHERE object_name = @object-name
            AND object_type = @object-type
          INTO @result.
        RETURN.

      WHEN zif_adcoset_c_global=>c_source_code_type-program.
        result = object-name.
        RETURN.

    ENDCASE.

    IF result IS INITIAL AND include_suffix IS NOT INITIAL.
      result = object-name.
      TRANSLATE result(30) USING ' ='.
      result+30 = include_suffix.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
