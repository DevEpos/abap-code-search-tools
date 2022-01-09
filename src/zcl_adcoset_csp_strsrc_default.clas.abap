"! <p class="shorttext synchronized" lang="en">Default search provider for string sources</p>
CLASS zcl_adcoset_csp_strsrc_default DEFINITION
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
ENDCLASS.



CLASS zcl_adcoset_csp_strsrc_default IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->matchers = matchers.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.
    TRY.
        DATA(source) = src_code_reader->get_source_code( object-name ).
        result = source->find_matches(
          matchers             = matchers
          match_all            = search_settings-match_all_patterns
          ignore_comment_lines = search_settings-ignore_comment_lines ).

      CATCH zcx_adcoset_src_code_read.
        "handle exception
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
