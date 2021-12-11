CLASS zcl_adcoset_csp_strsrc_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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
        DATA(matches) = source->find_matches( matchers ).

        LOOP AT matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
          APPEND <match_without_source> TO result ASSIGNING FIELD-SYMBOL(<match>).
          <match>-object_name = object-name.
          <match>-object_type = object-type.
        ENDLOOP.

      CATCH zcx_adcoset_src_code_read.
        "handle exception
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
