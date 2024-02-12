"! <p class="shorttext synchronized">Default search provider for string sources</p>
CLASS zcl_adcoset_csp_strsrc_default DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    "! <p class="shorttext synchronized">Creates new instance of a default search provider</p>
    METHODS constructor.

  PRIVATE SECTION.
    DATA search_settings TYPE zif_adcoset_ty_global=>ty_search_settings.
ENDCLASS.


CLASS zcl_adcoset_csp_strsrc_default IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD zif_adcoset_code_search_prov~search.
    TRY.
        DATA(source) = src_code_reader->get_source_code( object-name ).
        result = src_code_searcher->search( source ).
      CATCH zcx_adcoset_src_code_read.
        " handle exception
    ENDTRY.

    zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
  ENDMETHOD.
ENDCLASS.
