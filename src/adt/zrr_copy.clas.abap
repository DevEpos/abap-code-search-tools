"! <p class="shorttext synchronized">Resource for code search</p>
CLASS zrr_copy DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zrr_copy IMPLEMENTATION.
  METHOD get.
    DATA(result) = NEW lcl_search_query( request )->run( ).
    response->set_body_data( content_handler = zcl_adcoset_adt_ch_factory=>create_search_result_ch( )
                             data            = result ).
  ENDMETHOD.
ENDCLASS.
