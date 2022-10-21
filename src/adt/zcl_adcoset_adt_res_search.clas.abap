"! <p class="shorttext synchronized" lang="en">Resource for code search</p>
CLASS zcl_adcoset_adt_res_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_adt_res_search IMPLEMENTATION.


  METHOD get.
    DATA(result) = NEW lcl_search_query( request )->run( ).
    response->set_body_data(
      content_handler = zcl_adcoset_adt_ch_factory=>create_search_result_ch( )
      data            = result ).
  ENDMETHOD.


ENDCLASS.
