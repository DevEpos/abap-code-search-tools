"! <p class="shorttext synchronized">Resource for Transport Request Type VH</p>
CLASS zcl_adcoset_adt_res_trtype_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_adt_res_trtype_vh IMPLEMENTATION.
  METHOD get_named_items.
    p_named_item_list-items            = VALUE #(
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-workbench_request description = 'Workbench Request' )
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-dev_corr_task description = 'Development/Correction Task' )
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-repair_task description = 'Repair Task' )
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-transport_of_copies description = 'Transport of Copies' )
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-piece_list description = 'Piece List' )
        ( name = zif_adcoset_c_global=>c_trkorr_type_vh-relocation_request description = 'Relocation Request' ) ).
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).

    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
