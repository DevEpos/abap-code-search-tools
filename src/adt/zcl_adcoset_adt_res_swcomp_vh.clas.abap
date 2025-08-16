"! Resource for VH of Software Component
CLASS zcl_adcoset_adt_res_swcomp_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_adt_res_swcomp_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_sw_comps TYPE trdlvunits.

    CALL FUNCTION 'TR_GET_DLVUNITS'
      EXPORTING iv_with_description = abap_true
      IMPORTING et_dlvunits         = lt_sw_comps.

    p_filter_already_applied = abap_false.
    p_named_item_list-items            = VALUE #( FOR sw_comp IN lt_sw_comps
                                                  ( name = sw_comp-dlvunit description = sw_comp-desc_text ) ).
    p_named_item_list-total_item_count = lines( lt_sw_comps ).
  ENDMETHOD.
ENDCLASS.
