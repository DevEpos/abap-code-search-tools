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
    DATA fix_values TYPE dd07v_tab.

    CALL FUNCTION 'DDUT_DOMVALUES_GET'
      EXPORTING  name          = 'TRFUNCTION'
                 langu         = 'E'
      TABLES     dd07v_tab     = fix_values
      EXCEPTIONS illegal_input = 1
                 OTHERS        = 2.
    IF sy-subrc = 0.
      " Customizing Tasks/Requests do not contain any searchable objects
      DELETE fix_values WHERE domvalue_l = 'W' OR domvalue_l = 'Q'.
      p_named_item_list-items            = VALUE #( FOR fix_val IN fix_values
                                                    ( name = fix_val-domvalue_l description = fix_val-ddtext ) ).
      p_named_item_list-total_item_count = lines( fix_values ).

    ENDIF.

    p_filter_already_applied = abap_true.
  ENDMETHOD.
ENDCLASS.
