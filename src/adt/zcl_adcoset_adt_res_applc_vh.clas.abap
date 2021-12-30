"! <p class="shorttext synchronized" lang="en">Resource for Application Component value help</p>
CLASS zcl_adcoset_adt_res_applc_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_adt_res_applc_vh IMPLEMENTATION.


  METHOD get_named_items.
    DATA: lt_appl_comp_name_range TYPE RANGE OF df14l-ps_posid.

    IF p_filter_name IS NOT INITIAL.
      lt_appl_comp_name_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT ps_posid AS name,
           text~name AS description
      FROM df14l AS comp
        LEFT OUTER JOIN df14t AS text
          ON  comp~fctr_id = text~fctr_id
          AND comp~as4local = text~as4local
      WHERE comp~as4local = 'A'
        AND comp~ps_posid IN @lt_appl_comp_name_range
        AND text~langu = @sy-langu
      ORDER BY comp~ps_posid
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.


ENDCLASS.
