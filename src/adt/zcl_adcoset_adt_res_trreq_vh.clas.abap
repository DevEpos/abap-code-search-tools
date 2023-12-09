"! <p class="shorttext synchronized">Resource for Application Component value help</p>
CLASS zcl_adcoset_adt_res_trreq_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_adt_res_trreq_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA lt_tr_request_name_range TYPE RANGE OF trkorr.

    IF p_filter_name IS NOT INITIAL.
      lt_tr_request_name_range = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( p_filter_name ) ) ).
    ENDIF.

    SELECT tr_request~trkorr AS name,
           text~as4text AS description
      FROM e070 AS tr_request
        LEFT OUTER JOIN e07t AS text
          ON  tr_request~trkorr = text~trkorr
      WHERE tr_request~trkorr IN @lt_tr_request_name_range
         OR upper( text~as4text ) IN @lt_tr_request_name_range
        AND text~langu = @sy-langu
      ORDER BY tr_request~trkorr
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
