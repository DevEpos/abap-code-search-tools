"! <p class="shorttext synchronized">Resource for Server Group Value Help</p>
CLASS zcl_adcoset_res_server_grp_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_res_server_grp_vh IMPLEMENTATION.
  METHOD get_named_items.
    DATA server_group_range TYPE RANGE OF zif_adcoset_ty_global=>ty_server_group.

    IF p_filter_name IS NOT INITIAL.
      server_group_range = VALUE #( ( sign = 'I' option = 'CP' low = p_filter_name ) ).
    ENDIF.

    SELECT classname AS name
      FROM rzllitab
      WHERE classname IN @server_group_range
        AND grouptype = @zif_adcoset_c_global=>c_group_type_server_group
      INTO CORRESPONDING FIELDS OF TABLE @p_named_item_list-items
      UP TO @p_filter_max_item_count ROWS.

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.
ENDCLASS.
