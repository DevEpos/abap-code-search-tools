"! <p class="shorttext synchronized">Resource for Application Component value help</p>
CLASS zcl_adcoset_adt_res_trreq_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

  PROTECTED SECTION.
    METHODS get_named_items REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_request,
        trkorr     TYPE e070-trkorr,
        as4text    TYPE e07t-as4text,
        trfunction TYPE e070-trfunction,
        trstatus   TYPE e070-trstatus,
      END OF ty_request,

      ty_requests TYPE STANDARD TABLE OF ty_request WITH EMPTY KEY.

    CONSTANTS c_split_marker TYPE string VALUE '@@##@@'.
    CONSTANTS c_filter_val_sep TYPE string VALUE ',' ##NO_TEXT.
    CONSTANTS c_true TYPE string VALUE 'true'.
    CONSTANTS c_false TYPE string VALUE 'false'.

    CONSTANTS:
      BEGIN OF c_date_filters,
        since_yesterday     TYPE string VALUE 'SINCE_YESTERDAY',
        from_two_weeks_ago  TYPE string VALUE 'FROM_TWO_WEEKS_AGO',
        from_four_weeks_ago TYPE string VALUE 'FROM_FOUR_WEEKS_AGO',
        all                 TYPE string VALUE 'ALL',
      END OF c_date_filters.

    CONSTANTS:
      BEGIN OF c_tr_status_ext,
        modifiable TYPE string VALUE 'modifiable',
        released   TYPE string VALUE 'released',
      END OF c_tr_status_ext.

    DATA excluded_types TYPE RANGE OF e070-trfunction.
    DATA name_filter TYPE RANGE OF e070-trkorr.
    DATA text_filter TYPE RANGE OF e07t-as4text.
    DATA changed_date_filter TYPE RANGE OF e070-as4date.
    DATA owner_filter TYPE RANGE OF e070-as4user.
    DATA status_filter TYPE RANGE OF e070-trstatus.
    DATA type_filter TYPE RANGE OF e070-trfunction.
    DATA requests TYPE ty_requests.
    DATA custom_filter_active TYPE abap_bool.
    DATA is_all_status_requested TYPE abap_bool.

    METHODS get_changed_date_filter
      IMPORTING
        values_str TYPE string.

    METHODS get_type_filter
      IMPORTING
        values_str TYPE string.

    METHODS get_generic_filter
      IMPORTING
        !name      TYPE string
        values_str TYPE string
        upper_case TYPE abap_bool OPTIONAL
      CHANGING
        range_tab  TYPE table.

    METHODS transform_special_values
      CHANGING
        user_filter LIKE owner_filter.

    METHODS select_tr_requests
      IMPORTING
        max_item_count TYPE i.

    METHODS convert_to_named_items
      RETURNING
        VALUE(result) TYPE if_adt_named_item=>ty_named_item_list.

    METHODS get_status_filter
      IMPORTING
        values_str TYPE string.

    METHODS parse_data_filter
      IMPORTING
        !filter TYPE string.

    METHODS get_name_filter
      IMPORTING
        !filter TYPE string.
ENDCLASS.


CLASS zcl_adcoset_adt_res_trreq_vh IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    excluded_types = VALUE #( sign   = 'I'
                              option = 'EQ'
                              ( low = sctsc_type_customizing )
                              ( low = sctsc_type_cust_task )
                              ( low = sctsc_type_unclass_task )
                              ( low = sctsc_type_client ) ).
  ENDMETHOD.

  METHOD get_named_items.
    CLEAR: is_all_status_requested,
           name_filter,
           text_filter,
           changed_date_filter,
           owner_filter,
           status_filter,
           type_filter,
           requests,
           custom_filter_active.

    parse_data_filter( filter = p_filter_data ).
    get_name_filter( filter = p_filter_name ).
    select_tr_requests( p_filter_max_item_count ).

    p_named_item_list = convert_to_named_items( ).

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.

  METHOD parse_data_filter.
    DATA filters_encoded TYPE string_table.

    IF filter IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT filter AT c_split_marker INTO TABLE filters_encoded.

    LOOP AT filters_encoded INTO DATA(single_filter).
      SPLIT single_filter AT '=' INTO DATA(name) DATA(val).
      CASE name.
        WHEN 'customFilter'.
          custom_filter_active = abap_true.
        WHEN 'changed'.
          get_changed_date_filter( val ).
        WHEN 'owner'.
          get_generic_filter( EXPORTING name       = name
                                        values_str = val
                                        upper_case = abap_true
                              CHANGING  range_tab  = owner_filter ).
          transform_special_values( CHANGING user_filter = owner_filter ).
        WHEN 'type'.
          get_type_filter( val ).
        WHEN 'status'.
          get_status_filter( val ).
      ENDCASE.
    ENDLOOP.

    IF is_all_status_requested = abap_true AND changed_date_filter IS INITIAL.
      CLEAR is_all_status_requested.
    ENDIF.
  ENDMETHOD.

  METHOD get_name_filter.
    IF filter IS INITIAL.
      RETURN.
    ENDIF.

    DATA(l_name_filter) = to_upper( COND string( WHEN filter = '*' AND custom_filter_active = abap_true
                                                 THEN |{ sy-sysid }K*|
                                                 ELSE filter ) ).
    DATA(l_text_filter) = COND string( WHEN filter = '*' AND custom_filter_active = abap_true
                                       THEN |{ sy-sysid }K*|
                                       ELSE filter ).
    name_filter = VALUE #( ( sign   = 'I'
                             option = 'CP'
                             low    = l_name_filter ) ).
    text_filter = VALUE #( ( sign = 'I' option = 'CP' low = l_text_filter ) ).
  ENDMETHOD.

  METHOD get_changed_date_filter.
    DATA values TYPE string_table.
    DATA date_range_line LIKE LINE OF changed_date_filter.

    SPLIT values_str AT c_filter_val_sep INTO TABLE values.

    LOOP AT values INTO DATA(value).
      CLEAR date_range_line.

      CASE value.
        WHEN c_date_filters-since_yesterday.
          date_range_line = VALUE #( sign   = 'I'
                                     option = 'GE'
                                     low    = sy-datum - 1 ).
        WHEN c_date_filters-from_two_weeks_ago.
          date_range_line = VALUE #( sign   = 'I'
                                     option = 'GE'
                                     low    = sy-datum - 14 ).
        WHEN c_date_filters-from_four_weeks_ago.
          date_range_line = VALUE #( sign   = 'I'
                                     option = 'GE'
                                     low    = sy-datum - 28 ).
        WHEN c_date_filters-all.
          CONTINUE.
        WHEN OTHERS.
          date_range_line = value.
      ENDCASE.

      APPEND date_range_line TO changed_date_filter.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_type_filter.
    DATA values TYPE string_table.

    SPLIT values_str AT c_filter_val_sep INTO TABLE values.

    LOOP AT values INTO DATA(value).
      value = to_upper( value ).
      CASE value.
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-workbench_request.
          type_filter = VALUE #( BASE type_filter ( sign = 'I' option = 'EQ' low = sctsc_type_workbench ) ).
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-dev_corr_task.
          type_filter = VALUE #( BASE type_filter ( sign = 'I' option = 'EQ' low = sctsc_type_correction ) ).
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-piece_list.
          type_filter = VALUE #( BASE type_filter
                                 sign   = 'I'
                                 option = 'EQ'
                                 ( low = sctsc_type_objlist )
                                 ( low = sctsc_type_projectlist )
                                 ( low = sctsc_type_upgradelist )
                                 ( low = sctsc_type_patch ) ).
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-repair_task.
          type_filter = VALUE #( BASE type_filter ( sign = 'I' option = 'EQ' low = sctsc_type_repair ) ).
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-transport_of_copies.
          type_filter = VALUE #( BASE type_filter ( sign = 'I' option = 'EQ' low = sctsc_type_transport ) ).
        WHEN zif_adcoset_c_global=>c_trkorr_type_vh-relocation_request.
          type_filter = VALUE #( BASE type_filter
                                 sign   = 'I'
                                 option = 'EQ'
                                 ( low = sctsc_type_relocation )
                                 ( low = sctsc_type_relocation_devclass )
                                 ( low = sctsc_type_relocation_objs ) ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_status_filter.
    DATA values TYPE string_table.

    SPLIT values_str AT c_filter_val_sep INTO TABLE values.

    is_all_status_requested = xsdbool( lines( values ) = 2 ).

    LOOP AT values INTO DATA(value).
      value = to_lower( value ).
      IF value = c_tr_status_ext-modifiable.
        status_filter = VALUE #( BASE status_filter sign   = 'I'
                                 option = 'EQ'
                                 ( low = sctsc_state_changeable )
                                 ( low = sctsc_state_protected  ) ).
      ELSEIF value = c_tr_status_ext-released.
        status_filter = VALUE #( BASE status_filter sign   = 'I'
                                 option = 'EQ'
                                 ( low = sctsc_state_export_started )
                                 ( low = sctsc_state_notconfirmed )
                                 ( low = sctsc_state_released ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_generic_filter.
    " TODO: parameter NAME is never used (ABAP cleaner)

    DATA values TYPE string_table.

    SPLIT values_str AT c_filter_val_sep INTO TABLE values.

    LOOP AT values INTO DATA(value).
      DATA(sign) = 'I'.
      DATA(option) = 'EQ'.

      IF upper_case = abap_true.
        value = to_upper( value ).
      ENDIF.

      IF value CP '!*'.
        value = value+1.
        sign = 'E'.
      ENDIF.

      IF value CA '*'.
        option = 'CP'.
      ENDIF.

      APPEND INITIAL LINE TO range_tab ASSIGNING FIELD-SYMBOL(<range_line>).
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<sign>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<option>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <range_line> TO FIELD-SYMBOL(<low>).

      <sign> = sign.
      <option> = option.
      <low> = value.
    ENDLOOP.
  ENDMETHOD.

  METHOD transform_special_values.
    LOOP AT user_filter REFERENCE INTO DATA(filter_row).
      IF filter_row->low = 'ME'.
        filter_row->low = sy-uname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD select_tr_requests.
    IF    custom_filter_active = abap_false
       OR ( custom_filter_active = abap_true AND is_all_status_requested = abap_false ).
      SELECT tr_request~trkorr,
             text~as4text,
             tr_request~trfunction,
             tr_request~trstatus
        FROM e070 AS tr_request
             LEFT OUTER JOIN e07t AS text
               ON  tr_request~trkorr = text~trkorr
               AND text~langu        = @sy-langu
        WHERE (    tr_request~trkorr IN @name_filter
                OR text~as4text      IN @text_filter )
          AND tr_request~as4user        IN @owner_filter
          AND tr_request~as4date        IN @changed_date_filter
          AND tr_request~trfunction     IN @type_filter
          " Exclude requests without source code objects
          AND tr_request~trfunction NOT IN @excluded_types
          AND tr_request~trstatus       IN @status_filter
        ORDER BY tr_request~trkorr
        INTO TABLE @requests
        UP TO @max_item_count ROWS.
    ELSE.
      SELECT tr_request~trkorr,
             text~as4text,
             tr_request~trfunction,
             tr_request~trstatus
        FROM e070 AS tr_request
             LEFT OUTER JOIN e07t AS text
               ON  tr_request~trkorr = text~trkorr
               AND text~langu        = @sy-langu
        WHERE (    tr_request~trkorr IN @name_filter
                OR text~as4text      IN @text_filter )
          AND tr_request~as4user        IN @owner_filter
          AND tr_request~trfunction     IN @type_filter
          " Exclude requests without source code objects
          AND tr_request~trfunction NOT IN @excluded_types
          AND (    tr_request~trstatus IN ( @sctsc_state_changeable, @sctsc_state_protected )
                OR (     tr_request~trstatus IN ( @sctsc_state_released, @sctsc_state_notconfirmed, @sctsc_state_export_started )
                     AND tr_request~as4date  IN @changed_date_filter ) )
        ORDER BY tr_request~trkorr
        INTO TABLE @requests
        UP TO @max_item_count ROWS.
    ENDIF.
  ENDMETHOD.

  METHOD convert_to_named_items.
    LOOP AT requests REFERENCE INTO DATA(request).
      result-items = VALUE #(
          BASE result-items
          ( name        = request->trkorr
            description = request->as4text
            data        = 'isTask=' && SWITCH string( request->trfunction
                                                      WHEN sctsc_type_correction OR
                                                           sctsc_type_repair
                                                      THEN c_true
                                                      ELSE c_false ) &&
                          c_split_marker &&
                          'isReleased=' && SWITCH string( request->trstatus
                                                          WHEN sctsc_state_notconfirmed OR sctsc_state_released
                                                          THEN c_true
                                                          ELSE c_false ) ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
