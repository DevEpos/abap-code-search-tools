"! <p class="shorttext synchronized">Resource for Transport Request VH</p>
CLASS zcl_adcoset_adt_res_trreq_vh DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_res_named_items FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

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
      BEGIN OF c_tr_type,
        cust_request      TYPE trfunction VALUE 'W',
        dev_corr_task     TYPE trfunction VALUE 'S',
        repair_task       TYPE trfunction VALUE 'R',
        cust_task         TYPE trfunction VALUE 'Q',
        unclassified_task TYPE trfunction VALUE 'X',
      END OF c_tr_type.

    CONSTANTS:
      BEGIN OF c_tr_status,
        modifiable               TYPE trstatus VALUE 'D',
        modifiable_protected     TYPE trstatus VALUE 'L',
        release_started          TYPE trstatus VALUE 'O',
        released                 TYPE trstatus VALUE 'R',
        released_with_protection TYPE trstatus VALUE 'N',
      END OF c_tr_status.
    CONSTANTS:
      BEGIN OF c_tr_status_ext,
        modifiable TYPE string VALUE 'modifiable',
        released   TYPE string VALUE 'released',
      END OF c_tr_status_ext.

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

    METHODS get_generic_filter
      IMPORTING
        !name      TYPE string
        values_str TYPE string
        upper_case TYPE abap_bool OPTIONAL
      CHANGING
        range_tab  TYPE table.

    METHODS get_filters
      IMPORTING
        name_filter_str TYPE string
        data_filter_str TYPE string.

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
ENDCLASS.


CLASS zcl_adcoset_adt_res_trreq_vh IMPLEMENTATION.
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

    get_filters( name_filter_str = p_filter_name
                 data_filter_str = p_filter_data ).

    select_tr_requests( p_filter_max_item_count ).

    p_named_item_list = convert_to_named_items( ).

    p_filter_already_applied = abap_true.
    p_named_item_list-total_item_count = lines( p_named_item_list-items ).
  ENDMETHOD.

  METHOD get_filters.
    DATA data_filters_encoded TYPE string_table.

    IF name_filter_str IS NOT INITIAL.
      name_filter = VALUE #( ( sign   = 'I'
                               option = 'CP'
                               low    = to_upper( COND string( WHEN name_filter_str = '*'
                                                               THEN |{ sy-sysid }K*|
                                                               ELSE name_filter_str ) ) ) ).
      text_filter = VALUE #( ( sign = 'I' option = 'CP' low = to_upper( name_filter_str ) ) ).
    ENDIF.

    IF data_filter_str IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT data_filter_str AT c_split_marker INTO TABLE data_filters_encoded.

    LOOP AT data_filters_encoded INTO DATA(single_filter).
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
          get_generic_filter( EXPORTING name       = name
                                        values_str = val
                                        upper_case = abap_true
                              CHANGING  range_tab  = type_filter ).
        WHEN 'status'.
          get_status_filter( val ).
      ENDCASE.
    ENDLOOP.

    IF is_all_status_requested = abap_true AND changed_date_filter IS INITIAL.
      CLEAR is_all_status_requested.
    ENDIF.
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

  METHOD get_status_filter.
    DATA values TYPE string_table.

    SPLIT values_str AT c_filter_val_sep INTO TABLE values.

    is_all_status_requested = xsdbool( lines( values ) = 2 ).

    LOOP AT values INTO DATA(value).
      value = to_lower( value ).
      IF value = c_tr_status_ext-modifiable.
        status_filter = VALUE #( BASE status_filter sign   = 'I'
                                 option = 'EQ'
                                 ( low = c_tr_status-modifiable )
                                 ( low = c_tr_status-modifiable_protected  ) ).
      ELSEIF value = c_tr_status_ext-released.
        status_filter = VALUE #( BASE status_filter sign   = 'I'
                                 option = 'EQ'
                                 ( low = c_tr_status-release_started )
                                 ( low = c_tr_status-released )
                                 ( low = c_tr_status-released_with_protection ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_generic_filter.
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
               ON tr_request~trkorr = text~trkorr
        WHERE (    tr_request~trkorr     IN @name_filter
                OR upper( text~as4text ) IN @text_filter )
          AND text~langu                 = @sy-langu
          AND tr_request~as4user        IN @owner_filter
          AND tr_request~as4date        IN @changed_date_filter
          AND tr_request~trfunction     IN @type_filter
          " Exclude customizing Tasks/Requests
          AND tr_request~trfunction NOT IN ( @c_tr_type-cust_request, @c_tr_type-cust_task )
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
               ON tr_request~trkorr = text~trkorr
        WHERE (    tr_request~trkorr     IN @name_filter
                OR upper( text~as4text ) IN @text_filter )
          AND text~langu                 = @sy-langu
          AND tr_request~as4user        IN @owner_filter
          AND tr_request~trfunction     IN @type_filter
          " Exclude customizing Tasks/Requests
          AND tr_request~trfunction NOT IN ( @c_tr_type-cust_request, @c_tr_type-cust_task )
          AND (    tr_request~trstatus IN ( @c_tr_status-modifiable, @c_tr_status-modifiable_protected )
                OR (     tr_request~trstatus IN ( @c_tr_status-released, @c_tr_status-released_with_protection, @c_tr_status-release_started )
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
                                                      WHEN c_tr_type-dev_corr_task OR c_tr_type-unclassified_task OR c_tr_type-repair_task
                                                      THEN c_true
                                                      ELSE c_false ) &&
                          c_split_marker &&
                          'isReleased=' && SWITCH string( request->trstatus
                                                          WHEN c_tr_status-released OR c_tr_status-released_with_protection
                                                          THEN c_true
                                                          ELSE c_false ) ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
