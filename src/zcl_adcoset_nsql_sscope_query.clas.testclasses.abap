*"* use this source file for your ABAP unit test classes
CLASS ltcl_query_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_query_with_where FOR TESTING RAISING cx_static_check.
    methods test_cte_count_query for testing raising cx_static_check.
ENDCLASS.


CLASS ltcl_query_test IMPLEMENTATION.
  METHOD test_query_with_where.
    TYPES:
      BEGIN OF ty_result,
        obj_type TYPE tadir-object,
      END OF ty_result.

    DATA result TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.
    DATA obj_type_range TYPE RANGE OF tadir-object.

    obj_type_range = VALUE #( sign = 'I'
                              ( option = 'CP' low = 'C*' )
                              (  option = 'EQ' low = 'XSLT' ) ).

    DATA(query) = NEW zcl_adcoset_nsql_sscope_query( ).
    query->set_from( 'TADIR tdir' ).
    query->set_order_by( `obj_type` ).
    query->set_select( value    = 'tdir.object obj_type'
                       distinct = abap_true
                       cols     = VALUE #( ( 'OBJ_TYPE' ) ) ).
    query->add_range_to_where( ranges        = obj_type_range
                               sql_fieldname = 'tdir.object' ).
    query->set_limit( 10 ).

    cl_abap_unit_assert=>assert_true( query->execute_query( itab = REF #( result ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 10
                                        act = lines( result ) ).
  ENDMETHOD.

  METHOD test_cte_count_query.
    TYPES:
      BEGIN OF ty_result,
        obj_type TYPE tadir-object,
      END OF ty_result.

    DATA obj_type_range TYPE RANGE OF tadir-object.

    obj_type_range = VALUE #( sign = 'I'
                              ( option = 'CP' low = 'C*' )
                              (  option = 'EQ' low = 'XSLT' ) ).

    DATA(query) = NEW zcl_adcoset_nsql_sscope_query( ).
    query->set_from( 'TADIR tdir' ).
    query->set_order_by( `obj_type` ).
    query->set_select( value    = 'tdir.object obj_type'
                       distinct = abap_true
                       cols     = VALUE #( ( 'OBJ_TYPE' ) ) ).
    query->add_range_to_where( ranges        = obj_type_range
                               sql_fieldname = 'tdir.object' ).

    query->set_limit( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 42
                                        act = query->execute_cte_count( ) ).
  ENDMETHOD.

ENDCLASS.
