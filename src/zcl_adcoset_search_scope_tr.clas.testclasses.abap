*"* use this source file for your ABAP unit test classes

CLASS ltcl_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_native_sql_builder FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unit IMPLEMENTATION.
  METHOD test_native_sql_builder.
    DATA(cut) = NEW ZCL_ADCOSET_SEARCH_SCOPE_TR(
                        search_scope = VALUE #( object_name_range = VALUE #( ( sign = 'I' option = 'CP' low = 'CL_*' ) )
                                                object_type_range = VALUE #( option = 'EQ'
                                                                             ( sign = 'I' low = 'CLAS' )
                                                                             ( sign = 'E' low = 'INTF' ) )
                                                package_range     = VALUE #( ( sign = 'I' option = 'CP' low = 'AB*' ) )
                                                current_offset    = 0
                                                max_objects       = 50 ) ).

    cl_abap_unit_assert=>assert_true( cut->zif_adcoset_search_scope~has_next_package( ) ).
    DATA(obj_count) = cut->zif_adcoset_search_scope~count( ).

    DATA(results) = cut->zif_adcoset_search_scope~next_package( ).

    cl_abap_unit_assert=>assert_equals( act = lines( results-objects )
                                        exp = COND #( WHEN obj_count >= 50 THEN 50
                                                      WHEN obj_count < 50  THEN obj_count
                                                      ELSE                      0 ) ).
  ENDMETHOD.
ENDCLASS.
