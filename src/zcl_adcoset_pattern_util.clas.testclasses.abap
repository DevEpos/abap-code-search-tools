*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_valid_sequence1   FOR TESTING.
    METHODS test_valid_sequence2   FOR TESTING.
    METHODS test_valid_sequence3   FOR TESTING.
    METHODS test_valid_sequence4   FOR TESTING.
    METHODS test_valid_sequence5   FOR TESTING.
    METHODS test_valid_sequence6   FOR TESTING.
    METHODS test_invalid_sequence1 FOR TESTING.
    METHODS test_invalid_sequence2 FOR TESTING.
    METHODS test_invalid_sequence3 FOR TESTING.
    METHODS test_invalid_sequence4 FOR TESTING.
    METHODS test_invalid_sequence5 FOR TESTING.
    METHODS test_invalid_sequence6 FOR TESTING.
    METHODS test_invalid_sequence7 FOR TESTING.
    METHODS test_invalid_sequence8 FOR TESTING.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.
  METHOD test_valid_sequence1.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start) loop at' )
                                                               ( content = ' group' )
                                                               ( content = '(#b-end).' )
                                                               ( content = '(#m-start) select' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>assert_equals( exp = lines( patterns )
                                            act = lines( valid_patterns ) ).
        cl_abap_unit_assert=>assert_true(
            act = xsdbool( valid_patterns[ 3 ]-flags BIT-AND zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-boundary_end =
                           zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-boundary_end ) ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( msg = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_valid_sequence2.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start) data(' )
                                                               ( content = '(#exclude)val' )
                                                               ( content = ' = ' )
                                                               ( content = '(#b-end).' ) ).

    TRY.
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>assert_equals( exp = lines( patterns )
                                            act = lines( valid_patterns ) ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( msg = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_valid_sequence3.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start) data(' )
                                                               ( content = '(#exclude)val' )
                                                               ( content = '(#match) = ' )
                                                               ( content = '(#b-end).' ) ).

    TRY.
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>assert_equals( exp = lines( patterns )
                                            act = lines( valid_patterns ) ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( msg = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_valid_sequence4.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start)(#m-start) data(' )
                                                               ( content = '(#exclude)val' )
                                                               ( content = '(#m-end) = ' )
                                                               ( content = '(#b-end).' ) ).

    TRY.
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>assert_equals( exp = lines( patterns )
                                            act = lines( valid_patterns ) ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( msg = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_valid_sequence5.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#exclude)loop' )
                                                               ( content = '(#m-start) select' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( msg = error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_valid_sequence6.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = ' loop' )
                                                               ( content = '(#m-start) select' )
                                                               ( content = '(#m-end).' )
                                                               ( content = '(#exclude) endloop' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
      CATCH zcx_adcoset_static_error INTO DATA(error).
        cl_abap_unit_assert=>fail( error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence1.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-end) loop at' )
                                                               ( content = ' group' )
                                                               ( content = '(#b-end).' )
                                                               ( content = '(#m-start) select' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Pattern sequence validation should have failed' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence2.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start) loop at' )
                                                               ( content = ' group' )
                                                               ( content = '(#b-startd).' )
                                                               ( content = '(#m-start) select' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Pattern sequence validation should have failed' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence3.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#m-end) select' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Pattern sequence validation should have failed' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence4.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start) select' )
                                                               ( content = '(#b-start) d' )
                                                               ( content = '(#m-end).' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Pattern sequence validation should have failed' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence5.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#match).' )
                                                               ( content = '(#exclude) loop' )
                                                               ( content = '(#match) 2' ) ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(valid_patterns) = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Pattern sequence validation should have failed' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence6.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-start)(#m-start)value' ) ).

    TRY.
        zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Multiple seqeuences at a single pattern assigned' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence7.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = '(#b-Start) value' ) ).

    TRY.
        zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Invalid Sequence found' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_sequence8.
    DATA(patterns) = VALUE zif_adcoset_ty_global=>ty_patterns( ( content = 'value' )
                                                               ( content = '(#exclude) endloop' )
                                                               ( content = '(#m-start) test' ) ).

    TRY.
        zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns ).
        cl_abap_unit_assert=>fail( msg = 'Invalid Sequence found' ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
