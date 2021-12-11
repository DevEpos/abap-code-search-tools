*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_find_matches_table FOR TESTING RAISING cx_static_check,
      test_find_matches_text FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD test_find_matches_table.
    DATA: source TYPE TABLE OF string.
    source = VALUE #(
      ( `  METHOD zif_adcoset_source_code~find_matches.` )
      ( )
      ( `    LOOP AT matchers INTO DATA(matcher).` )
      ( `      DATA(matches) = matcher->find_matches( source ).` )
      ( `      APPEND LINES OF enhance_matches( matches ) TO result.` )
      ( `    ENDLOOP.` )
      ( )
      ( `  ENDMETHOD.        ` ) ).

    FIND ALL OCCURRENCES OF SUBSTRING 'matcher' IN TABLE source IGNORING CASE RESULTS DATA(raw_matches).
    DATA(matcher) = CAST zif_adcoset_pattern_matcher(
      cl_abap_testdouble=>create( object_name = 'ZIF_ADCOSET_PATTERN_MATCHER' ) ).

    cl_abap_testdouble=>configure_call( matcher
      )->returning( raw_matches
      )->ignore_all_parameters( ).
    matcher->find_matches( source = VALUE #( ) ).

    DATA(cut) = NEW zcl_adcoset_source_code(
      source = source ).

    DATA(matches) = cut->zif_adcoset_source_code~find_matches( matchers = VALUE #( ( CAST #( matcher ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( matches )
      exp = lines( raw_matches ) ).
  ENDMETHOD.


  METHOD test_find_matches_text.
    DATA: source TYPE TABLE OF string.
    source = VALUE #(
      ( `  METHOD zif_adcoset_source_code~find_matches.` && |\r\n|  &&
        |\r\n|  &&
        `    LOOP AT matchers INTO DATA(matcher).` && |\r\n|  &&
        `      DATA(raw_matches) = matcher->find_matches( source ).` && |\r\n|  &&
        `      IF raw_matches IS NOT INITIAL.` && |\r\n|  &&
        `        result = VALUE #( BASE result ( LINES OF enhance_matches( raw_matches ) ) ).` && |\r\n|  &&
        `      ENDIF.` && |\r\n|  &&
        `    ENDLOOP.` && |\r\n|  &&
        |\r\n|  &&
        `  ENDMETHOD.` ) ).

    FIND ALL OCCURRENCES OF SUBSTRING 'matcher' IN TABLE source IGNORING CASE RESULTS DATA(raw_matches).
    DATA(matcher) = CAST zif_adcoset_pattern_matcher(
      cl_abap_testdouble=>create( object_name = 'ZIF_ADCOSET_PATTERN_MATCHER' ) ).

    cl_abap_testdouble=>configure_call( matcher
      )->returning( raw_matches
      )->ignore_all_parameters( ).
    matcher->find_matches( source = VALUE #( ) ).

    DATA(cut) = NEW zcl_adcoset_source_code(
      source = source
      line_indexes = zcl_adcoset_string_util=>determine_line_indexes(
        source_text  = source[ 1 ]
        line_feed    = |\r\n| ) ).

    DATA(matches) = cut->zif_adcoset_source_code~find_matches( matchers = VALUE #( ( CAST #( matcher ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( matches )
      exp = lines( raw_matches ) ).
  ENDMETHOD.

ENDCLASS.
