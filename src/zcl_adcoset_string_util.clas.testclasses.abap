*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
  constants: c_lf_regex type string value `^\n|[^\r]\n`.
    METHODS:
      test_tranform_to_string FOR TESTING,
      test_adjust_line_endings FOR TESTING,
      test_adjust_line_endings2 FOR TESTING,
      test_adjust_line_endings3 FOR TESTING.
ENDCLASS.

CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD test_tranform_to_string.
    DATA(source_table) = VALUE string_table(
      ( `This is the first line` )
      ( `and another line` )
      ( ` now we are done` ) ).

    DATA(line_feed) = |\r\n|.
    DATA(line_feed_length) = strlen( line_feed ).

    zcl_adcoset_string_util=>transform_to_string(
      EXPORTING
        source_table = source_table
        line_feed    = line_feed
      IMPORTING
        source_text  = DATA(source_text)
        indexes      = DATA(indexes) ).

    cl_abap_unit_assert=>assert_not_initial(
      act = source_text
      msg = 'Transformation failed' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = indexes
      msg = 'No indexes were determined' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( indexes )
      exp = 3
      msg = 'Number of determined indexes does not match' ).

    DATA(length) = REDUCE i(
      INIT n = 0
      FOR line IN source_table
      NEXT n = n + strlen( line ) + line_feed_length ) - line_feed_length.

    cl_abap_unit_assert=>assert_equals(
      act = strlen( source_text )
      exp = length
      msg = 'Length of transform string is false' ).
  ENDMETHOD.


  METHOD test_adjust_line_endings.
    DATA(text) = `    zcl_adcoset_string_util=>transform_to_string(` && |\n|  &&
                 `      EXPORTING` && |\n|  &&
                 `        source_table = source_table` && |\r\n|  &&
                 `        line_feed    = line_feed` && |\r\n|  &&
                 `      IMPORTING` && |\n|  &&
                 `        source_text  = DATA(source_text)` && |\n|  &&
                 `        indexes      = DATA(indexes) ).`.

    DATA(adjusted_text) = zcl_adcoset_string_util=>adjust_line_endings(
      text      = text
      line_feed = |\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = find( val = adjusted_text sub = |\r\n| )
      exp = -1 ).
  ENDMETHOD.


  METHOD test_adjust_line_endings2.
    DATA(text) = `    zcl_adcoset_string_util=>transform_to_string(` && |\n|  &&
                 `      EXPORTING` && |\n|  &&
                 `        source_table = source_table` && |\r\n|  &&
                 `        line_feed    = line_feed` && |\r\n|  &&
                 `      IMPORTING` && |\n|  &&
                 `        source_text  = DATA(source_text)` && |\n|  &&
                 `        indexes      = DATA(indexes) ).`.

    DATA(adjusted_text) = zcl_adcoset_string_util=>adjust_line_endings(
      text      = text
      line_feed = |\r\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = find( val = adjusted_text regex = c_lf_regex )
      exp = -1 ).
  ENDMETHOD.


  METHOD test_adjust_line_endings3.
    DATA(text) = |\n| && `    zcl_adcoset_string_util=>transform_to_string(` && |\r\n|  &&
                 `      EXPORTING` && |\r\n|  &&
                 `        source_table = source_table` && |\r\n|  &&
                 `        line_feed    = line_feed` && |\r\n|  &&
                 `      IMPORTING` && |\r\n|  &&
                 `        source_text  = DATA(source_text)` && |\r\n|  &&
                 `        indexes      = DATA(indexes) ).`.

    DATA(adjusted_text) = zcl_adcoset_string_util=>adjust_line_endings(
      text      = text
      line_feed = |\r\n| ).

    cl_abap_unit_assert=>assert_equals(
      act = find( val = adjusted_text regex = c_lf_regex )
      exp = -1 ).
  ENDMETHOD.

ENDCLASS.
