"! <p class="shorttext synchronized" lang="en">Source code reader for CDS view</p>
CLASS zcl_adcoset_scr_ddls DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_src_code_reader.

    METHODS:
      constructor
        IMPORTING
          line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      line_feed_type        TYPE zif_adcoset_ty_global=>ty_line_feed_type,
      is_baseinfo_supported TYPE abap_bool.

    METHODS:
      read_ddls
        IMPORTING
          name          TYPE ddlname
        RETURNING
          VALUE(result) TYPE string,
      check_base_info_support.
ENDCLASS.



CLASS zcl_adcoset_scr_ddls IMPLEMENTATION.


  METHOD constructor.
    me->line_feed_type = line_feed_type.
    check_base_info_support( ).
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_table.
    result = zcl_adcoset_string_util=>split_at_line_feed(
      text           = read_ddls( name )
      line_feed_type = line_feed_type ).
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_text.
    result = read_ddls( name ).
  ENDMETHOD.


  METHOD read_ddls.
    DATA(ddlname) = to_upper( name ).

    SELECT SINGLE source
      FROM ddddlsrc
      WHERE ddlname = @ddlname
        AND as4local = 'A' " only active source
      INTO @result.

    IF sy-subrc <> 0 OR is_baseinfo_supported = abap_false.
      RETURN.
    ENDIF.

    " handle line feed
    result = zcl_adcoset_string_util=>adjust_line_endings(
      text           = result
      line_feed_type = line_feed_type ).

    " remove base info string which is not externally visible anyway
    DATA(base_info_offset) = find( val = result regex = '\s*[\r\n|\n]\s*/\*\+\[internal\]' ).

    IF base_info_offset > 0.
      result = substring( val = result len = base_info_offset ).
    ENDIF.
  ENDMETHOD.


  METHOD check_base_info_support.
    ASSIGN 'IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE' TO FIELD-SYMBOL(<base_info_type>).
    is_baseinfo_supported = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.


ENDCLASS.
