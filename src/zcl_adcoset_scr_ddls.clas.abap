"! <p class="shorttext synchronized">Source code reader for CDS view</p>
CLASS zcl_adcoset_scr_ddls DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_src_code_reader.

    METHODS constructor
      IMPORTING
        is_multiline TYPE abap_bool
        line_feed    TYPE string.

  PRIVATE SECTION.
    DATA is_multiline TYPE abap_bool.
    DATA line_feed TYPE string.
    DATA is_baseinfo_supported TYPE abap_bool.

    METHODS read_ddls
      IMPORTING
        !name         TYPE ddlname
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_adcoset_src_code_read.

    METHODS check_base_info_support.
ENDCLASS.


CLASS zcl_adcoset_scr_ddls IMPLEMENTATION.
  METHOD constructor.
    me->line_feed    = line_feed.
    me->is_multiline = is_multiline.
    check_base_info_support( ).
  ENDMETHOD.

  METHOD zif_adcoset_src_code_reader~get_source_code.
    DATA source TYPE TABLE OF string.
    DATA indexes TYPE zif_adcoset_source_code=>ty_line_indexes.

    DATA(source_text) = read_ddls( name ).

    IF is_multiline = abap_true.
      indexes = zcl_adcoset_string_util=>determine_line_indexes( source_text = source_text
                                                                 line_feed   = line_feed ).
      source = VALUE #( ( source_text ) ).
    ELSE.
      SPLIT source_text AT line_feed INTO TABLE source.
    ENDIF.

    result = NEW zcl_adcoset_source_code( source        = source
                                          line_indexes  = indexes
                                          comment_regex = zif_adcoset_c_global=>c_cds_comment_regex ).
  ENDMETHOD.

  METHOD read_ddls.
    DATA(ddlname) = to_upper( name ).

    SELECT SINGLE source
      FROM ddddlsrc
      WHERE ddlname = @ddlname
        AND as4local = 'A' " only active source
      INTO @result.

    IF sy-subrc <> 0 OR result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ELSEIF is_baseinfo_supported = abap_false.
      RETURN.
    ENDIF.

    " handle line feed
    result = zcl_adcoset_string_util=>adjust_line_endings( text      = result
                                                           line_feed = line_feed ).

    " remove base info string which is not externally visible anyway
    DATA(base_info_offset) = find( val = result regex = '\s*[\r\n|\n]\s*/\*\+\[internal\]' ).

    IF base_info_offset > 0.
      result = substring( val = result len = base_info_offset ).
    ENDIF.
  ENDMETHOD.

  METHOD check_base_info_support.
    " TODO: variable is assigned but never used (ABAP cleaner)
    ASSIGN 'IF_DD_DDL_TYPES=>TY_S_BASEINFO_STRING_SAVE' TO FIELD-SYMBOL(<base_info_type>).
    is_baseinfo_supported = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.
ENDCLASS.
