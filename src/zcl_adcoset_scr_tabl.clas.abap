"! <p class="shorttext synchronized">Source code reader for table & structure</p>
CLASS zcl_adcoset_scr_tabl DEFINITION
  PUBLIC FINAL
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

    METHODS read_tabl
      IMPORTING
        !name         TYPE seu_objkey
        !type         TYPE trobjtype
      RETURNING
        VALUE(result) TYPE string
      RAISING
        zcx_adcoset_src_code_read.

ENDCLASS.


CLASS zcl_adcoset_scr_tabl IMPLEMENTATION.
  METHOD constructor.
    me->line_feed    = line_feed.
    me->is_multiline = is_multiline.
  ENDMETHOD.

  METHOD zif_adcoset_src_code_reader~get_source_code.
    DATA source TYPE TABLE OF string.
    DATA indexes TYPE zif_adcoset_source_code=>ty_line_indexes.

    DATA(source_text) = read_tabl( name = CONV #( name ) type = type ).

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

  METHOD read_tabl.
    DATA object_pers TYPE REF TO cl_sbd_structure_persist.
    DATA object_data TYPE REF TO if_wb_object_data_model.

    object_pers = NEW #( ).
    object_pers->if_wb_object_persist~initialize(
        p_object_type = COND #( WHEN type = zif_adcoset_c_global=>c_source_code_type-structure THEN
                                  VALUE #( objtype_tr =  zif_adcoset_c_global=>c_source_code_type-table subtype_wb = 'DS' )
                                WHEN type = zif_adcoset_c_global=>c_source_code_type-database_table THEN
                                  VALUE #( objtype_tr =  zif_adcoset_c_global=>c_source_code_type-table subtype_wb = 'DT' ) ) ).
    TRY.
        object_pers->if_wb_object_persist~get( EXPORTING p_object_key              = name
                                                         p_version                 = 'A'
                                               " TODO: variable is assigned but never used (ABAP cleaner)
                                               IMPORTING p_langu_is_not_maintained = DATA(is_langu_maint)
                                               CHANGING  p_object_data             = object_data ).
      CATCH cx_swb_object_does_not_exist.
      CATCH cx_swb_exception.

        RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ENDTRY.

    IF object_data IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ENDIF.

    object_data->get_content( IMPORTING p_data = result ).

    " handle line feed
    result = zcl_adcoset_string_util=>adjust_line_endings( text      = result
                                                           line_feed = line_feed ).
  ENDMETHOD.
ENDCLASS.
