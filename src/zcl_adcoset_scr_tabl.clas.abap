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
    CONSTANTS c_tabl_persistence_class TYPE string VALUE 'CL_SBD_STRUCTURE_PERSIST' ##NO_TEXT.
    CONSTANTS c_initialize_method TYPE string VALUE 'IF_WB_OBJECT_PERSIST~INITIALIZE' ##NO_TEXT.
    CONSTANTS c_param_object_type TYPE string VALUE 'P_OBJECT_TYPE' ##NO_TEXT.

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

    METHODS initialize_table_persistence
      IMPORTING
        tabl_type TYPE trobjtype
      EXPORTING
        tabl_pers TYPE REF TO cl_sbd_structure_persist
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

    " todo Ludwig -> comment_regex not needed right?
    result = NEW zcl_adcoset_source_code( source        = source
                                          line_indexes  = indexes
                                          comment_regex = zif_adcoset_c_global=>c_cds_comment_regex ).
  ENDMETHOD.

  METHOD read_tabl.
    DATA object_data TYPE REF TO if_wb_object_data_model.

    initialize_table_persistence( EXPORTING tabl_type = type
                                  IMPORTING tabl_pers = DATA(table_pers) ).

    TRY.
        table_pers->if_wb_object_persist~get( EXPORTING p_object_key  = name
                                                        p_version     = 'A'
                                              CHANGING  p_object_data = object_data ).
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

  METHOD initialize_table_persistence.
    DATA lo_class_descr TYPE REF TO cl_abap_classdescr.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = c_tabl_persistence_class
                                         RECEIVING  p_descr_ref    = DATA(lo_type_descr)
                                         EXCEPTIONS type_not_found = 1 ).
    IF sy-subrc = 0 AND lo_type_descr->kind = cl_abap_typedescr=>kind_class.
      lo_class_descr ?= lo_type_descr.
    ELSE.
      RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ENDIF.

    DATA(param_type_object_type) = lo_class_descr->methods[ name = c_initialize_method ]-parameters[
        name = c_param_object_type ]-parm_kind.

    DATA(obj_type_value) = VALUE wbobjtype(
        objtype_tr = zif_adcoset_c_global=>c_source_code_type-table
        subtype_wb = COND #( WHEN tabl_type = zif_adcoset_c_global=>c_source_code_type-structure THEN
                               zif_adcoset_c_global=>c_source_code_sub_type-structure
                             WHEN tabl_type = zif_adcoset_c_global=>c_source_code_type-database_table THEN
                               zif_adcoset_c_global=>c_source_code_sub_type-databasetable ) ).

    DATA(param_tab) = VALUE abap_parmbind_tab(
                                ( name  = c_param_object_type
                                  kind  = COND #( WHEN param_type_object_type = cl_abap_objectdescr=>exporting
                                                  THEN cl_abap_objectdescr=>importing
                                                  ELSE cl_abap_objectdescr=>exporting )
                                  value = REF #( obj_type_value ) ) ).

    tabl_pers = NEW cl_sbd_structure_persist( ).

    TRY.
        CALL METHOD tabl_pers->(c_initialize_method)
            PARAMETER-TABLE param_tab.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
