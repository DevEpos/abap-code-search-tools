"! <p class="shorttext synchronized">Factory for ADT links</p>
CLASS zcl_adcoset_adt_obj_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_adt_obj_factory.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_adt_obj_factory.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zcl_adcoset_adt_obj_factory.

    DATA uri_mapper TYPE REF TO if_adt_uri_mapper.
    DATA standard_mapping_options TYPE REF TO if_adt_mapping_options.
    DATA class_sections_mapping_options TYPE REF TO if_adt_mapping_options.

    METHODS constructor.

    METHODS map_wb_object_to_objref
      IMPORTING
        wb_object     TYPE REF TO cl_wb_object
      RETURNING
        VALUE(result) TYPE sadt_object_reference
      RAISING
        zcx_adcoset_static_error.
ENDCLASS.


CLASS zcl_adcoset_adt_obj_factory IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.
    result = instance.
  ENDMETHOD.

  METHOD constructor.
    DATA(adt_core_factory) = cl_adt_tools_core_factory=>get_instance( ).
    uri_mapper = adt_core_factory->get_uri_mapper( ).

    standard_mapping_options       = adt_core_factory->create_mapping_options( ).
    class_sections_mapping_options = adt_core_factory->create_mapping_options( ).

    standard_mapping_options->get_frag_types_accept_header(
      )->add( accepted_fragment_type = if_adt_frag_types_accept_hdr=>co_fragment_type_plaintext ).

    class_sections_mapping_options->get_frag_types_accept_header(
      )->add( accepted_fragment_type = if_adt_frag_types_accept_hdr=>co_fragment_type_plaintext
      )->add( accepted_fragment_type = if_adt_frag_types_accept_hdr=>co_fragment_type_workbench ).
  ENDMETHOD.

  METHOD zif_adcoset_adt_obj_factory~add_position_fragment.
    CHECK:
      link IS NOT INITIAL,
      start_line > 0.

    IF link NS '#'.
      link = |{ link }#|.
    ENDIF.

    link = |{ link }start={ start_line },{ COND #( WHEN start_column < 0 THEN 0 ELSE start_column ) }|.

    IF end_line <= 0 OR end_line < start_line OR end_column < 0.
      RETURN.
    ENDIF.

    link = |{ link };end={ end_line },{ end_column }|.
  ENDMETHOD.

  METHOD zif_adcoset_adt_obj_factory~get_object_ref_for_sub_object.
    cl_wb_object=>create_from_global_type( EXPORTING  p_object_type  = wbobjtype
                                                      p_enclobj_name = CONV #( enclosing_object )
                                                      p_object_name  = sub_object
                                           RECEIVING  p_wb_object    = DATA(wb_object)
                                           EXCEPTIONS OTHERS         = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDIF.

    result = map_wb_object_to_objref( wb_object ).
  ENDMETHOD.

  METHOD zif_adcoset_adt_obj_factory~get_object_ref_for_trobj.
    cl_wb_object=>create_from_transport_key( EXPORTING  p_object    = type
                                                        p_obj_name  = CONV #( name )
                                             RECEIVING  p_wb_object = DATA(wb_object)
                                             EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDIF.

    result = map_wb_object_to_objref( wb_object ).

    IF     result                 IS NOT INITIAL
       AND append_source_uri_path  = abap_true.

      result-uri = |{ result-uri }/source/main|.
    ENDIF.
  ENDMETHOD.

  METHOD zif_adcoset_adt_obj_factory~get_object_ref_for_include.
    DATA mapping_options TYPE REF TO if_adt_mapping_options.

    IF start_line <= 0.
      IF    include+30(2) = 'CU'
         OR include+30(2) = 'CO'
         OR include+30(2) = 'CI'
         OR include+30(2) = 'CM'.
        mapping_options = class_sections_mapping_options.
      ELSE.
        mapping_options = standard_mapping_options.
      ENDIF.
    ELSE.
      mapping_options = standard_mapping_options.
    ENDIF.

    TRY.
        DATA(adt_obj_ref) = uri_mapper->map_include_to_objref( program         = main_program
                                                               include         = include
                                                               line            = start_line
                                                               line_offset     = start_line_offset
                                                               end_line        = end_line
                                                               end_offset      = end_line_offset
                                                               mapping_options = mapping_options ).

        result = adt_obj_ref->ref_data.
      CATCH cx_adt_uri_mapping INTO DATA(mapping_error).
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING previous = mapping_error.
    ENDTRY.
  ENDMETHOD.

  METHOD map_wb_object_to_objref.
    TRY.
        DATA(adt_obj_ref) = uri_mapper->map_wb_object_to_objref( wb_object = wb_object ).
        result = adt_obj_ref->ref_data.
      CATCH cx_adt_uri_mapping INTO DATA(mapping_error).
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING previous = mapping_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
