CLASS zcl_adcoset_csp_tabl DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

  PRIVATE SECTION.
    TYPES include_list TYPE STANDARD TABLE OF precfield WITH DEFAULT KEY.

    METHODS get_includes
      IMPORTING
        object_name     TYPE sobj_name
      RETURNING
        VALUE(includes) TYPE include_list.

    METHODS get_includes_2
      IMPORTING
        object_name     TYPE sobj_name
      RETURNING
        VALUE(includes) TYPE include_list.

    METHODS assign_objects_to_matches
      IMPORTING
        unassigned_matches TYPE zif_adcoset_ty_global=>ty_search_matches
        !include           TYPE precfield
      CHANGING
        all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.


CLASS zcl_adcoset_csp_tabl IMPLEMENTATION.
  METHOD zif_adcoset_code_search_prov~search.
    DATA(searched_sources_count) = 1.
    TRY.
        DATA(source_code) = src_code_reader->get_source_code( name = object-name type = object-type ).
        DATA(matches) = src_code_searcher->search( source_code ).
        assign_objects_to_matches( EXPORTING unassigned_matches = matches
                                             include            = CONV #( object-name )
                                   CHANGING  all_matches        = result ).
      CATCH zcx_adcoset_src_code_read ##NO_HANDLER.
    ENDTRY.

    DATA(includes) = get_includes( object-name ).

    LOOP AT includes ASSIGNING FIELD-SYMBOL(<include>).
      CLEAR: source_code,
             matches.

      searched_sources_count = searched_sources_count + 1.

      TRY.
          source_code = src_code_reader->get_source_code( name = CONV #( <include> )
                                                          type = object-type ).
          matches = src_code_searcher->search( source_code ).
          assign_objects_to_matches( EXPORTING unassigned_matches = matches
                                               include            = <include>
                                     CHANGING  all_matches        = result ).

        CATCH zcx_adcoset_src_code_read ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
    zcl_adcoset_search_protocol=>increase_searchd_sources_count( searched_sources_count ).
  ENDMETHOD.

  METHOD get_includes.
    DATA include TYPE REF TO cl_abap_structdescr.

*    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( '/IWBEP/S_EPM_PRODUCT_HEADER' ) ).
    DATA(struct_descr) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( to_upper( object_name ) ) ).
    " to_upper( object-name ) ) ).
    DATA(components) = struct_descr->get_components( ).

    LOOP AT components ASSIGNING FIELD-SYMBOL(<include>) WHERE as_include = abap_true.
      include ?= <include>-type.
      includes = VALUE #( BASE includes ( include->get_relative_name( ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_includes_2.
    DATA(obj_name) = to_upper( object_name ).
    SELECT precfield FROM dd03l
      " TODO: variable is assigned but never used (ABAP cleaner)
      INTO TABLE includes
*      WHERE tabname = '/IWBEP/S_EPM_PRODUCT_HEADER'
      WHERE tabname = obj_name
        AND (    fieldname = '.INCLU--AP'
              OR fieldname = '.INCLUDE' ).
  ENDMETHOD.

  METHOD assign_objects_to_matches.
    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
      APPEND <match_without_source> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-include          = include.
      <match>-adt_include_type = 'TABL'.

      " set the display name
        <match>-display_name = include.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
