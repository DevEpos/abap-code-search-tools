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
                                                          type = zif_adcoset_c_global=>c_source_code_type-structure ).
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
    DATA(obj_name) = to_upper( object_name ).
    SELECT precfield FROM dd03l
      INTO TABLE includes
      WHERE tabname    = obj_name
        AND adminfield = 0 " level 1 includes and appends
        AND (    fieldname = '.INCLU--AP'
              OR fieldname = '.INCLUDE' ).
  ENDMETHOD.

  METHOD assign_objects_to_matches.
    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<match_without_source>).
      APPEND <match_without_source> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-object_name     = include.
      <match>-adt_object_type = zif_adcoset_c_global=>c_source_code_type-table.

      " set the display name
      <match>-display_name    = include.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
