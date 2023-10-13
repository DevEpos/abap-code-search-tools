"! <p class="shorttext synchronized">Maps technical to display name of Core Data Services</p>
CLASS zcl_adcoset_cds_name_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Collects entry for mapping</p>
    METHODS collect_entry
      IMPORTING
        !name         TYPE string
        !type         TYPE trobjtype
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Tries to map all collected entries</p>
    METHODS map_entries
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Retrieves display name of a given entry</p>
    METHODS get_display_name
      IMPORTING
        !name         TYPE string
        !type         TYPE trobjtype
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ddl2entity_map_entry,
        ddlname     TYPE ddlname,
        entity_name TYPE objectname,
      END OF ty_ddl2entity_map_entry,

      ty_ddl2entity_map TYPE HASHED TABLE OF ty_ddl2entity_map_entry WITH UNIQUE KEY ddlname.

    DATA ddlname_range TYPE RANGE OF ddlname.
    DATA bdefname_range TYPE RANGE OF objectname.
    DATA ddl2entity_map TYPE ty_ddl2entity_map.
    DATA bdef2entity_map TYPE ty_ddl2entity_map.
ENDCLASS.


CLASS zcl_adcoset_cds_name_mapper IMPLEMENTATION.
  METHOD collect_entry.
    IF type = zif_adcoset_c_global=>c_source_code_type-data_definition.
      ddlname_range = VALUE #( BASE ddlname_range ( sign = 'I' option = 'EQ' low = name ) ).
    ELSEIF type = zif_adcoset_c_global=>c_source_code_type-behavior_definition.
      bdefname_range = VALUE #( BASE bdefname_range ( sign = 'I' option = 'EQ' low = name ) ).
    ELSE.
      RETURN.
    ENDIF.

    result = abap_true.
  ENDMETHOD.

  METHOD get_display_name.
    DATA entity_map_entry TYPE REF TO ty_ddl2entity_map_entry.

    IF type = zif_adcoset_c_global=>c_source_code_type-data_definition.
      entity_map_entry = REF #( ddl2entity_map[ ddlname = name ] OPTIONAL ).
    ELSEIF type = zif_adcoset_c_global=>c_source_code_type-behavior_definition.
      entity_map_entry = REF #( bdef2entity_map[ ddlname = name ] OPTIONAL ).
    ENDIF.

    IF entity_map_entry IS NOT INITIAL.
      result = entity_map_entry->entity_name.
    ENDIF.
  ENDMETHOD.

  METHOD map_entries.
    IF ddlname_range IS NOT INITIAL.
      SELECT dep~ddlname,
             stob~strucobjn_raw AS entity_name
        FROM ddldependency AS dep
          INNER JOIN dd02b AS stob
            ON dep~objectname = stob~strucobjn
        WHERE ddlname IN @ddlname_range
          AND objecttype = 'STOB'
          AND state = 'A'
        INTO CORRESPONDING FIELDS OF TABLE @ddl2entity_map.
    ENDIF.

    IF bdefname_range IS NOT INITIAL.
      SELECT strucobjn AS ddlname,
             strucobjn_raw AS entity_name
        FROM dd02b
        WHERE strucobjn IN @bdefname_range
        INTO CORRESPONDING FIELDS OF TABLE @bdef2entity_map.
    ENDIF.

    result = xsdbool( ddl2entity_map IS NOT INITIAL OR bdef2entity_map IS NOT INITIAL ).
  ENDMETHOD.
ENDCLASS.
