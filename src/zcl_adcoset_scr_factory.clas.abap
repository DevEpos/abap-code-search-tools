"! <p class="shorttext synchronized">Factory for creating source code readers</p>
CLASS zcl_adcoset_scr_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Retrieves source code reader for type</p>
    CLASS-METHODS get_reader
      IMPORTING
        !type         TYPE trobjtype
        is_multiline  TYPE abap_bool
        line_feed     TYPE string
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_src_code_reader
      RAISING
        zcx_adcoset_no_src_code_reader.

  PRIVATE SECTION.
    CONSTANTS c_generic_reposrc_type TYPE trobjtype VALUE '$REP'.

    TYPES:
      BEGIN OF ty_readers,
        type TYPE trobjtype,
        ref  TYPE REF TO zif_adcoset_src_code_reader,
      END OF ty_readers.

    CLASS-DATA readers TYPE STANDARD TABLE OF ty_readers WITH KEY type.

    CLASS-METHODS map_type
      IMPORTING
        original      TYPE trobjtype
      RETURNING
        VALUE(result) TYPE trobjtype.

    CLASS-METHODS create_reader
      IMPORTING
        !type         TYPE trobjtype
        line_feed     TYPE string
        is_multiline  TYPE abap_bool
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_src_code_reader
      RAISING
        zcx_adcoset_no_src_code_reader.
ENDCLASS.


CLASS zcl_adcoset_scr_factory IMPLEMENTATION.
  METHOD get_reader.
    DATA(mapped_type) = map_type( type ).
    TRY.
        result = readers[ type = mapped_type ]-ref.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( type = mapped_type
                        ref  = create_reader( type         = mapped_type
                                              is_multiline = is_multiline
                                              line_feed    = line_feed ) ) INTO TABLE readers ASSIGNING FIELD-SYMBOL(<reader>).
        result = <reader>-ref.
    ENDTRY.
  ENDMETHOD.

  METHOD map_type.
    result = SWITCH #( original
                       WHEN zif_adcoset_c_global=>c_source_code_type-class OR
                            zif_adcoset_c_global=>c_source_code_type-interface OR
                            zif_adcoset_c_global=>c_source_code_type-program OR
                            zif_adcoset_c_global=>c_source_code_type-type_group OR
                            zif_adcoset_c_global=>c_source_code_type-function_group OR
                            zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
                            zif_adcoset_c_global=>c_source_code_type-behavior_definition
                       THEN c_generic_reposrc_type

                       ELSE original ).
  ENDMETHOD.

  METHOD create_reader.
    result = SWITCH #( type

                       WHEN c_generic_reposrc_type THEN
                         NEW zcl_adcoset_scr_reposrc( is_multiline = is_multiline
                                                      line_feed    = line_feed )

                       WHEN zif_adcoset_c_global=>c_source_code_type-data_definition THEN
                         NEW zcl_adcoset_scr_ddls( is_multiline = is_multiline
                                                   line_feed    = line_feed )

                       WHEN zif_adcoset_c_global=>c_source_code_type-metadata_extension THEN
                         NEW zcl_adcoset_scr_ddlx( is_multiline = is_multiline
                                                   line_feed    = line_feed )

                       WHEN zif_adcoset_c_global=>c_source_code_type-access_control THEN
                         NEW zcl_adcoset_scr_dcls( is_multiline = is_multiline
                                                   line_feed    = line_feed )

                       WHEN zif_adcoset_c_global=>c_source_code_type-structure THEN
                         NEW zcl_adcoset_scr_tabl( is_multiline = is_multiline
                                                   line_feed    = line_feed )

                       WHEN zif_adcoset_c_global=>c_source_code_type-database_table THEN
                         NEW zcl_adcoset_scr_tabl( is_multiline = is_multiline
                                                   line_feed    = line_feed )

                       ELSE
                         THROW zcx_adcoset_no_src_code_reader( ) ).
  ENDMETHOD.
ENDCLASS.
