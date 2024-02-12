"! <p class="shorttext synchronized">Factory for retrieving code search providers</p>
CLASS zcl_adcoset_csp_factory DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Retrieves</p>
    CLASS-METHODS get_search_provider
      IMPORTING
        !type           TYPE trobjtype
        custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
      RETURNING
        VALUE(result)   TYPE REF TO zif_adcoset_code_search_prov
      RAISING
        zcx_adcoset_no_provider.

  PRIVATE SECTION.
    CONSTANTS c_def_reposrc_provider TYPE trobjtype VALUE '$REP'.
    CONSTANTS c_def_string_src_provider TYPE trobjtype VALUE '$SRC'.

    TYPES:
      BEGIN OF ty_providers,
        type TYPE trobjtype,
        ref  TYPE REF TO zif_adcoset_code_search_prov,
      END OF ty_providers.

    CLASS-DATA providers TYPE STANDARD TABLE OF ty_providers WITH KEY type.

    CLASS-METHODS map_type
      IMPORTING
        original      TYPE trobjtype
      RETURNING
        VALUE(result) TYPE trobjtype.

    CLASS-METHODS create_provider
      IMPORTING
        !type           TYPE trobjtype
        custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
      RETURNING
        VALUE(result)   TYPE REF TO zif_adcoset_code_search_prov
      RAISING
        zcx_adcoset_no_provider.
ENDCLASS.


CLASS zcl_adcoset_csp_factory IMPLEMENTATION.
  METHOD get_search_provider.
    DATA(mapped_type) = map_type( type ).
    TRY.
        result = providers[ type = mapped_type ]-ref.
      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( type = mapped_type
                        ref  = create_provider( type            = mapped_type
                                                custom_settings = custom_settings ) ) INTO TABLE providers ASSIGNING FIELD-SYMBOL(<provider>).
        result = <provider>-ref.
    ENDTRY.
  ENDMETHOD.

  METHOD create_provider.
    result = SWITCH #( type

                       WHEN zif_adcoset_c_global=>c_source_code_type-class THEN
                         NEW zcl_adcoset_csp_clas( custom_settings = custom_settings-class )

                       WHEN zif_adcoset_c_global=>c_source_code_type-function_group THEN
                         NEW zcl_adcoset_csp_fugr( custom_settings = custom_settings-fugr )

                       WHEN zif_adcoset_c_global=>c_source_code_type-program THEN
                         NEW zcl_adcoset_csp_prog( custom_settings = custom_settings-prog )

                       WHEN c_def_reposrc_provider THEN
                         NEW zcl_adcoset_csp_repsrc_default( )

                       WHEN c_def_string_src_provider THEN
                         NEW zcl_adcoset_csp_strsrc_default( )

                       ELSE
                         THROW zcx_adcoset_no_provider( ) ).
  ENDMETHOD.

  METHOD map_type.
    result = SWITCH #( original

                       WHEN zif_adcoset_c_global=>c_source_code_type-class OR
                            zif_adcoset_c_global=>c_source_code_type-function_group OR
                            zif_adcoset_c_global=>c_source_code_type-program THEN
                         original

                       WHEN zif_adcoset_c_global=>c_source_code_type-type_group OR
                            zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
                            zif_adcoset_c_global=>c_source_code_type-interface OR
                            zif_adcoset_c_global=>c_source_code_type-behavior_definition THEN
                         c_def_reposrc_provider

                       WHEN zif_adcoset_c_global=>c_source_code_type-access_control OR
                            zif_adcoset_c_global=>c_source_code_type-data_definition OR
                            zif_adcoset_c_global=>c_source_code_type-metadata_extension OR
                            zif_adcoset_c_global=>c_source_code_type-access_control THEN
                         c_def_string_src_provider ).
  ENDMETHOD.
ENDCLASS.
