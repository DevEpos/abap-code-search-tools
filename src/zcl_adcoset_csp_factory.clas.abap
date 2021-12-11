"! <p class="shorttext synchronized" lang="en">Factory for retrieving code search providers</p>
CLASS zcl_adcoset_csp_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves</p>
      get_search_provider
        IMPORTING
          type            TYPE trobjtype
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
        RETURNING
          VALUE(result)   TYPE REF TO zif_adcoset_code_search_prov
        RAISING
          zcx_adcoset_no_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_def_reposrc_provider    TYPE trobjtype VALUE '$REP',
      c_def_string_src_provider TYPE trobjtype VALUE '$SRC'.

    TYPES:
      BEGIN OF ty_providers,
        type TYPE trobjtype,
        ref  TYPE REF TO zif_adcoset_code_search_prov,
      END OF ty_providers.

    CLASS-DATA:
      providers TYPE HASHED TABLE OF ty_providers WITH UNIQUE KEY type.

    CLASS-METHODS:
      map_type
        IMPORTING
          original      TYPE trobjtype
        RETURNING
          VALUE(result) TYPE trobjtype,
      create_provider
        IMPORTING
          type            TYPE trobjtype
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
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
        INSERT VALUE #(
          type = mapped_type
          ref  = create_provider(
            type            = mapped_type
            search_settings = search_settings
            custom_settings = custom_settings
            matchers        = matchers ) ) INTO TABLE providers ASSIGNING FIELD-SYMBOL(<provider>).
        result = <provider>-ref.
    ENDTRY.
  ENDMETHOD.


  METHOD create_provider.
    result = SWITCH #( type

      WHEN zif_adcoset_c_global=>c_source_code_type-class THEN
        NEW zcl_adcoset_csp_clas(
          search_settings = search_settings
          custom_settings = custom_settings-class
          matchers        = matchers )

      WHEN zif_adcoset_c_global=>c_source_code_type-function_group THEN
        NEW zcl_adcoset_csp_fugr(
          search_settings = search_settings
          matchers        = matchers )

      WHEN c_def_reposrc_provider THEN
        NEW zcl_adcoset_csp_repsrc_default(
          search_settings = search_settings
          matchers        = matchers )

      when c_def_string_src_provider then
        NEW zcl_adcoset_csp_strsrc_default(
          search_settings = search_settings
          matchers        = matchers )

      ELSE
        THROW zcx_adcoset_no_provider( ) ).
  ENDMETHOD.


  METHOD map_type.
    result = SWITCH #( original

      WHEN zif_adcoset_c_global=>c_source_code_type-class OR
           zif_adcoset_c_global=>c_source_code_type-function_group
        THEN original

      WHEN zif_adcoset_c_global=>c_source_code_type-program OR
           zif_adcoset_c_global=>c_source_code_type-type_group OR
           zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
           zif_adcoset_c_global=>c_source_code_type-interface OR
           zif_adcoset_c_global=>c_source_code_type-behavior_definition
        THEN c_def_reposrc_provider

      WHEN zif_adcoset_c_global=>c_source_code_type-access_control OR
           zif_adcoset_c_global=>c_source_code_type-data_definition OR
           zif_adcoset_c_global=>c_source_code_type-metadata_extension OR
           zif_adcoset_c_global=>c_source_code_type-access_control
        THEN c_def_string_src_provider ).
  ENDMETHOD.

ENDCLASS.
