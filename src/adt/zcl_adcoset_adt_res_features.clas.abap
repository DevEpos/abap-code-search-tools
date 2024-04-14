"! <p class="shorttext synchronized">Provides available features</p>
CLASS zcl_adcoset_adt_res_features DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_pcre_available_feature TYPE string VALUE 'pcreAvailable'.
    CONSTANTS c_tags_search_scope_feature TYPE string VALUE 'parameters.tagId'.
    CONSTANTS:
      BEGIN OF c_feature_value_type,
        boolean TYPE string VALUE 'Boolean',
        string  TYPE string VALUE 'String',
      END OF c_feature_value_type.

    CONSTANTS:
      BEGIN OF c_cond_type_values,
        structure TYPE string VALUE 'parameters.type.structure',
        db_table  TYPE string VALUE 'parameters.type.dbTable',
      END OF c_cond_type_values.

    CONSTANTS:
      BEGIN OF c_feature_categories,
        request_attribute TYPE string VALUE 'RequestAttribute',
      END OF c_feature_categories.
ENDCLASS.


CLASS zcl_adcoset_adt_res_features IMPLEMENTATION.
  METHOD get.
    DATA(features) = VALUE zif_adcoset_ty_adt_types=>ty_adt_plugin_features(
                               category = c_feature_categories-request_attribute
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_settings_uri
                                 name        = c_pcre_available_feature
                                 type        = c_feature_value_type-boolean
                                 enabled     = zcl_adcoset_pcre_util=>is_pcre_supported( )
                                 description = 'Indicates if Perl-Compatible-Regular-Expressions are available' )
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_scope_uri
                                 name        = c_tags_search_scope_feature
                                 type        = c_feature_value_type-string
                                 enabled     = zcl_adcoset_extensions_util=>is_abap_tags_available( )
                                 description = 'Extension for Code Search Scope' )
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_scope_uri
                                 name        = c_cond_type_values-db_table
                                 type        = c_feature_value_type-boolean
                                 enabled     = xsdbool( sy-saprl > '751' )
                                 description = 'Availability of DTAB in type parameter' )
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_scope_uri
                                 name        = c_cond_type_values-structure
                                 type        = c_feature_value_type-boolean
                                 enabled     = xsdbool( sy-saprl > '740' )
                                 description = 'Availability of STRU in type parameter' ) ).

    response->set_body_data( content_handler = zcl_adcoset_adt_ch_factory=>create_feature_list_ch( )
                             data            = features ).
  ENDMETHOD.
ENDCLASS.
