"! <p class="shorttext synchronized">Provides available features</p>
CLASS zcl_adcoset_adt_res_features DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      c_pcre_available_feature    TYPE string VALUE 'pcreAvailable' ##NO_TEXT,
      c_tags_search_scope_feature TYPE string VALUE 'parameters.tagId' ##NO_TEXT,

      BEGIN OF c_feature_categories,
        request_attribute TYPE string VALUE 'RequestAttribute' ##NO_TEXT,
      END OF c_feature_categories.
ENDCLASS.


CLASS zcl_adcoset_adt_res_features IMPLEMENTATION.
  METHOD get.
    DATA(features) = VALUE zif_adcoset_ty_adt_types=>ty_adt_plugin_features(
                               category = c_feature_categories-request_attribute
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_settings_uri
                                 name        = c_pcre_available_feature
                                 type        = 'Boolean'
                                 enabled     = zcl_adcoset_pcre_util=>is_pcre_supported( )
                                 description = 'Indicates if Perl-Compatible-Regular-Expressions are available' )
                               ( endpoint    = zcl_adcoset_adt_disc_app=>search_scope_uri
                                 name        = c_tags_search_scope_feature
                                 type        = 'String'
                                 enabled     = zcl_adcoset_extensions_util=>is_abap_tags_available( )
                                 description = 'Extension for Code Search Scope' ) ).

    response->set_body_data( content_handler = zcl_adcoset_adt_ch_factory=>create_feature_list_ch( )
                             data            = features ).
  ENDMETHOD.
ENDCLASS.
