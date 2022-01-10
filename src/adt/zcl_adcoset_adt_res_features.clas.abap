"! <p class="shorttext synchronized" lang="en">Provides available features</p>
CLASS zcl_adcoset_adt_res_features DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_pcre_available_feature TYPE string VALUE 'pcreAvailable' ##NO_TEXT.

ENDCLASS.



CLASS zcl_adcoset_adt_res_features IMPLEMENTATION.


  METHOD get.
    DATA(features) = VALUE zif_adcoset_ty_adt_types=>ty_adt_plugin_features(
      ( endpoint    = zcl_adcoset_adt_disc_app=>search_settings_uri
        name        = c_pcre_available_feature
        type        = 'Boolean'
        category    = 'RequestAttribute'
        enabled     = zcl_adcoset_matcher_factory=>is_pcre_supported( )
        description = 'Indicates if Perl-Compatible-Regular-Expressions are available' ) ).

    response->set_body_data(
      content_handler = zcl_adcoset_adt_ch_factory=>create_feature_list_ch( )
      data            = features ).
  ENDMETHOD.


ENDCLASS.
