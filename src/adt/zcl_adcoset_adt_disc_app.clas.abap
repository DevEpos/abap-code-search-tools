"! <p class="shorttext synchronized" lang="en">Router for ABAP Code Search</p>
CLASS zcl_adcoset_adt_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA:
      "! URI for search settings resource
      search_settings_uri TYPE string READ-ONLY,
      search_scope_uri    TYPE string READ-ONLY.

    CLASS-METHODS
      class_constructor.

    METHODS:
      if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      fill_router REDEFINITION,
      get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_search_uri                TYPE string VALUE '/codesearch',
      c_settings_relative_uri     TYPE string VALUE '/settings',
      c_search_scope_relative_uri TYPE string VALUE '/scope',
      c_pattern_validator_rel_uri TYPE string VALUE '/validatepattern',
      c_static_uri                TYPE string VALUE '/devepos/adt/cst',
      c_root_scheme               TYPE string VALUE 'http://www.devepos.com/adt/cst',
      c_root_rel_scheme           TYPE string VALUE 'http://www.devepos.com/adt/relations/cst',

      "! REST handler constants
      BEGIN OF c_handlers,
        search            TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_SEARCH',
        search_settings   TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_CS_CONFIG',
        search_scope      TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_CS_SCOPE',
        appl_comp_vh      TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_APPLC_VH',
        server_group_vh   TYPE string VALUE 'ZCL_ADCOSET_RES_SERVER_GRP_VH',
        plugin_features   TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_FEATURES',
        pattern_validator TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_PATTRNVAL',
      END OF c_handlers.

    METHODS:
      register_search_value_helps
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry,
      register_code_search
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry,
      register_plugin_features
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry,
      register_other_value_helps
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.



CLASS zcl_adcoset_adt_disc_app IMPLEMENTATION.


  METHOD class_constructor.
    search_settings_uri = |{ c_static_uri }{ c_search_uri }{ c_settings_relative_uri }|.
    search_scope_uri = |{ c_static_uri }{ c_search_uri }{ c_search_scope_relative_uri }|.
  ENDMETHOD.


  METHOD fill_router.
    super->fill_router( CHANGING router = router ).
    router->attach(
      iv_template      = '/discovery'
      iv_handler_class = cl_adt_res_discovery=>co_class_name ).
  ENDMETHOD.


  METHOD get_application_title.
    result = 'ABAP Code Search Tools'.
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = c_static_uri.
  ENDMETHOD.


  METHOD register_code_search.

    DATA(search_collection) = registry->register_discoverable_resource(
      url             = c_search_uri
      handler_class   = c_handlers-search
      description     = 'ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'codesearch' ).

    DATA(template) = |{ c_search_uri }\{?{ zif_adcoset_c_global=>c_search_params-search_pattern }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-scope_id }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-scope_offset }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-max_objects }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-match_all_patterns }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-sequential_matching }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-use_regex }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-class_includes }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-fugr_includes }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-ignore_comment_lines }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-ignore_case }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-multi_line }*\}|.

    search_collection->register_disc_res_w_template(
      relation      = c_root_rel_scheme && c_search_uri
      template      = template
      handler_class = c_handlers-search ).

    registry->register_discoverable_resource(
      url             = |{ c_search_uri }{ c_settings_relative_uri }|
      handler_class   = c_handlers-search_settings
      description     = 'Settings for ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'codesearchSettings' ).

    registry->register_discoverable_resource(
      url             = |{ c_search_uri }{ c_search_scope_relative_uri }|
      handler_class   = c_handlers-search_scope
      description     = 'Search scope for ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'codesearchScope' ).

    registry->register_discoverable_resource(
      url             = |{ c_search_uri }{ c_pattern_validator_rel_uri }|
      handler_class   = c_handlers-pattern_validator
      description     = 'Pattern Validator for ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'patternValidator' ).
  ENDMETHOD.


  METHOD register_other_value_helps.
    registry->register_discoverable_resource(
      url             = '/servergroup'
      handler_class   = c_handlers-server_group_vh
      description     = 'Server group value help'
      category_scheme = c_root_scheme
      category_term   = 'servergroup' ).
  ENDMETHOD.


  METHOD register_plugin_features.
    registry->register_discoverable_resource(
      url             = '/pluginfeatures'
      handler_class   = c_handlers-plugin_features
      description     = 'Available Plugin features'
      category_scheme = c_root_scheme
      category_term   = 'pluginFeatures' ).
  ENDMETHOD.


  METHOD register_resources.
    CHECK zcl_adcoset_db_support_util=>is_db_supported( ).

    register_code_search( registry ).
    register_plugin_features( registry ).
    register_search_value_helps( registry ).
    register_other_value_helps( registry ).
  ENDMETHOD.


  METHOD register_search_value_helps.
    registry->register_discoverable_resource(
      url             = '/applcomp'
      handler_class   = c_handlers-appl_comp_vh
      description     = 'Application Component value help'
      category_scheme = c_root_scheme
      category_term   = 'applcomp' ).
  ENDMETHOD.
ENDCLASS.
