"! <p class="shorttext synchronized" lang="en">Router for ABAP Code Search</p>
CLASS zcl_adcoset_adt_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA:
      "! URI for search settings resource
      search_settings_uri TYPE string READ-ONLY.

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
      c_search_uri            TYPE string VALUE '/codesearch',
      c_settings_relative_uri TYPE string VALUE '/settings',
      c_static_uri            TYPE string VALUE '/devepos/adt/codesearchtools',
      c_root_scheme           TYPE string VALUE 'http://www.devepos.com/adt/codesearchtools',
      c_root_rel_scheme       TYPE string VALUE 'http://www.devepos.com/adt/relations/codesearchtools',

      "! REST handler constants
      BEGIN OF c_handlers,
        search          TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_SEARCH',
        search_settings TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_CS_CONFIG',
        appl_comp_vh    TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_APPLC_VH',
        server_group_vh TYPE string VALUE 'ZCL_ADCOSET_RES_SERVER_GRP_VH',
        plugin_features TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_FEATURES',
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


  METHOD register_resources.
    register_code_search( registry ).
    register_plugin_features( registry ).
    register_search_value_helps( registry ).
    register_other_value_helps( registry ).
  ENDMETHOD.


  METHOD register_code_search.

    DATA(search_collection) = registry->register_discoverable_resource(
      url             = c_search_uri
      handler_class   = c_handlers-search
      description     = 'ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'codesearch' ).

    DATA(template) = |{ c_search_uri }\{?{ zif_adcoset_c_global=>c_search_params-search_pattern }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-max_objects }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-read_package_hierarchy }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-match_all_patterns }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-max_results }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-all_results }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-object_name }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-object_type }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-owner }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-package }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-appl_comp }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-created_date }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-use_regex }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-class_search_scope }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-ignore_comment_lines }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-ignore_case }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-multi_line }*\}|.

    search_collection->register_disc_res_w_template(
      relation      = c_root_rel_scheme && c_search_uri
      template      = template
      handler_class = c_handlers-search ).

    " register resource for search settings
    registry->register_discoverable_resource(
      url             = |{ c_search_uri }{ c_settings_relative_uri }|
      handler_class   = c_handlers-search_settings
      description     = 'Settings for ABAP Code Search'
      category_scheme = c_root_scheme
      category_term   = 'codesearchSettings' ).
  ENDMETHOD.


  METHOD register_search_value_helps.
    registry->register_discoverable_resource(
      url             = '/applcomp'
      handler_class   = c_handlers-appl_comp_vh
      description     = 'Application Component value help'
      category_scheme = c_root_scheme
      category_term   = 'applcomp' ).
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


ENDCLASS.
