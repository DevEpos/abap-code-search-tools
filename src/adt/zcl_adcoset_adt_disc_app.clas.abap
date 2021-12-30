"! <p class="shorttext synchronized" lang="en">Router for ABAP Code Search</p>
CLASS zcl_adcoset_adt_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      fill_router REDEFINITION,
      get_application_title REDEFINITION,
      register_resources REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_root_scheme     TYPE string VALUE 'http://www.devepos.com/adt/cst',
      c_root_rel_scheme TYPE string VALUE 'http://www.devepos.com/adt/relations/cst',
      c_search_uri      TYPE string VALUE '/codesearch',
      c_static_uri      TYPE string VALUE '/devepos/adt/cst',

      BEGIN OF c_handlers,
        search       TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_SEARCH',
        appl_comp_vh TYPE string VALUE 'ZCL_ADCOSET_ADT_RES_APPLC_VH',
      END OF c_handlers.

    METHODS:
      register_search_value_helps
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry,

      register_code_search
        IMPORTING
          registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.



CLASS zcl_adcoset_adt_disc_app IMPLEMENTATION.


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

    register_search_value_helps( registry ).
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
                     |\{&{ zif_adcoset_c_global=>c_search_params-max_results }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-object_name }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-object_type }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-owner }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-package }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-appl_comp }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-use_regex }*\}| &&
                     |\{&{ zif_adcoset_c_global=>c_search_params-prefer_pcre }*\}|.

    search_collection->register_disc_res_w_template(
      relation      = c_root_rel_scheme && c_search_uri
      template      = template
      handler_class = c_handlers-search ).

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
