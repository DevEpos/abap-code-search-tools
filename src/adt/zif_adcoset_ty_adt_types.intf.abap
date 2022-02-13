INTERFACE zif_adcoset_ty_adt_types
  PUBLIC.

  TYPES:
    BEGIN OF ty_adt_obj_ref,
      "! URI - mandatory client response, optional for client request
      uri          TYPE string,
      "! Name of the referenced entity - optional
      name         TYPE string,
      "! Description of the referenced entity - optional
      description  TYPE string,
      "! ADT Type of the referenced entity - optional
      type         TYPE string,
      "! Package name of the referenced entity- optional
      package_name TYPE string,
      "! Owner of the referenced entity - optional
      owner        TYPE string,
    END OF ty_adt_obj_ref,

    BEGIN OF ty_code_search_match,
      uri     TYPE string,
      snippet TYPE string,
    END OF ty_code_search_match,

    ty_code_search_matches TYPE STANDARD TABLE OF ty_code_search_match WITH EMPTY KEY,

    BEGIN OF ty_code_search_object,
      uri             TYPE string,
      parent_uri      TYPE string,
      adt_main_object TYPE if_adt_tools_core_types=>ty_main_object,
      matches         TYPE ty_code_search_matches,
    END OF ty_code_search_object,

    ty_code_search_objects TYPE STANDARD TABLE OF ty_code_search_object WITH EMPTY KEY,

    BEGIN OF ty_code_search_result,
      code_search_objects        TYPE ty_code_search_objects,
      messages                   TYPE zif_adcoset_ty_global=>ty_messages,
      number_of_results          TYPE i,
      number_of_searched_objects TYPE i,
      number_of_searched_sources TYPE i,
      query_time_in_ms           TYPE i,
    END OF ty_code_search_result,

    "! Describes a feature of an ADT plugin
    BEGIN OF ty_adt_plugin_feature,
      "! Name of a feature.
      name        TYPE string,
      "! URI endpoint where feature is used
      endpoint    TYPE string,
      "! The type of the feature. <br>
      "! e.g. Boolean, String, List
      type        TYPE string,
      "! Indicates if the feature is enabled or not
      enabled     TYPE abap_bool,
      "! Category of the feature. <br>
      "! e.g. Attribute, Parameter
      category    TYPE string,
      "! Optional description of the feature
      description TYPE string,
    END OF ty_adt_plugin_feature,

    "! List of ADT plugin features
    ty_adt_plugin_features TYPE STANDARD TABLE OF ty_adt_plugin_feature WITH EMPTY KEY,

    "! Code Search scope Parameter
    BEGIN OF ty_search_scope_param,
      name  TYPE string,
      value TYPE string,
    END OF ty_search_scope_param,

    "! List of Code Search Scope Parameters
    ty_search_scope_params TYPE STANDARD TABLE OF ty_search_scope_param WITH EMPTY KEY,

    "! Scope for code search
    BEGIN OF ty_search_scope,
      id           TYPE sysuuid_x16,
      object_count TYPE i,
    END OF ty_search_scope.

  "! Settings for Code Search
  TYPES BEGIN OF ty_code_search_settings.
  INCLUDE TYPE zadcoset_csset.
  TYPES END OF ty_code_search_settings.

ENDINTERFACE.
