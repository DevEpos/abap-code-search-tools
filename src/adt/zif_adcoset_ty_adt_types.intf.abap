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
      adt_main_obj_ref TYPE ty_adt_obj_ref,
      adt_obj_ref      TYPE ty_adt_obj_ref,
      matches          TYPE ty_code_search_matches,
    END OF ty_code_search_object,

    ty_code_search_objects TYPE STANDARD TABLE OF ty_code_search_object WITH EMPTY KEY,

    BEGIN OF ty_code_search_result,
      code_search_objects        TYPE ty_code_search_objects,
      number_of_results          TYPE i,
      number_of_searched_objects TYPE i,
      query_time_millis          TYPE i,
    END OF ty_code_search_result.

ENDINTERFACE.
