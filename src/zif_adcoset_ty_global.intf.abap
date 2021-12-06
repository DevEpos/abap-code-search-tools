"! <p class="shorttext synchronized" lang="en">Global types for advanced code search</p>
INTERFACE zif_adcoset_ty_global
  PUBLIC .

  TYPES:
    ty_server_group              TYPE rzlli_apcl,
    ty_package_name              TYPE devclass,
    ty_package_names             TYPE STANDARD TABLE OF devclass WITH EMPTY KEY,
    ty_tadir_types               TYPE STANDARD TABLE OF trobjtype WITH EMPTY KEY,
    ty_obj_names                 TYPE STANDARD TABLE OF sobj_name WITH EMPTY KEY,
    ty_search_results            TYPE STANDARD TABLE OF REF TO zif_adcoset_search_result,
    ty_cls_main_incl_search_mode TYPE string,
    ty_matcher_type              TYPE c LENGTH 1,

    BEGIN OF ty_wb_obj_type,
      type     TYPE trobjtype,
      sub_type TYPE seu_objtyp,
    END OF ty_wb_obj_type,

    ty_wb_obj_types TYPE STANDARD TABLE OF ty_wb_obj_type WITH EMPTY KEY,

    BEGIN OF ty_object,
      name TYPE sobj_name,
      type TYPE ty_wb_obj_type,
    END OF ty_object,


    ty_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Settings for code based class search</p>
    BEGIN OF ty_cls_search_settings,
      search_main_incl       TYPE abap_bool,
      search_methods_incl    TYPE abap_bool,
      search_test_incl       TYPE abap_bool,
      search_macro_incl      TYPE abap_bool,
      search_local_def_incl  TYPE abap_bool,
      search_local_impl_incl TYPE abap_bool,
      main_incl_search_mode  TYPE ty_cls_main_incl_search_mode,
    END OF ty_cls_search_settings,

    BEGIN OF ty_search_settings,
      line_feed            TYPE string,
      ignore_comment_lines TYPE abap_bool,
      match_all_patterns   TYPE abap_bool,
      multiline_search     TYPE abap_bool,
      max_results          TYPE i,
      all_results          TYPE abap_bool,
    END OF ty_search_settings,

    "! <p class="shorttext synchronized" lang="en">Ranges for search scope</p>
    BEGIN OF ty_search_scope,
      package_range     TYPE RANGE OF devclass,
      object_type_range TYPE RANGE OF trobjtype,
      object_name_range TYPE RANGE OF sobj_name,
      owner_range       TYPE RANGE OF uname,
      created_on_range  TYPE RANGE OF tadir-created_on,
      appl_comp_range   TYPE RANGE OF df14l-ps_posid,
      max_objects       TYPE i,
    END OF ty_search_scope,

    BEGIN OF ty_match_identifier,
      object_name  TYPE sobj_name,
      object_type  TYPE wbobjtype,
      display_name TYPE string,
      main_include TYPE program,
      include      TYPE program,
    END OF ty_match_identifier.

  TYPES BEGIN OF ty_search_match.
  INCLUDE TYPE ty_match_identifier.
  TYPES:
    start_line   TYPE i,
    end_line     TYPE i,
    column_start TYPE i,
    column_end   TYPE i.
  TYPES END OF ty_search_match.

  TYPES ty_search_matches TYPE STANDARD TABLE OF ty_search_match WITH EMPTY KEY.

  "! <p class="shorttext synchronized" lang="en">General settings for code based search</p>
  TYPES BEGIN OF ty_search_settings_extended.
  INCLUDE TYPE ty_search_settings.
  TYPES:
    ignore_case  TYPE abap_bool,
    matcher_type TYPE ty_matcher_type,
    BEGIN OF parallel_processing,
      enabled      TYPE abap_bool,
      server_group TYPE ty_server_group,
    END OF parallel_processing,
    BEGIN OF custom_settings,
      class TYPE ty_cls_search_settings,
    END OF custom_settings,
    pattern_range TYPE RANGE OF string,
    search_scope  TYPE ty_search_scope.
  TYPES END OF ty_search_settings_extended.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Value range for search option</p>
    BEGIN OF ty_search_option_range,
      sign    TYPE ddsign,
      sign2   TYPE ddsign,
      option  TYPE ddoption,
      option2 TYPE ddoption,
      low     TYPE string,
      high    TYPE string,
    END OF ty_search_option_range,

    "! <p class="shorttext synchronized" lang="en">Table of option value ranges</p>
    ty_search_option_ranges TYPE STANDARD TABLE OF ty_search_option_range WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Represents search option with its values</p>
    BEGIN OF ty_search_option,
      option TYPE string,
      ranges TYPE ty_search_option_ranges,
    END OF ty_search_option,

    "! <p class="shorttext synchronized" lang="en">Table of search options</p>
    ty_search_options TYPE STANDARD TABLE OF ty_search_option WITH KEY option.

ENDINTERFACE.
