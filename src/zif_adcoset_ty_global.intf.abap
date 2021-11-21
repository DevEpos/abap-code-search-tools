"! <p class="shorttext synchronized" lang="en">Global types for advanced code search</p>
INTERFACE zif_adcoset_ty_global
  PUBLIC .

  TYPES:
    ty_server_group   TYPE rzlli_apcl,
    ty_package_name   TYPE devclass,
    ty_package_names  TYPE STANDARD TABLE OF devclass WITH EMPTY KEY,
    ty_tadir_types    TYPE STANDARD TABLE OF trobjtype WITH EMPTY KEY,
    ty_obj_names      TYPE STANDARD TABLE OF sobj_name WITH EMPTY KEY,
    ty_search_results TYPE STANDARD TABLE OF REF TO zif_adcoset_search_result,

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

    "! General settings for code based search
    BEGIN OF ty_search_settings,
      use_regex             TYPE abap_bool,
      multiline_search      TYPE abap_bool,
      ignore_comment_lines  TYPE abap_bool,
      match_all_patterns    TYPE abap_bool,
      new_line_character    TYPE string,
      parallel_processing   TYPE abap_bool,
      parl_processing_group TYPE ty_server_group,
    END OF ty_search_settings,

    "! Settings for code based class search
    BEGIN OF ty_cls_search_settings,
      serach_test_incl       TYPE abap_bool,
      search_macro_incl      TYPE abap_bool,
      search_local_def_incl  TYPE abap_bool,
      search_local_impl_incl TYPE abap_bool,
    END OF ty_cls_search_settings.
ENDINTERFACE.
