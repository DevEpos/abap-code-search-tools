"! <p class="shorttext synchronized">Global types for advanced code search</p>
interface ZIF_ADCOSET_TY_GLOBAL
  public .


  types TY_SERVER_GROUP type RZLLI_APCL .
  types TY_PACKAGE_NAME type DEVCLASS .
  types:
    ty_package_names TYPE STANDARD TABLE OF devclass WITH EMPTY KEY .
  types:
    ty_package_name_range TYPE RANGE OF devclass .
  types:
    ty_tadir_types TYPE STANDARD TABLE OF trobjtype WITH EMPTY KEY .
  types:
    ty_obj_names TYPE STANDARD TABLE OF sobj_name WITH EMPTY KEY .
  types TY_CLS_MAIN_INCL_SEARCH_MODE type STRING .
  types:
    ty_matcher_type TYPE c LENGTH 1 .
  types:
  "! type for control flags of a matcher
    ty_control_flags TYPE x LENGTH 4 .
  types:
    ty_duration_in_s TYPE p LENGTH 15 DECIMALS 2 .
  types TY_DURATION_IN_MS type I .
  types:
    ty_duration_in_micros TYPE p LENGTH 12 DECIMALS 2 .
  "! Type for DDLX Source name (not available on 7.40)
  types TY_DDLXNAME type PROGNAME .
  types:
    BEGIN OF ty_message,
      type        TYPE string,
      content     TYPE string,
      occurrences TYPE i,
    END OF ty_message .
  types:
    ty_messages TYPE STANDARD TABLE OF ty_message WITH EMPTY KEY .
  types:
    BEGIN OF ty_method_param_info,
      "! Name of a method parameter
      name        TYPE seocmpname,
      "! Type handle of the parameter
      type_handle TYPE REF TO cl_abap_datadescr,
    END OF ty_method_param_info .
  types:
    "! Param definitions for parallel processing handler
    BEGIN OF ty_parallel_handler,
      "! Class name of the parallel handler
      classname    TYPE string,
      "! Method name of the parallel handler
      method       TYPE seocpdname,
      "! Information about input parameter
      input_param  TYPE ty_method_param_info,
      "! Information about output parameter
      output_param TYPE ty_method_param_info,
    END OF ty_parallel_handler .
  types:
    BEGIN OF ty_tadir_object_info,
      name         TYPE sobj_name,
      type         TYPE trobjtype,
      owner        TYPE responsibl,
      package_name TYPE devclass,
    END OF ty_tadir_object_info .
  types:
    ty_tadir_object_infos TYPE STANDARD TABLE OF ty_tadir_object_info WITH EMPTY KEY .
  types:
    ty_tadir_object_infos_sorted TYPE SORTED TABLE OF ty_tadir_object_info WITH UNIQUE KEY type name .
  types:
    BEGIN OF ty_tr_request_object,
      pgmid        TYPE pgmid,
      obj_type     TYPE trobjtype,
      obj_name     TYPE trobj_name,
      package_name TYPE devclass,
      owner        TYPE responsibl,
      created_date TYPE creationdt,
    END OF ty_tr_request_object .
  types:
    ty_tr_request_objects TYPE SORTED TABLE OF ty_tr_request_object WITH UNIQUE KEY pgmid obj_type obj_name .
  types:
    BEGIN OF ty_object,
      name TYPE sobj_name,
      type TYPE wbobjtype,
    END OF ty_object .
  types:
    ty_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY .
  types:
    "! Ranges for determining the search scope
    BEGIN OF ty_search_scope_ranges,
      package_range     TYPE ty_package_name_range,
      object_type_range TYPE RANGE OF trobjtype,
      object_name_range TYPE RANGE OF sobj_name,
      owner_range       TYPE RANGE OF uname,
      created_on_range  TYPE RANGE OF tadir-created_on,
      appl_comp_range   TYPE RANGE OF df14l-ps_posid,
      tag_id_range      TYPE RANGE OF sysuuid_x16,
      tr_request_range  TYPE RANGE OF trkorr,
    END OF ty_search_scope_ranges .
  types:
  "! Tadir object with corresponding subobjects
    BEGIN OF ty_tadir_object.
          INCLUDE TYPE ty_tadir_object_info AS info.
  TYPES   subobjects TYPE ty_tadir_object_infos_sorted.
  TYPES END OF ty_tadir_object .
  types:
  "! Table of tadir object with corresponding subobjects
    ty_tadir_objects TYPE STANDARD TABLE OF ty_tadir_object WITH EMPTY KEY .
  types:
    BEGIN OF ty_scope_package,
      count   TYPE i,
      objects TYPE zif_adcoset_ty_global=>ty_tadir_objects,
    END OF ty_scope_package .
  types:
  "! Ranges / data to define an object scope
    BEGIN OF ty_search_scope.
  TYPES   scope_id       TYPE sysuuid_x16.
  TYPES   current_offset TYPE i.
          INCLUDE        TYPE ty_search_scope_ranges AS ranges.
  TYPES   max_objects    TYPE i.
  TYPES END OF ty_search_scope .
  types:
    "! Uniquely identifies a match
    BEGIN OF ty_match_identifier,
      display_name     TYPE string,
      main_include     TYPE program,
      include          TYPE program,
      "! ADT type for the include - if the include is filled<br>
      "! <br>
      "! Reason:<br>
      "! the uri mapper does not create the most suitable type which will be used
      "! to fetch the right image in ADT, so we write the correct one in this
      "! component
      adt_include_type TYPE string,
    END OF ty_match_identifier .
  types:
    BEGIN OF ty_search_match.
          INCLUDE TYPE ty_match_identifier.
  TYPES   start_line   TYPE i.
  TYPES   start_column TYPE i.
  TYPES   end_line     TYPE i.
  TYPES   end_column   TYPE i.
  TYPES   snippet      TYPE string.
  TYPES   long_snippet TYPE string.
  TYPES END OF ty_search_match .
  types:
    ty_search_matches TYPE STANDARD TABLE OF ty_search_match WITH EMPTY KEY .
  types:
    BEGIN OF ty_search_result_object,
      object       TYPE ty_tadir_object_info,
      text_matches TYPE ty_search_matches,
      match_count  TYPE i,
    END OF ty_search_result_object .
  types:
    ty_search_result_objects TYPE SORTED TABLE OF ty_search_result_object WITH UNIQUE KEY object-name object-type .
  types:
    "! Code search result
    BEGIN OF ty_search_result,
      results                TYPE ty_search_result_objects,
      searched_objects_count TYPE i,
      searched_sources_count TYPE i,
      loc                    TYPE f,
      messages               TYPE ty_messages,
      duration_in_ms         TYPE ty_duration_in_ms,
    END OF ty_search_result .
  types:
    "! Class settings for code search
    BEGIN OF ty_clas_cs_settings,
      "! Indicates which includes of a class shall be searched
      BEGIN OF include_flags,
        public_section    TYPE abap_bool,
        protected_section TYPE abap_bool,
        private_section   TYPE abap_bool,
        methods           TYPE abap_bool,
        main              TYPE abap_bool,
        test              TYPE abap_bool,
        macro             TYPE abap_bool,
        local_def         TYPE abap_bool,
        local_impl        TYPE abap_bool,
      END OF include_flags,
    END OF ty_clas_cs_settings .
  types:
    "! Function group settings for code search
    BEGIN OF ty_fugr_cs_settings,
      "! Indicates which includes of a function group shall be searched
      BEGIN OF include_flags,
        function     TYPE abap_bool,
        non_function TYPE abap_bool,
      END OF include_flags,
    END OF ty_fugr_cs_settings .
  types:
    "! Program settings for code search
    BEGIN OF ty_prog_cs_settings,
      expand_includes TYPE abap_bool,
    END OF ty_prog_cs_settings .
  types:
    "! Basic search settings
    BEGIN OF ty_search_settings,
      line_feed             TYPE string,
      ignore_comment_lines  TYPE abap_bool,
      match_all_patterns    TYPE abap_bool,
      sequential_matching   TYPE abap_bool,
      check_sequence_bounds TYPE abap_bool,
      multiline_search      TYPE abap_bool,
    END OF ty_search_settings .
  types:
    "! Custom code search settings for some object types
    BEGIN OF ty_custom_search_settings,
      class TYPE ty_clas_cs_settings,
      fugr  TYPE ty_fugr_cs_settings,
      prog  TYPE ty_prog_cs_settings,
    END OF ty_custom_search_settings .
  types:
    BEGIN OF ty_parl_processing,
           enabled      TYPE abap_bool,
           server_group TYPE ty_server_group,
         END OF ty_parl_processing .
  types:
    "! Settings for PCRE Regex
    BEGIN OF ty_pcre_regex_settings,
      "! Extended mode means all whitespaces have to be escaped to be
      "! recognized as whitespaces
      extended_mode_disabled   TYPE abap_bool,
      "! Single line mode tells the RegEx engine that the '.' character should
      "! match line break sequences as well (i.e. \r, \n, \r\n)
      single_line_mode_enabled TYPE abap_bool,
    END OF ty_pcre_regex_settings .
  types:
    "! Pattern for search
    BEGIN OF ty_pattern,
      content TYPE string,
      flags   TYPE ty_control_flags,
    END OF ty_pattern .
  types:
    "! Table of patterns for search
    ty_patterns TYPE STANDARD TABLE OF ty_pattern WITH EMPTY KEY .
  types:
    BEGIN OF ty_pattern_config,
      ignore_case   TYPE abap_bool,
      matcher_type  TYPE ty_matcher_type,
      patterns      TYPE ty_patterns,
      pcre_settings TYPE ty_pcre_regex_settings,
    END OF ty_pattern_config .
  types:
  "! Internal code search settings
    BEGIN OF ty_search_settings_int.
          INCLUDE TYPE ty_search_settings AS basic_settings.
          INCLUDE TYPE ty_pattern_config AS pattern_config.
  TYPES   custom_settings TYPE ty_custom_search_settings.
  TYPES   is_adt          TYPE abap_bool.
  TYPES END OF ty_search_settings_int .
  types:
  "! External settings for code search API
    BEGIN OF ty_search_settings_external.
          INCLUDE TYPE ty_search_settings_int AS internal_settings.
  TYPES   parallel_processing TYPE ty_parl_processing.
  TYPES   search_scope        TYPE ty_search_scope.
  TYPES END OF ty_search_settings_external .
  types:
  "! Defines search package for parallel search
    BEGIN OF ty_search_package.
          INCLUDE TYPE ty_search_settings_int AS settings.
  TYPES   scope_package TYPE ty_scope_package.
  TYPES END OF ty_search_package .
  types:
    BEGIN OF ty_search_package_result,
      result_objects         TYPE ty_search_result_objects,
      searched_objects_count TYPE i,
      searched_sources_count TYPE i,
      loc                    TYPE f,
      messages               TYPE ty_messages,
    END OF ty_search_package_result .
  types:
    "! Value range for search option
    BEGIN OF ty_search_option_range,
      sign    TYPE ddsign,
      sign2   TYPE ddsign,
      option  TYPE ddoption,
      option2 TYPE ddoption,
      low     TYPE string,
      high    TYPE string,
    END OF ty_search_option_range .
  types:
    "! Table of option value ranges
    ty_search_option_ranges TYPE STANDARD TABLE OF ty_search_option_range WITH EMPTY KEY .
  types:
    "! Represents search option with its values
    BEGIN OF ty_search_option,
      option TYPE string,
      ranges TYPE ty_search_option_ranges,
    END OF ty_search_option .
  types:
    "! Table of search options
    ty_search_options TYPE STANDARD TABLE OF ty_search_option WITH KEY option .
endinterface.
