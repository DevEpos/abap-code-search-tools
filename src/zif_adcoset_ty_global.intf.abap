"! <p class="shorttext synchronized">Global types for advanced code search</p>
INTERFACE zif_adcoset_ty_global
  PUBLIC.

  TYPES ty_server_group TYPE rzlli_apcl.
  TYPES ty_package_name TYPE devclass.
  TYPES ty_package_names TYPE STANDARD TABLE OF devclass WITH EMPTY KEY.
  TYPES ty_package_name_range TYPE RANGE OF devclass.
  TYPES ty_tadir_types TYPE STANDARD TABLE OF trobjtype WITH EMPTY KEY.
  TYPES ty_obj_names TYPE STANDARD TABLE OF sobj_name WITH EMPTY KEY.
  TYPES ty_cls_main_incl_search_mode TYPE string.
  TYPES ty_matcher_type TYPE c LENGTH 1.
  "! type for control flags of a matcher
  TYPES ty_control_flags TYPE x LENGTH 4.
  TYPES ty_duration_in_s TYPE p LENGTH 15 DECIMALS 2.
  TYPES ty_duration_in_ms TYPE i.
  TYPES ty_duration_in_micros TYPE p LENGTH 12 DECIMALS 2.
  "! Type for DDLX Source name (not available on 7.40)
  TYPES ty_ddlxname TYPE progname.

  TYPES: BEGIN OF ty_message,
           type        TYPE string,
           content     TYPE string,
           occurrences TYPE i,
         END OF ty_message,

         ty_messages TYPE STANDARD TABLE OF ty_message WITH EMPTY KEY.

  TYPES: BEGIN OF ty_method_param_info,
           "! Name of a method parameter
           name        TYPE seocmpname,
           "! Type handle of the parameter
           type_handle TYPE REF TO cl_abap_datadescr,
         END OF ty_method_param_info.

  " <p class="shorttext synchronized" lang="en">Param definitions for parallel processing handler</p>
  TYPES: BEGIN OF ty_parallel_handler,
           "! Class name of the parallel handler
           classname    TYPE string,
           "! Method name of the parallel handler
           method       TYPE seocpdname,
           "! Information about input parameter
           input_param  TYPE ty_method_param_info,
           "! Information about output parameter
           output_param TYPE ty_method_param_info,
         END OF ty_parallel_handler.

  TYPES: BEGIN OF ty_tadir_object_info,
           name         TYPE sobj_name,
           type         TYPE trobjtype,
           owner        TYPE responsibl,
           package_name TYPE devclass,
         END OF ty_tadir_object_info.

  TYPES ty_tadir_object_infos TYPE STANDARD TABLE OF ty_tadir_object_info WITH EMPTY KEY.
  TYPES ty_tadir_object_infos_sorted TYPE SORTED TABLE OF ty_tadir_object_info WITH UNIQUE KEY type name.

  TYPES: BEGIN OF ty_tr_request_object,
           trkorr       TYPE trkorr,
           pgmid        TYPE pgmid,
           obj_type     TYPE trobjtype,
           obj_name     TYPE trobj_name,
           package_name TYPE devclass,
           owner        TYPE responsibl,
           created_date TYPE creationdt,
         END OF ty_tr_request_object,

         ty_tr_request_objects TYPE SORTED TABLE OF ty_tr_request_object WITH UNIQUE KEY pgmid obj_type obj_name.

  TYPES: BEGIN OF ty_object,
           name TYPE sobj_name,
           type TYPE wbobjtype,
         END OF ty_object,

         ty_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Ranges for determining the search scope</p>
    BEGIN OF ty_search_scope_ranges,
      package_range     TYPE ty_package_name_range,
      object_type_range TYPE RANGE OF trobjtype,
      object_name_range TYPE RANGE OF sobj_name,
      owner_range       TYPE RANGE OF uname,
      created_on_range  TYPE RANGE OF tadir-created_on,
      appl_comp_range   TYPE RANGE OF df14l-ps_posid,
      tag_id_range      TYPE RANGE OF sysuuid_x16,
      tr_request_range  TYPE RANGE OF trkorr,
    END OF ty_search_scope_ranges.

  "! <p class="shorttext synchronized">tadir object with corresponding subobjects</p>
  TYPES BEGIN OF ty_tadir_object.
          INCLUDE TYPE ty_tadir_object_info AS info.
  TYPES   subobjects TYPE ty_tadir_object_infos_sorted.
  TYPES END OF ty_tadir_object.

  "! <p class="shorttext synchronized">Table of tadir object with corresponding subobjects</p>
  TYPES ty_tadir_objects TYPE STANDARD TABLE OF ty_tadir_object WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_scope_package,
      count   TYPE i,
      objects TYPE zif_adcoset_ty_global=>ty_tadir_objects,
    END OF ty_scope_package.

  "! <p class="shorttext synchronized">Ranges / data to define an object scope</p>
  TYPES BEGIN OF ty_search_scope.
  TYPES   scope_id       TYPE sysuuid_x16.
  TYPES   current_offset TYPE i.
          INCLUDE        TYPE ty_search_scope_ranges AS ranges.
  TYPES   max_objects    TYPE i.
  TYPES END OF ty_search_scope.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Uniquely identifies a match</p>
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
    END OF ty_match_identifier.

  TYPES BEGIN OF ty_search_match.
          INCLUDE TYPE ty_match_identifier.
  TYPES   start_line   TYPE i.
  TYPES   start_column TYPE i.
  TYPES   end_line     TYPE i.
  TYPES   end_column   TYPE i.
  TYPES   snippet      TYPE string.
  TYPES   long_snippet TYPE string.
  TYPES END OF ty_search_match.

  TYPES ty_search_matches TYPE STANDARD TABLE OF ty_search_match WITH EMPTY KEY.

  TYPES: BEGIN OF ty_search_result_object,
           object       TYPE ty_tadir_object_info,
           text_matches TYPE ty_search_matches,
           match_count  TYPE i,
         END OF ty_search_result_object,

         ty_search_result_objects TYPE SORTED TABLE OF ty_search_result_object WITH UNIQUE KEY object-name object-type.

  TYPES:
    "! Code search result
    BEGIN OF ty_search_result,
      results                TYPE ty_search_result_objects,
      searched_objects_count TYPE i,
      searched_sources_count TYPE i,
      loc                    TYPE f,
      messages               TYPE ty_messages,
      duration_in_ms         TYPE ty_duration_in_ms,
    END OF ty_search_result.

  TYPES:
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
    END OF ty_clas_cs_settings.

  TYPES:
    "! Function group settings for code search
    BEGIN OF ty_fugr_cs_settings,
      "! Indicates which includes of a function group shall be searched
      BEGIN OF include_flags,
        function     TYPE abap_bool,
        non_function TYPE abap_bool,
      END OF include_flags,
    END OF ty_fugr_cs_settings.

  TYPES:
    "! Program settings for code search
    BEGIN OF ty_prog_cs_settings,
      expand_includes TYPE abap_bool,
    END OF ty_prog_cs_settings.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Basic search settings</p>
    BEGIN OF ty_search_settings,
      line_feed             TYPE string,
      ignore_comment_lines  TYPE abap_bool,
      match_all_patterns    TYPE abap_bool,
      sequential_matching   TYPE abap_bool,
      check_sequence_bounds TYPE abap_bool,
      multiline_search      TYPE abap_bool,
    END OF ty_search_settings.

  TYPES:
    "! Custom code search settings for some object types
    BEGIN OF ty_custom_search_settings,
      class TYPE ty_clas_cs_settings,
      fugr  TYPE ty_fugr_cs_settings,
      prog  TYPE ty_prog_cs_settings,
    END OF ty_custom_search_settings.

  TYPES: BEGIN OF ty_parl_processing,
           enabled      TYPE abap_bool,
           server_group TYPE ty_server_group,
         END OF ty_parl_processing.

  TYPES:
    "! Settings for PCRE Regex
    BEGIN OF ty_pcre_regex_settings,
      "! Extended mode means all whitespaces have to be escaped to be
      "! recognized as whitespaces
      extended_mode_disabled   TYPE abap_bool,
      "! Single line mode tells the RegEx engine that the '.' character should
      "! match line break sequences as well (i.e. \r, \n, \r\n)
      single_line_mode_enabled TYPE abap_bool,
    END OF ty_pcre_regex_settings.

  TYPES:
    "! Pattern for search
    BEGIN OF ty_pattern,
      content TYPE string,
      flags   TYPE ty_control_flags,
    END OF ty_pattern,

    "! Table of patterns for search
    ty_patterns TYPE STANDARD TABLE OF ty_pattern WITH EMPTY KEY.

  TYPES: BEGIN OF ty_pattern_config,
           ignore_case   TYPE abap_bool,
           matcher_type  TYPE ty_matcher_type,
           patterns      TYPE ty_patterns,
           pcre_settings TYPE ty_pcre_regex_settings,
         END OF ty_pattern_config.

  "! <p class="shorttext synchronized">Internal code search settings</p>
  TYPES BEGIN OF ty_search_settings_int.
          INCLUDE TYPE ty_search_settings AS basic_settings.
          INCLUDE TYPE ty_pattern_config AS pattern_config.
  TYPES   custom_settings TYPE ty_custom_search_settings.
  TYPES   is_adt          TYPE abap_bool.
  TYPES END OF ty_search_settings_int.

  "! <p class="shorttext synchronized">External settings for code search API</p>
  TYPES BEGIN OF ty_search_settings_external.
          INCLUDE TYPE ty_search_settings_int AS internal_settings.
  TYPES   parallel_processing TYPE ty_parl_processing.
  TYPES   search_scope        TYPE ty_search_scope.
  TYPES END OF ty_search_settings_external.

  "! <p class="shorttext synchronized">Defines search package for parallel search</p>
  TYPES BEGIN OF ty_search_package.
          INCLUDE TYPE ty_search_settings_int AS settings.
  TYPES   scope_package TYPE ty_scope_package.
  TYPES END OF ty_search_package.

  TYPES: BEGIN OF ty_search_package_result,
           result_objects         TYPE ty_search_result_objects,
           searched_objects_count TYPE i,
           searched_sources_count TYPE i,
           loc                    TYPE f,
           messages               TYPE ty_messages,
         END OF ty_search_package_result.

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
    ty_search_option_ranges TYPE STANDARD TABLE OF ty_search_option_range WITH EMPTY KEY.

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Represents search option with its values</p>
    BEGIN OF ty_search_option,
      option TYPE string,
      ranges TYPE ty_search_option_ranges,
    END OF ty_search_option,

    "! <p class="shorttext synchronized" lang="en">Table of search options</p>
    ty_search_options TYPE STANDARD TABLE OF ty_search_option WITH KEY option.

ENDINTERFACE.
