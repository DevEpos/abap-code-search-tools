"! <p class="shorttext synchronized">Global constants for code search tools</p>
INTERFACE zif_adcoset_c_global
  PUBLIC.

  "! Minium Objects in scope that are needed so parallel
  "! processing will be used.
  CONSTANTS c_parl_proc_min_objects TYPE i VALUE 100.

  "! Default scope expiration in seconds. <br/>
  "! A scope is therefore valid for the duration of the current search query
  CONSTANTS c_default_scope_expiration TYPE i VALUE 600.

  "! Minimum package size for a single ADT request with parallel processing enabled
  CONSTANTS c_parl_proc_min_pack_size TYPE i VALUE 500.

  "! Maximum package size for a single ADT request with parallel processing enabled
  CONSTANTS c_parl_proc_max_pack_size TYPE i VALUE 10000.

  "! Type for Server Group (see RZ12)
  CONSTANTS c_group_type_server_group TYPE rzlli_grpt VALUE 'S'.

  "! <p class="shorttext synchronized">Regex pattern to detect comment lines in CDS sources</p>
  "! Can be used for the following source types:
  "! <ul>
  "!   <li>DDLS</li>
  "!   <li>DCLS</li>
  "!   <li>DDLX</li>
  "! </ul>
  CONSTANTS c_cds_comment_regex TYPE string VALUE '^\s*(//|/\*|--)'.

  CONSTANTS:
    "! Id's for supported databases
    BEGIN OF c_dbsys,
      oracle TYPE sy-dbsys VALUE 'ORACLE',
      ms_sql TYPE sy-dbsys VALUE 'MSSQL',
      hana   TYPE sy-dbsys VALUE 'HDB',
    END OF c_dbsys.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Matcher types</p>
    BEGIN OF c_matcher_type,
      substring   TYPE zif_adcoset_ty_global=>ty_matcher_type VALUE '1',
      posix_regex TYPE zif_adcoset_ty_global=>ty_matcher_type VALUE '2',
      "! Perl compatible regular expression pattern
      pcre        TYPE zif_adcoset_ty_global=>ty_matcher_type VALUE '3',
    END OF c_matcher_type.

  CONSTANTS:
    "! Type of a message
    BEGIN OF c_message_type,
      info    TYPE string VALUE 'INFO',
      warning TYPE string VALUE 'WARNING',
      error   TYPE string VALUE 'ERROR',
    END OF c_message_type.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Names of code search parameters</p>
    BEGIN OF c_search_params,
      scope_id             TYPE string VALUE 'scopeId',
      scope_offset         TYPE string VALUE 'scopeOffset',
      package              TYPE string VALUE 'packageName',
      owner                TYPE string VALUE 'owner',
      use_regex            TYPE string VALUE 'useRegex',
      match_all_patterns   TYPE string VALUE 'matchAll',
      sequential_matching  TYPE string VALUE 'seqMatching',
      ignore_comment_lines TYPE string VALUE 'ignoreCommentLines',
      ignore_case          TYPE string VALUE 'ignoreCase',
      multi_line           TYPE string VALUE 'multiLine',
      appl_comp            TYPE string VALUE 'applComp',
      object_name          TYPE string VALUE 'objectName',
      object_type          TYPE string VALUE 'objectType',
      search_pattern       TYPE string VALUE 'searchPattern',
      created_date         TYPE string VALUE 'createdDate',
      class_includes       TYPE string VALUE 'classIncludes',
      fugr_includes        TYPE string VALUE 'fugrIncludes',
      expand_prog_includes TYPE string VALUE 'expandProgIncl',
      max_objects          TYPE string VALUE 'maxObjects',
      tag_id               TYPE string VALUE 'tagId',
      tr_request           TYPE string VALUE 'transportRequest',
    END OF c_search_params.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Include id's for class</p>
    BEGIN OF c_class_include_id,
      all                  TYPE string VALUE 'all',
      public_section       TYPE string VALUE 'pubSec',
      protected_section    TYPE string VALUE 'proSec',
      private_section      TYPE string VALUE 'priSec',
      methods              TYPE string VALUE 'methods',
      tests                TYPE string VALUE 'tests',
      macros               TYPE string VALUE 'macros',
      local_definitions    TYPE string VALUE 'localDef',
      local_implementation TYPE string VALUE 'localImpl',
    END OF c_class_include_id.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Include id's for function group</p>
    BEGIN OF c_fugr_include_id,
      all          TYPE string VALUE 'all',
      function     TYPE string VALUE 'func',
      non_function TYPE string VALUE 'nonFunc',
    END OF c_fugr_include_id.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Constants for needed TADIR types</p>
    BEGIN OF c_tadir_type,
      package TYPE trobjtype VALUE 'DEVC',
    END OF c_tadir_type.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Technical identifier for source code types</p>
    BEGIN OF c_source_code_type,
      class                 TYPE trobjtype VALUE 'CLAS',
      interface             TYPE trobjtype VALUE 'INTF',
      program               TYPE trobjtype VALUE 'PROG',
      type_group            TYPE trobjtype VALUE 'TYPE',
      data_definition       TYPE trobjtype VALUE 'DDLS',
      metadata_extension    TYPE trobjtype VALUE 'DDLX',
      access_control        TYPE trobjtype VALUE 'DCLS',
      behavior_definition   TYPE trobjtype VALUE 'BDEF',
      simple_transformation TYPE trobjtype VALUE 'XSLT',
      function_group        TYPE trobjtype VALUE 'FUGR',
    END OF c_source_code_type.

  CONSTANTS:
    BEGIN OF c_source_code_limu_type,
      function_module         TYPE trobjtype VALUE 'FUNC',
      method                  TYPE trobjtype VALUE 'METH',
      report_source_code      TYPE trobjtype VALUE 'REPS',
      class_definition        TYPE trobjtype VALUE 'CLSD',
      class_public_section    TYPE trobjtype VALUE 'CPUB',
      class_protected_section TYPE trobjtype VALUE 'CPRO',
      class_private_section   TYPE trobjtype VALUE 'CPRI',
      class_include           TYPE trobjtype VALUE 'CINC',
    END OF c_source_code_limu_type.

  CONSTANTS:
    BEGIN OF c_scope_type,
      universal_scope   TYPE zadcoset_scope_type VALUE 'US',
      transport_request TYPE zadcoset_scope_type VALUE 'TR',
    END OF c_scope_type.

  CONSTANTS:
    BEGIN OF c_program_id,
      r3tr TYPE pgmid VALUE 'R3TR',
      limu TYPE pgmid VALUE 'LIMU',
    END OF c_program_id.

  CONSTANTS:
    BEGIN OF c_trkorr_type_vh,
      workbench_request   TYPE string VALUE 'WORKBENCH_REQUEST',
      piece_list          TYPE string VALUE 'PIECE_LIST',
      relocation_request  TYPE string VALUE 'RELOCATION_REQUEST',
      repair_task         TYPE string VALUE 'REPAIR_TASK',
      dev_corr_task       TYPE string VALUE 'DEV_CORR_TASK',
      transport_of_copies TYPE string VALUE 'TRANSPORT_OF_COPIES',
    END OF c_trkorr_type_vh.
ENDINTERFACE.
