"! <p class="shorttext synchronized" lang="en">Global constants for code search tools</p>
INTERFACE zif_adcoset_c_global
  PUBLIC.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Line feed types</p>
    BEGIN OF c_line_feed_type,
      lf    TYPE zif_adcoset_ty_global=>ty_line_feed_type VALUE 'lf',
      cr_lf TYPE zif_adcoset_ty_global=>ty_line_feed_type VALUE 'cr_lf',
    END OF c_line_feed_type,


    "! <p class="shorttext synchronized" lang="en">Search modes for main class include</p>
    BEGIN OF c_cls_main_search_modes,
      "! The main source exists of the following includes: <br>
      "! <ul>
      "!  <li>Public section</li>
      "!  <li>Protected section</li>
      "!  <li>Private section</li>
      "!  <li>Methods</li>
      "! </ul>
      "! These are all separately read and searched
      search_separate_includes TYPE zif_adcoset_ty_global=>ty_cls_main_incl_search_mode VALUE '1',
      "! Only the single include (suffix CS in REPOSRC table) will be searched. <br>
      "! <strong>Advantage</strong>: <br>
      "! Search will be faster as only a single include has to be fetched for the main
      "! source of a global class. <br>
      "! <strong>Disadvantage</strong>: <br>
      "! If classes are also edited in SE24/SE80 the search may not be accurate as the
      "! main source include won't be updated if a single method include gets changed
      search_only_main_source  TYPE zif_adcoset_ty_global=>ty_cls_main_incl_search_mode VALUE '2',
    END OF c_cls_main_search_modes,

    "! <p class="shorttext synchronized" lang="en">Technical identifier for source code types</p>
    BEGIN OF c_source_code_type,
      class                 TYPE trobjtype VALUE 'CLAS',
      interface             TYPE trobjtype VALUE 'INTF',
      data_definition       TYPE trobjtype VALUE 'DDLS',
      metadata_extension    TYPE trobjtype VALUE 'DDLX',
      access_control        TYPE trobjtype VALUE 'DCLS',
      behavior_definition   TYPE trobjtype VALUE 'BDEF',
      simple_transformation TYPE trobjtype VALUE 'XSLT',
      function_group        TYPE trobjtype VALUE 'FUGR',
      function_module       TYPE trobjtype VALUE 'FUNC',
    END OF c_source_code_type.
ENDINTERFACE.
