"! <p class="shorttext synchronized" lang="en">Search Provider for Classes</p>
CLASS zcl_adcoset_csp_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of a class search provider</p>
      constructor
        IMPORTING
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          custom_settings TYPE zif_adcoset_ty_global=>ty_cls_search_settings
          src_code_reader TYPE REF TO zif_adcoset_src_code_reader
          matchers        TYPE REF TO zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_section_texts,
        main_source       TYPE string VALUE `Main Source`,
        locals_def        TYPE string VALUE `Local Class Definitions`,
        local_impl        TYPE string VALUE `Local Types`,
        macros            TYPE string VALUE `Macros`,
        test_cls          TYPE string VALUE `Test Class Definitions`,
        public_section    TYPE string VALUE 'Public Section',
        protected_section TYPE string VALUE 'Protected Section',
        private_section   TYPE string VALUE 'Private Section',
        method            TYPE string VALUE `Method `,
      END OF c_section_texts.

    TYPES:
      BEGIN OF ty_line_index,
        number TYPE i,
        offset TYPE i,
      END OF ty_line_index,
      ty_line_indexes TYPE TABLE OF ty_line_index WITH KEY number
                                                  WITH UNIQUE HASHED KEY offset COMPONENTS offset,
      BEGIN OF ty_source_code,
        code         TYPE string,
        line_indexes TYPE ty_line_indexes,
      END OF ty_source_code,

      BEGIN OF ty_method_pos,
        offset TYPE i,
        method TYPE seocpdname,
      END OF ty_method_pos.

    DATA:
      custom_settings TYPE zif_adcoset_ty_global=>ty_cls_search_settings,
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings,
      src_code_reader TYPE REF TO zif_adcoset_src_code_reader.
ENDCLASS.



CLASS zcl_adcoset_csp_clas IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->src_code_reader = src_code_reader.
    me->custom_settings = custom_settings.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.

  ENDMETHOD.


ENDCLASS.
