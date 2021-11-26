"! <p class="shorttext synchronized" lang="en">Search Provider for Classes</p>
CLASS zcl_adcoset_csp_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          matchers        TYPE REF TO zif_adcoset_pattern_matcher=>ty_ref_tab,
      search
        IMPORTING
          object TYPE zif_adcoset_ty_global=>ty_object.
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
        number               TYPE i,
        offset               TYPE i,
        method_name          TYPE seocpdname,
        previous_method_line TYPE i,
        is_class_def_end     TYPE abap_bool,
      END OF ty_line_index,
      ty_line_indexes TYPE TABLE OF ty_line_index WITH KEY number
                                                  WITH UNIQUE HASHED KEY offset COMPONENTS offset,
      BEGIN OF ty_source_code,
        code                    TYPE string,
        class_def_end_line      TYPE i,
        first_method_begin_line TYPE i,
        last_method_begin_line  TYPE i,
        line_indexes            TYPE ty_line_indexes,
      END OF ty_source_code,

      BEGIN OF ty_parse_settings,
        determine_class_def_begin TYPE abap_bool,
        determine_method_begin    TYPE abap_bool,
      END OF ty_parse_settings,

      BEGIN OF ty_method_pos,
        offset TYPE i,
        method TYPE seocpdname,
      END OF ty_method_pos.

    DATA:
      custom_settings TYPE zif_adcoset_ty_global=>ty_cls_search_settings,
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings.
ENDCLASS.



CLASS zcl_adcoset_csp_clas IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.

    ASSIGN search_settings-custom_settings->* TO FIELD-SYMBOL(<custom_settings>).
    IF sy-subrc = 0.
      custom_settings = CORRESPONDING #( <custom_settings> ).
    ENDIF.
  ENDMETHOD.


  METHOD search.

  ENDMETHOD.


ENDCLASS.
