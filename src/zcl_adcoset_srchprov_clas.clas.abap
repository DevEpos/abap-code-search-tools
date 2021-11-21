"! <p class="shorttext synchronized" lang="en">Search Provider for Classes</p>
CLASS zcl_adcoset_srchprov_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          search_settings     TYPE REF TO zif_adcoset_search_settings
          cls_search_settings TYPE REF TO zif_adcoset_cls_search_settngs,
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
    DATA:
      custom_settings TYPE REF TO zif_adcoset_cls_search_settngs,
      search_settings TYPE REF TO zif_adcoset_search_settings.
ENDCLASS.



CLASS zcl_adcoset_srchprov_clas IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->custom_settings = cls_search_settings.
  ENDMETHOD.


  METHOD search.

  ENDMETHOD.


ENDCLASS.
