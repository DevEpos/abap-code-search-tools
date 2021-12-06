"! <p class="shorttext synchronized" lang="en">Resource for code search</p>
CLASS zcl_adcoset_adt_res_search DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_matcher_type
        IMPORTING
          use_regex     TYPE abap_bool
          use_pcre      TYPE abap_bool
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_matcher_type.
ENDCLASS.



CLASS zcl_adcoset_adt_res_search IMPLEMENTATION.


  METHOD get.

  ENDMETHOD.


  METHOD get_matcher_type.

    IF use_pcre = abap_true.
      " TODO: some notify use that PCRE is not supported in the system??
      IF zcl_adcoset_matcher_factory=>is_pcre_supported( ).
        result = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSEIF use_regex = abap_true.
      result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
    ELSE.
      result = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
