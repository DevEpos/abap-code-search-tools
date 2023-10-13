"! <p class="shorttext synchronized">Access to search settings stored on the server</p>
CLASS zcl_adcoset_search_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Retrieves server side code search settings</p>
    CLASS-METHODS get_settings
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_adt_types=>ty_code_search_settings.
ENDCLASS.


CLASS zcl_adcoset_search_settings IMPLEMENTATION.
  METHOD get_settings.
    " as the default settings correspond to an empty entry sy-subrc <> 0 does
    " have to be handled
    SELECT SINGLE *
      FROM zadcoset_csset
      WHERE uname = @sy-uname
      INTO CORRESPONDING FIELDS OF @result.

    IF sy-subrc <> 0.
      result = VALUE #( parallel_proc_pack_size = zif_adcoset_c_global=>c_parl_proc_min_pack_size ).
    ELSEIF result-parallel_proc_pack_size < zif_adcoset_c_global=>c_parl_proc_min_pack_size.
      result-parallel_proc_pack_size = zif_adcoset_c_global=>c_parl_proc_min_pack_size.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
