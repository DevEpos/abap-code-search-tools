"! <p class="shorttext synchronized" lang="en">Factory for creating search scopes</p>
CLASS zcl_adcoset_search_scope_fac DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_scope
        IMPORTING
          search_scope  TYPE zif_adcoset_ty_global=>ty_search_scope
          parallel_mode TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_search_scope,

      create_final_scope
        IMPORTING
          objects       TYPE zif_adcoset_ty_global=>ty_tadir_objects
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_search_scope.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_search_scope_fac IMPLEMENTATION.


  METHOD create_final_scope.
    result = NEW zcl_adcoset_search_scope_final( objects = objects ).
  ENDMETHOD.


  METHOD create_scope.
    result = NEW zcl_adcoset_search_scope(
      search_scope  = search_scope
      parallel_mode = parallel_mode ).
  ENDMETHOD.

ENDCLASS.
