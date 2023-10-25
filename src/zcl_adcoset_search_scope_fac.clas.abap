"! <p class="shorttext synchronized">Factory for creating search scopes</p>
CLASS zcl_adcoset_search_scope_fac DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Creates scope whoose size is not known yet</p>
    CLASS-METHODS create_scope
      IMPORTING
        search_scope  TYPE zif_adcoset_ty_global=>ty_search_scope
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_search_scope.

    "! <p class="shorttext synchronized">Creates scope with a given list of objects</p>
    CLASS-METHODS create_final_scope
      IMPORTING
*        !objects      TYPE zif_adcoset_ty_global=>ty_tadir_objects
        !objects_new      TYPE zif_adcoset_ty_global=>ty_tadir_objects_new
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_search_scope.
ENDCLASS.


CLASS zcl_adcoset_search_scope_fac IMPLEMENTATION.
  METHOD create_final_scope.
*    result = NEW zcl_adcoset_search_scope_final( objects = objects ).
    result = NEW zcl_adcoset_search_scope_final( objects_new = objects_new ).
  ENDMETHOD.

  METHOD create_scope.
    result = NEW zcl_adcoset_search_scope( search_scope = search_scope ).
  ENDMETHOD.
ENDCLASS.
