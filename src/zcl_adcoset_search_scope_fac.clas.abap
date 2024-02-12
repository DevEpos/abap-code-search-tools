"! <p class="shorttext synchronized">Factory for creating search scopes</p>
CLASS zcl_adcoset_search_scope_fac DEFINITION
  PUBLIC FINAL
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
        !package      TYPE zif_adcoset_ty_global=>ty_scope_package
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_search_scope.

  PRIVATE SECTION.
    CLASS-METHODS read_scope_type
      IMPORTING
        scope_id      TYPE sysuuid_x16
      RETURNING
        VALUE(result) TYPE zadcoset_scope_type.
ENDCLASS.


CLASS zcl_adcoset_search_scope_fac IMPLEMENTATION.
  METHOD create_final_scope.
    result = NEW zcl_adcoset_search_scope_final( package = package ).
  ENDMETHOD.

  METHOD create_scope.
    IF search_scope-scope_id IS NOT INITIAL.
      DATA(scope_type) = read_scope_type( search_scope-scope_id ).
    ENDIF.

    result = COND #( WHEN search_scope-tr_request_range IS NOT INITIAL
                       OR scope_type = zif_adcoset_c_global=>c_scope_type-transport_request
                     THEN NEW zcl_adcoset_search_scope_tr( search_scope = search_scope )
                     ELSE NEW zcl_adcoset_search_scope( search_scope = search_scope ) ).
  ENDMETHOD.

  METHOD read_scope_type.
    SELECT SINGLE scope_type FROM zadcoset_csscope
      WHERE id = @scope_id
      INTO @result.
  ENDMETHOD.
ENDCLASS.
