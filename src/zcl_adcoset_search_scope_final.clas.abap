"! <p class="shorttext synchronized">Search scope which is already fully known</p>
CLASS zcl_adcoset_search_scope_final DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_scope.

    METHODS constructor
      IMPORTING
        !package TYPE zif_adcoset_ty_global=>ty_scope_package.

  PRIVATE SECTION.
    DATA objects TYPE zif_adcoset_ty_global=>ty_scope_package.
ENDCLASS.


CLASS zcl_adcoset_search_scope_final IMPLEMENTATION.
  METHOD constructor.
    objects = objects.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~count.
    result = objects-count.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~has_next_package.
    result = xsdbool( objects-objects IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    result = objects.
    CLEAR objects.
  ENDMETHOD.
ENDCLASS.
