"! <p class="shorttext synchronized">Search scope which is already fully known</p>
CLASS zcl_adcoset_search_scope_final DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_scope.

    METHODS constructor
      IMPORTING
*        !objects TYPE zif_adcoset_ty_global=>ty_tadir_objects
        !objects_new TYPE zif_adcoset_ty_global=>ty_tadir_objects_new.

  PRIVATE SECTION.
*    DATA objects TYPE zif_adcoset_ty_global=>ty_tadir_objects.
    DATA objects_new TYPE zif_adcoset_ty_global=>ty_tadir_objects_new.
ENDCLASS.


CLASS zcl_adcoset_search_scope_final IMPLEMENTATION.
  METHOD constructor.
*    me->objects = objects.
    me->objects_new = objects_new.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~count.
*    result = lines( objects ).
    result = lines( objects_new ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~has_next_package.
*    result = xsdbool( objects IS NOT INITIAL ).
    result = xsdbool( objects_new IS NOT INITIAL ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
*    result = objects.
    result = objects_new.
*    CLEAR objects.
    CLEAR objects_new.
  ENDMETHOD.
ENDCLASS.
