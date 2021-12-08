"! <p class="shorttext synchronized" lang="en">Search scope implementation</p>
CLASS zcl_adcoset_search_scope DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_adcoset_search_scope.

    METHODS:
      constructor
        IMPORTING
          search_scope TYPE zif_adcoset_ty_global=>ty_search_scope
          parallel_mode type abap_bool optional.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      search_scope TYPE zif_adcoset_ty_global=>ty_search_scope,
      object_count TYPE i,
      parallel_mode type abap_bool.

    METHODS:
      determine_count.
ENDCLASS.



CLASS zcl_adcoset_search_scope IMPLEMENTATION.


  METHOD constructor.
    me->search_scope = search_scope.
    me->parallel_mode = parallel_mode.
    determine_count( ).
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~count.

  ENDMETHOD.


  METHOD zif_adcoset_search_scope~has_next_package.

  ENDMETHOD.


  METHOD zif_adcoset_search_scope~next_package.

  ENDMETHOD.


  METHOD determine_count.
    SELECT COUNT(*)
      FROM zadcoset_repoobj
      WHERE object_type IN @search_scope-object_type_range
        AND object_name IN @search_scope-object_name_range
        AND ps_posid IN @search_scope-appl_comp_range
        AND devclass IN @search_scope-package_range
        AND owner IN @search_scope-owner_range
        AND created_date IN @search_scope-created_on_range
      INTO @object_count
      UP TO @search_scope-max_objects ROWS.
  ENDMETHOD.


ENDCLASS.
