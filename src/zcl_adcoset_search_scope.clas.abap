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
          search_scope  TYPE zif_adcoset_ty_global=>ty_search_scope
          parallel_mode TYPE abap_bool OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      search_scope   TYPE zif_adcoset_ty_global=>ty_search_scope,
      object_count   TYPE i,
      current_offset TYPE i,
      package_size   TYPE i,
      parallel_mode  TYPE abap_bool.

    METHODS:
      determine_count,
      set_package_size.
ENDCLASS.



CLASS zcl_adcoset_search_scope IMPLEMENTATION.


  METHOD constructor.
    me->search_scope = search_scope.
    me->parallel_mode = parallel_mode.
    determine_count( ).
    set_package_size( ).
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~count.
    result = object_count.
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~has_next_package.
    result = xsdbool( current_offset < object_count ).
  ENDMETHOD.


  METHOD zif_adcoset_search_scope~next_package.
    SELECT object_type AS type,
           object_name AS name
      FROM zadcoset_repoobj
      WHERE object_type IN @search_scope-object_type_range
        AND object_name IN @search_scope-object_name_range
        AND ps_posid IN @search_scope-appl_comp_range
        AND devclass IN @search_scope-package_range
        AND owner IN @search_scope-owner_range
        AND created_date IN @search_scope-created_on_range
      ORDER BY object_name
      INTO CORRESPONDING FIELDS OF TABLE @result
      UP TO @package_size ROWS
      OFFSET @current_offset.

    current_offset = current_offset + package_size.
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


  METHOD set_package_size.
    IF parallel_mode = abap_true.
      IF object_count < 10.
        package_size = 2.
      ELSEIF object_count < 50.
        package_size = 10.
      ELSEIF object_count < 500.
        package_size = 100.
      ELSE.
        package_size = 150.
      ENDIF.
    ELSE.
      " fixed size during sequential mode regardless of full scope
      package_size = 100.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
