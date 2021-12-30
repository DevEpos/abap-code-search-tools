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
      is_more_objects_available  TYPE abap_bool,
      parallel_mode  TYPE abap_bool.

    METHODS:
      determine_count,
      set_package_size,
      resolve_packages.
ENDCLASS.



CLASS zcl_adcoset_search_scope IMPLEMENTATION.


  METHOD constructor.
    me->search_scope = search_scope.
    me->parallel_mode = parallel_mode.

    resolve_packages( ).
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


  METHOD zif_adcoset_search_scope~more_objects_in_scope.
    result = is_more_objects_available.
  ENDMETHOD.


  METHOD determine_count.
    DATA(selection_limit) = search_scope-max_objects + 1.
    SELECT COUNT(*)
      FROM zadcoset_repoobj
      WHERE object_type IN @search_scope-object_type_range
        AND object_name IN @search_scope-object_name_range
        AND ps_posid IN @search_scope-appl_comp_range
        AND devclass IN @search_scope-package_range
        AND owner IN @search_scope-owner_range
        AND created_date IN @search_scope-created_on_range
      INTO @object_count
      UP TO @selection_limit ROWS.

    IF object_count = selection_limit.
      object_count = search_scope-max_objects.
      is_more_objects_available = abap_true.
    ENDIF.
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


  METHOD resolve_packages.
    CHECK search_scope-package_range IS NOT INITIAL.

    DATA(resolved_package_range) = zcl_adcoset_devc_reader=>resolve_packages( search_scope-package_range ).
    search_scope-package_range = VALUE #(
      ( LINES OF resolved_package_range )
      ( LINES OF zcl_adcoset_devc_reader=>get_subpackages_by_range( resolved_package_range ) ) ).

    SORT search_scope-package_range BY low.
    DELETE ADJACENT DUPLICATES FROM search_scope-package_range COMPARING low.
  ENDMETHOD.

ENDCLASS.
