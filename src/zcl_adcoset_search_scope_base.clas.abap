"! <p class="shorttext synchronized">Base class for search scope</p>
CLASS zcl_adcoset_search_scope_base DEFINITION
  PUBLIC ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_scope
      ABSTRACT METHODS next_package.

  PROTECTED SECTION.
    DATA search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    "! Restricts the maximum number of objects to select for the search
    DATA max_objects TYPE i.
    "! Holds the object count depending on whether the scope was loaded from the
    "! database or not
    DATA obj_count_for_package_building TYPE i.
    "! This holds the object count for the current scope
    DATA object_count TYPE i.
    DATA current_offset TYPE i.
    DATA all_packages_read TYPE abap_bool.
    DATA package_size TYPE i VALUE zif_adcoset_search_scope=>c_serial_package_size.
    DATA more_objects_in_scope TYPE abap_bool.

    "! Initializes the scope <br>
    "! Should be called inside subclass constructor
    METHODS init
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    "! Determines the object count that are in scope<br>
    "! Should set the attributes
    "! <ul>
    "!   <li>object_count</li>
    "!   <li>more_objects_in_scope</li>
    "! </ul>
    METHODS determine_count ABSTRACT.

    METHODS init_from_db
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS init_from_data
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS increase_scope_expiration
      IMPORTING
        scope_id TYPE sysuuid_x16.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_search_scope_base IMPLEMENTATION.
  METHOD init.
    IF search_scope-scope_id IS NOT INITIAL.
      init_from_db( search_scope ).
    ELSE.
      init_from_data( search_scope ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~count.
    result = object_count.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~has_next_package.
    result = xsdbool( all_packages_read = abap_false AND current_offset < object_count ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~more_objects_in_scope.
    result = more_objects_in_scope.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~get_scope_ranges.
    result = search_ranges.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~configure_package_size.
    CHECK max_task_count > 0.

    IF max_objects IS NOT INITIAL.
      " update max objects number
      package_size = max_objects.
      me->max_objects = max_objects.
      " set fixed object count
      object_count = max_objects + current_offset.
      obj_count_for_package_building = max_objects.
    ENDIF.

    DATA(determined_pack_size) = obj_count_for_package_building / max_task_count.

    IF determined_pack_size < zif_adcoset_search_scope=>c_min_parl_package_size.
      package_size = zif_adcoset_search_scope=>c_min_parl_package_size.
    ELSEIF determined_pack_size > zif_adcoset_search_scope=>c_max_parl_package_size.
      package_size = zif_adcoset_search_scope=>c_max_parl_package_size.
    ELSE.
      package_size = determined_pack_size.
    ENDIF.
  ENDMETHOD.

  METHOD init_from_db.
    SELECT SINGLE * FROM zadcoset_csscope
      WHERE id = @search_scope-scope_id
      INTO @DATA(scope_db).

    IF sy-subrc <> 0.
      object_count = 0.
      RETURN.
    ENDIF.

    increase_scope_expiration( scope_id = search_scope-scope_id ).

    current_offset = search_scope-current_offset.
    package_size = search_scope-max_objects.
    max_objects = search_scope-max_objects.
    " set fixed object count
    object_count = max_objects + current_offset.

    obj_count_for_package_building = max_objects.

    IF scope_db-ranges_data IS NOT INITIAL.
      CALL TRANSFORMATION id
           SOURCE XML scope_db-ranges_data
           RESULT data = search_ranges.
    ENDIF.
  ENDMETHOD.

  METHOD init_from_data.
    max_objects = search_scope-max_objects.
    search_ranges = search_scope-ranges.

    determine_count( ).

    obj_count_for_package_building = object_count.
  ENDMETHOD.

  METHOD increase_scope_expiration.
    DATA expiration TYPE zadcoset_csscope-expiration_datetime.

    GET TIME STAMP FIELD expiration.
    expiration = cl_abap_tstmp=>add( tstmp = expiration
                                     secs  = zif_adcoset_c_global=>c_default_scope_expiration ).

    UPDATE zadcoset_csscope SET expiration_datetime = expiration
                            WHERE id = scope_id.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

