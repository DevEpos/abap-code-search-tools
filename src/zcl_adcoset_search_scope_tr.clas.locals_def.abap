*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CLASS lcl_limu_processor DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        tr_objects    TYPE zif_adcoset_ty_global=>ty_tr_request_objects
        search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    METHODS run
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects.

  PRIVATE SECTION.
    DATA objects TYPE zif_adcoset_ty_global=>ty_tadir_objects.
    DATA tr_objects TYPE zif_adcoset_ty_global=>ty_tr_request_objects.
    DATA search_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    METHODS handle_limu_object
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_function_module
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_report_source_code
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_method
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_include
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_section
      IMPORTING
        limu_object     TYPE zif_adcoset_ty_global=>ty_tr_request_object
        section_include TYPE program.

    METHODS handle_class_private_section
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_protected_section
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_public_section
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_definition
      IMPORTING
        limu_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS add_subobject
      IMPORTING
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype
        subobjects       TYPE zif_adcoset_ty_global=>ty_tadir_object_infos
      RAISING
        cx_sy_itab_line_not_found.

    METHODS add_result
      IMPORTING
        limu_object      TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype.

    METHODS add_result_cl_definition
      IMPORTING
        limu_object      TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype.

    METHODS apply_post_filter.

    METHODS fill_missing_tadir_info.

    METHODS post_filter.

ENDCLASS.
