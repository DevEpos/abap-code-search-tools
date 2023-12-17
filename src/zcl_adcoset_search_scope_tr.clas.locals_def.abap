*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES BEGIN OF ty_tadir_object_extended.
        INCLUDE TYPE zif_adcoset_ty_global=>ty_tadir_object_info AS info.
TYPES   complete_main_object TYPE abap_bool.
TYPES   subobjects           TYPE zif_adcoset_ty_global=>ty_tadir_object_infos.
TYPES END OF ty_tadir_object_extended.

TYPES:
  BEGIN OF ty_scope_package_ext,
    count  TYPE i,
    object TYPE STANDARD TABLE OF ty_tadir_object_extended WITH DEFAULT KEY,
  END OF ty_scope_package_ext.

CLASS lcl_limu_processor DEFINITION.

  PUBLIC SECTION.
    DATA result_extended TYPE ty_scope_package_ext.

    METHODS constructor
      IMPORTING
        result_extended TYPE ty_scope_package_ext.

    METHODS handle_function_module
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_report_source_code
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_method
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_include
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_section
      IMPORTING
        tr_object       TYPE zif_adcoset_ty_global=>ty_tr_request_object
        section_include TYPE program.

    METHODS handle_class_private_section
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_protected_section
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_public_section
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

    METHODS handle_class_definition
      IMPORTING
        tr_object TYPE zif_adcoset_ty_global=>ty_tr_request_object.

  PRIVATE SECTION.
    METHODS add_subobject
      IMPORTING
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype
        subobjects       TYPE zif_adcoset_ty_global=>ty_tadir_object_infos
      RAISING
        cx_sy_itab_line_not_found.

    METHODS add_result
      IMPORTING
        tr_object        TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype.

    METHODS add_result_cl_definition
      IMPORTING
        tr_object        TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name TYPE sobj_name
        main_object_type TYPE trobjtype.

ENDCLASS.
