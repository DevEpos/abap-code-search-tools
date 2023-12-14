*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_limu_processor DEFINITION.

  PUBLIC SECTION.
    DATA result TYPE zif_adcoset_ty_global=>ty_tadir_objects.

    METHODS constructor
      IMPORTING
        !result TYPE zif_adcoset_ty_global=>ty_tadir_objects.

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
        main_object_name       TYPE sobj_name
        main_object_type       TYPE trobjtype
        has_deleted_subobjects TYPE abap_bool
        subobjects             TYPE zif_adcoset_ty_global=>ty_tadir_object_infos
      RAISING
        cx_sy_itab_line_not_found.

    METHODS add_result
      IMPORTING
        tr_object              TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name       TYPE sobj_name
        main_object_type       TYPE trobjtype
        has_deleted_subobjects TYPE abap_bool.

    METHODS add_result_cl_definition
      IMPORTING
        tr_object              TYPE zif_adcoset_ty_global=>ty_tr_request_object
        main_object_name       TYPE sobj_name
        main_object_type       TYPE trobjtype
        has_deleted_subobjects TYPE abap_bool.

ENDCLASS.
