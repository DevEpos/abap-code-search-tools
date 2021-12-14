"! <p class="shorttext synchronized" lang="en">Factory for ADT links</p>
INTERFACE zif_adcoset_adt_obj_factory
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves ADT object reference for tadir object</p>
    get_object_ref_for_trobj
      IMPORTING
        type                   TYPE trobjtype
        name                   TYPE sobj_name
        append_source_uri_path TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)          TYPE sadt_object_reference
      RAISING
        zcx_adcoset_static_error,

    "! <p class="shorttext synchronized" lang="en">Retrieves ADT object reference for workbench object</p>
    get_object_ref_for_sub_object
      IMPORTING
        wbobjtype        TYPE wbobjtype
        enclosing_object TYPE sobj_name
        sub_object       TYPE eu_lname
      RETURNING
        VALUE(result)    TYPE sadt_object_reference
      RAISING
        zcx_adcoset_static_error,

    "! <p class="shorttext synchronized" lang="en">Retrieves ADT object reference for a given include</p>
    get_object_ref_for_include
      IMPORTING
        main_program      TYPE progname
        include           TYPE progname
        start_line        TYPE i DEFAULT if_adt_uri_mapper=>co_position_not_set
        start_line_offset TYPE i DEFAULT if_adt_uri_mapper=>co_position_not_set
        end_line          TYPE i DEFAULT if_adt_uri_mapper=>co_position_not_set
        end_line_offset   TYPE i DEFAULT if_adt_uri_mapper=>co_position_not_set
      RETURNING
        VALUE(result)     TYPE sadt_object_reference
      RAISING
        zcx_adcoset_static_error,

    "! <p class="shorttext synchronized" lang="en">Add positional fragment to uri</p>
    add_position_fragment
      IMPORTING
        start_line   TYPE i
        start_column TYPE i OPTIONAL
        end_line     TYPE i OPTIONAL
        end_column   TYPE i OPTIONAL
      CHANGING
        link         TYPE string.

ENDINTERFACE.
