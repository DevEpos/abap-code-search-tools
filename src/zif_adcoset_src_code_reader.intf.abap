"! <p class="shorttext synchronized" lang="en">Reader for code sources</p>
INTERFACE zif_adcoset_src_code_reader
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves source code instance of object via name</p>
    get_source_code
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_source_code
      RAISING
        zcx_adcoset_src_code_read.

ENDINTERFACE.
