"! <p class="shorttext synchronized">Reader for code sources</p>
INTERFACE zif_adcoset_src_code_reader
  PUBLIC.

  "! <p class="shorttext synchronized">Retrieves source code instance of object via name</p>
  METHODS get_source_code
    IMPORTING
      !name         TYPE sobj_name
      !type         TYPE trobjtype OPTIONAL
    RETURNING
      VALUE(result) TYPE REF TO zif_adcoset_source_code
    RAISING
      zcx_adcoset_src_code_read.

ENDINTERFACE.
