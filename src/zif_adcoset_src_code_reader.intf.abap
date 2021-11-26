"! <p class="shorttext synchronized" lang="en">Reader for code sources</p>
INTERFACE zif_adcoset_src_code_reader
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves source code as string</p>
    get_source_text
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string,

    "! <p class="shorttext synchronized" lang="en">Retrieves source code as string table</p>
    get_source_table
      IMPORTING
        name          TYPE sobj_name
      RETURNING
        VALUE(result) TYPE string_table.

ENDINTERFACE.
