"! <p class="shorttext synchronized">Represents the source code of an object/include</p>
INTERFACE zif_adcoset_source_code
  PUBLIC.

  TYPES:
    BEGIN OF ty_line_index,
      number TYPE i,
      offset TYPE i,
      length TYPE i,
    END OF ty_line_index,
    ty_line_indexes TYPE TABLE OF ty_line_index WITH KEY number
                                                WITH UNIQUE HASHED KEY offset COMPONENTS offset.

  DATA content TYPE string_table READ-ONLY.
  DATA line_indexes TYPE ty_line_indexes READ-ONLY.
  DATA is_single_line_content TYPE abap_bool READ-ONLY.
  DATA line_count TYPE i READ-ONLY.
  DATA comment_regex TYPE string READ-ONLY.

ENDINTERFACE.
