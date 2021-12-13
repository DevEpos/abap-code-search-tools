"! <p class="shorttext synchronized" lang="en">Represents the source code of an object/include</p>
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

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Find matches with the given table of matchers</p>
    find_matches
      IMPORTING
        matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
        match_all            TYPE abap_bool OPTIONAL
        ignore_comment_lines TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_matches.

ENDINTERFACE.
