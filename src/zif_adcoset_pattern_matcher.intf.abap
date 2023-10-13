"! <p class="shorttext synchronized">Finds matches in a string for a given (RegExp) pattern</p>
INTERFACE zif_adcoset_pattern_matcher
  PUBLIC.

  TYPES ty_ref_tab TYPE STANDARD TABLE OF REF TO zif_adcoset_pattern_matcher WITH EMPTY KEY.

  DATA control_flags TYPE zif_adcoset_ty_global=>ty_control_flags READ-ONLY.

  "! <p class="shorttext synchronized">Retrieves the matches in the given source</p>
  METHODS find_matches
    IMPORTING
      !source       TYPE string_table
    RETURNING
      VALUE(result) TYPE match_result_tab
    RAISING
      zcx_adcoset_pattern_sh_error.

  "! <p class="shorttext synchronized">Retrieves matches in the given source and range</p>
  METHODS find_matches_in_range
    IMPORTING
      !source       TYPE string_table
      start_line    TYPE i
      !offset       TYPE i
      end_line      TYPE i
      end_offset    TYPE i
    RETURNING
      VALUE(result) TYPE match_result_tab
    RAISING
      zcx_adcoset_pattern_sh_error.

  "! <p class="shorttext synchronized">Retrieves the next match in the given source and offset</p>
  METHODS find_next_match
    IMPORTING
      !source       TYPE string_table
      start_line    TYPE i OPTIONAL
      !offset       TYPE i OPTIONAL
      end_line      TYPE i OPTIONAL
      end_offset    TYPE i OPTIONAL
    RETURNING
      VALUE(result) TYPE match_result
    RAISING
      zcx_adcoset_pattern_sh_error.
ENDINTERFACE.
