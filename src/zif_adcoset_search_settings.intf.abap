"! <p class="shorttext synchronized" lang="en">Settings for code search</p>
INTERFACE zif_adcoset_search_settings
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'use_regex'</p>
    is_use_regex
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'multiline_search'</p>
    "! If active the search will be performed on the whole source code, otherwise it will be
    "! searched line by line.
    is_multiline_search
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'ignore_comment_lines'</p>
    "! If the setting 'ignore_comment_lines' is active, all comment lines in the source code will
    "! not be searched.
    is_ignore_comment_lines
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'match_all_patterns'</p>
    "! If the setting 'match_all_patterns' is active, an object with source code will only count as
    "! match if all patterns are found
    is_match_all_patterns
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the new line character for the source code</p>
    "! If multi line search is active some source code needs to be concatenated
    "! from a multi line string table to one continues string. <br>
    "! The correct new line character is important otherwise the navigation from the
    "! search result may not be accurate.
    get_new_line_character
      RETURNING
        VALUE(result) TYPE string,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'parallel_processing'</p>
    is_parallel_processing
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
