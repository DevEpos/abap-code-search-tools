"! <p class="shorttext synchronized" lang="en">No valid pattern matcher found</p>
CLASS zcx_adcoset_no_matcher DEFINITION
  PUBLIC
  INHERITING FROM zcx_adcoset_static_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_adcoset_no_matcher IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      textid   = textid
      previous = previous ).
  ENDMETHOD.


ENDCLASS.
