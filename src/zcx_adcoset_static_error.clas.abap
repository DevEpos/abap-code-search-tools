"! <p class="shorttext synchronized" lang="en">General static exception in code search tools library</p>
CLASS zcx_adcoset_static_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.

    ALIASES:
      msgv1 FOR if_t100_dyn_msg~msgv1,
      msgv2 FOR if_t100_dyn_msg~msgv2,
      msgv3 FOR if_t100_dyn_msg~msgv3,
      msgv4 FOR if_t100_dyn_msg~msgv4,
      msgty FOR if_t100_dyn_msg~msgty.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_adcoset_static_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
