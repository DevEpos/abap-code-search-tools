"! <p class="shorttext synchronized">Utility for message handling</p>
CLASS zcl_adcoset_message_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Splits text into sy-msg variables</p>
    CLASS-METHODS split_string_to_symsg
      IMPORTING
        !text TYPE string.
ENDCLASS.


CLASS zcl_adcoset_message_util IMPLEMENTATION.
  METHOD split_string_to_symsg.
    DATA offset TYPE i.

    DATA(rest_text) = text.

    DATA(msgv1) = rest_text.
    SHIFT rest_text LEFT BY 50 PLACES.
    DATA(msgv2) = rest_text.
    SHIFT rest_text LEFT BY 50 PLACES.
    DATA(msgv3) = rest_text.
    SHIFT rest_text LEFT BY 50 PLACES.
    DATA(msgv4) = rest_text.

    IF strlen( rest_text ) > 50.
      FIND ALL OCCURRENCES OF REGEX '.\s.' IN SECTION LENGTH 47 OF msgv4 MATCH OFFSET offset.
      IF sy-subrc = 0.
        offset = offset + 1.
        msgv4 = msgv4(offset).

        msgv4 = |{ msgv4 }...|.
      ENDIF.
    ENDIF.

    " TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)
    MESSAGE e001(00) WITH msgv1 msgv2 msgv3 msgv4 INTO DATA(msg).
  ENDMETHOD.
ENDCLASS.
