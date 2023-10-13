"! <p class="shorttext synchronized">Messages that occur during processing</p>
CLASS zcl_adcoset_log DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS add_exception
      IMPORTING
        !exception TYPE REF TO cx_root.

    "! <p class="shorttext synchronized">Adds message</p>
    CLASS-METHODS add_message
      IMPORTING
        !type   TYPE string
        content TYPE string.

    "! <p class="shorttext synchronized">Adds a list of messages</p>
    CLASS-METHODS add_all_messages
      IMPORTING
        !messages TYPE zif_adcoset_ty_global=>ty_messages.

    "! <p class="shorttext synchronized">Clears current list of messages</p>
    CLASS-METHODS clear.

    "! <p class="shorttext synchronized">Returns current list of messages</p>
    CLASS-METHODS get_messages
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_messages.

  PRIVATE SECTION.
    CLASS-DATA messages TYPE HASHED TABLE OF zif_adcoset_ty_global=>ty_message WITH UNIQUE KEY content.
ENDCLASS.


CLASS zcl_adcoset_log IMPLEMENTATION.
  METHOD add_exception.
    DATA error_message TYPE string.

    DATA(parent_exc) = exception.

    WHILE parent_exc IS BOUND OR error_message IS INITIAL.
      TRY.
          DATA(t100_message) = CAST if_t100_message( parent_exc ).
          IF t100_message->t100key-msgid = 'SY' AND t100_message->t100key-msgno = 530.
            parent_exc = parent_exc->previous.
            CONTINUE.
          ENDIF.

        CATCH cx_sy_move_cast_error.
      ENDTRY.

      " add all messages to the log??
      error_message = parent_exc->get_text( ).
      parent_exc = parent_exc->previous.

    ENDWHILE.

    IF error_message IS INITIAL AND exception IS BOUND.
      error_message = exception->get_text( ).
    ENDIF.

    add_message( type    = zif_adcoset_c_global=>c_message_type-error
                 content = error_message ).
  ENDMETHOD.

  METHOD add_message.
    CHECK content IS NOT INITIAL.

    ASSIGN messages[ content = content ] TO FIELD-SYMBOL(<message>).
    IF sy-subrc <> 0.
      INSERT VALUE #( type        = type
                      content     = content
                      occurrences = 1 ) INTO TABLE messages.
    ELSE.
      <message>-occurrences = <message>-occurrences + 1.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR messages.
  ENDMETHOD.

  METHOD get_messages.
    result = messages.
  ENDMETHOD.

  METHOD add_all_messages.
    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>).
      INSERT <message> INTO TABLE zcl_adcoset_log=>messages.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
