"! <p class="shorttext synchronized">General ADT error</p>
CLASS zcx_adcoset_adt_rest DEFINITION
  PUBLIC
  INHERITING FROM cx_adt_rest
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        textid      LIKE if_t100_message=>t100key           OPTIONAL
        !text       TYPE string                             OPTIONAL
        !previous   LIKE previous                           OPTIONAL
        subtype     TYPE sadt_exc_type                      OPTIONAL
        !properties TYPE REF TO if_adt_exception_properties OPTIONAL.

    METHODS get_http_status REDEFINITION.
    METHODS get_namespace   REDEFINITION.
    METHODS get_type        REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_adcoset_adt_rest IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    DATA fill_t100key TYPE abap_bool.

    super->constructor( previous   = previous
                        subtype    = subtype
                        msgv1      = msgv1
                        msgv2      = msgv2
                        msgv3      = msgv3
                        msgv4      = msgv4
                        properties = properties ).

    IF text IS NOT INITIAL.
      fill_t100key = abap_true.
      zcl_adcoset_message_util=>split_string_to_symsg( text ).
    ELSEIF sy-msgid IS NOT INITIAL.
      fill_t100key = abap_true.
    ENDIF.

    IF fill_t100key = abap_true.
      msgv1 = sy-msgv1.
      msgv2 = sy-msgv2.
      msgv3 = sy-msgv3.
      msgv4 = sy-msgv4.
      if_t100_message~t100key = VALUE #( msgid = sy-msgid
                                         msgno = sy-msgno
                                         attr1 = 'MSGV1'
                                         attr2 = 'MSGV2'
                                         attr3 = 'MSGV3'
                                         attr4 = 'MSGV4' ).
    ELSE.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ENDIF.
  ENDMETHOD.

  METHOD get_http_status.
    result = cl_rest_status_code=>gc_server_error_internal.
  ENDMETHOD.

  METHOD get_namespace.
    result = 'com.devepos.adt.cst'.
  ENDMETHOD.

  METHOD get_type.
    result = 'General'.
  ENDMETHOD.
ENDCLASS.
