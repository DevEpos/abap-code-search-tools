"! <p class="shorttext synchronized" lang="en">Utility to test extension availability</p>
CLASS zcl_adcoset_extensions_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Checks if ABAP Tags feature is installed</p>
      is_abap_tags_available
        RETURNING
          VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_field_config,
        fieldname TYPE fieldname,
        rollname  TYPE rollname,
      END OF ty_field_config,
      ty_field_configs TYPE STANDARD TABLE OF ty_field_config WITH EMPTY KEY,
      ty_table_fields  TYPE STANDARD TABLE OF dd03p WITH EMPTY KEY.

    CLASS-DATA:
      abap_tags_available TYPE abap_bool VALUE abap_undefined.

    CLASS-METHODS:
      test_field_existence
        IMPORTING
          fields_to_test TYPE ty_field_configs
          fields         TYPE ty_table_fields
        RETURNING
          VALUE(result)  TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_extensions_util IMPLEMENTATION.

  METHOD is_abap_tags_available.
    DATA: table_def    TYPE dd02v,
          table_fields TYPE ty_table_fields.

    IF abap_tags_available = abap_undefined.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name          = 'ZABAPTAGS_TGOBJ'
        IMPORTING
          dd02v_wa      = table_def
        TABLES
          dd03p_tab     = table_fields
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc = 0 AND
          table_def-tabclass = 'TRANSP' AND
          table_fields IS NOT INITIAL AND
          test_field_existence(
            fields         = table_fields
            fields_to_test = VALUE #(
              ( fieldname = 'OBJECT_NAME' rollname = 'SOBJ_NAME' )
              ( fieldname = 'OBJECT_TYPE' rollname = 'TROBJTYPE' )
              ( fieldname = 'TAG_ID'      rollname = 'ZABAPTAGS_TAG_ID' )  ) ).
        abap_tags_available = abap_true.
      ELSE.
        abap_tags_available = abap_false.
      ENDIF.
    ENDIF.

    result = abap_tags_available.
  ENDMETHOD.


  METHOD test_field_existence.

    result = abap_true.

    LOOP AT fields_to_test ASSIGNING FIELD-SYMBOL(<field_to_test>).
      IF NOT line_exists( fields[
                            fieldname = <field_to_test>-fieldname
                            rollname  = <field_to_test>-rollname  ] ).
        CLEAR result.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
