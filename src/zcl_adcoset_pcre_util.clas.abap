"! <p class="shorttext synchronized">Utility to check PCRE support</p>
CLASS zcl_adcoset_pcre_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized">Checks if PCRE is supported in the system</p>
    CLASS-METHODS is_pcre_supported
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized">Returns 'X' if the parameter 'DOT_ALL' exists at CREATE_PCRE</p>
    CLASS-METHODS is_dot_all_param_existing
      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA pcre_supported TYPE abap_bool VALUE abap_undefined.
    CLASS-DATA dot_all_param_exists TYPE abap_bool VALUE abap_undefined.
ENDCLASS.


CLASS zcl_adcoset_pcre_util IMPLEMENTATION.
  METHOD class_constructor.
    DATA(abap_regex_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'CL_ABAP_REGEX' ) ).

    ASSIGN abap_regex_descr->methods[ name = 'CREATE_PCRE' ] TO FIELD-SYMBOL(<create_pcre_method>).
    pcre_supported = xsdbool( sy-subrc = 0 ).

    IF pcre_supported = abap_true.
      dot_all_param_exists = xsdbool( line_exists( <create_pcre_method>-parameters[ name = 'DOT_ALL' ] ) ).
    ELSE.
      dot_all_param_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_pcre_supported.
    result = pcre_supported.
  ENDMETHOD.

  METHOD is_dot_all_param_existing.
    result = dot_all_param_exists.
  ENDMETHOD.
ENDCLASS.
