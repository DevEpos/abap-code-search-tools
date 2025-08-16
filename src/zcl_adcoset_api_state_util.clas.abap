CLASS zcl_adcoset_api_state_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_api_state_view TYPE tabname VALUE 'ARS_ADT_API_FILTER'.
    CONSTANTS c_released_state TYPE string VALUE 'RELEASED'.
    CONSTANTS c_deprecated_state TYPE string VALUE 'DEPRECATED'.
    CONSTANTS:
      BEGIN OF c_fields,
        object_name TYPE fieldname VALUE 'TADIR_OBJ_NAME',
        object_type TYPE fieldname VALUE 'TADIR_OBJECT',
        api_state   TYPE fieldname VALUE 'FILTER_VALUE',
      END OF c_fields.

    CLASS-METHODS is_api_state_available
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS adjust_api_states_for_filter
      CHANGING
        api_state_range TYPE zif_adcoset_ty_global=>ty_search_scope_ranges-api_state_range.

  PRIVATE SECTION.
    CLASS-METHODS get_api_states
      EXPORTING
        released_states   TYPE string_table
        deprecated_states TYPE string_table.
ENDCLASS.


CLASS zcl_adcoset_api_state_util IMPLEMENTATION.
  METHOD is_api_state_available.
    SELECT SINGLE @abap_true FROM ddddlsrc
      WHERE ddlname = @c_api_state_view
      INTO @result.
  ENDMETHOD.

  METHOD get_api_states.
    DATA(api_states) = cl_ris_adt_res_release_states=>get_all( ).

    LOOP AT api_states INTO DATA(api_state) WHERE     name <> c_released_state
                                                  AND name <> c_deprecated_state.
      IF api_state-name CP 'DEPRECATED*'.
        deprecated_states = VALUE #( BASE deprecated_states ( api_state-name ) ).
      ELSE.
        released_states = VALUE #( BASE released_states ( api_state-name ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD adjust_api_states_for_filter.
    DATA released_states TYPE string_table.
    DATA deprecated_states TYPE string_table.
    DATA replaced_states LIKE api_state_range.

    CHECK api_state_range IS NOT INITIAL.

    LOOP AT api_state_range REFERENCE INTO DATA(api_state).
      IF NOT ( api_state->low = c_released_state OR api_state->low = c_deprecated_state ).
        CONTINUE.
      ENDIF.

      IF released_states IS INITIAL.
        get_api_states( IMPORTING released_states   = released_states
                                  deprecated_states = deprecated_states ).
      ENDIF.

      IF api_state->low = c_released_state.
        replaced_states = VALUE #(
            BASE replaced_states
            ( LINES OF VALUE #( FOR <state> IN released_states
                                ( sign = api_state->sign option = api_state->option low = <state> ) ) ) ).
      ENDIF.

      IF api_state->low = c_deprecated_state.
        replaced_states = VALUE #(
            BASE replaced_states
            ( LINES OF VALUE #( FOR <state> IN deprecated_states
                                ( sign = api_state->sign option = api_state->option low = <state> ) ) ) ).
      ENDIF.

      DELETE api_state_range.
    ENDLOOP.

    api_state_range = VALUE #( BASE api_state_range ( LINES OF replaced_states ) ).

    SORT api_state_range BY sign
                            option
                            low.
    DELETE ADJACENT DUPLICATES FROM api_state_range COMPARING sign option low.
  ENDMETHOD.
ENDCLASS.
