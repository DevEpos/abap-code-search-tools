"! <p class="shorttext synchronized">Code Search Provider for Programs/Includes</p>
CLASS zcl_adcoset_csp_prog DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    METHODS constructor
      IMPORTING
        custom_settings TYPE zif_adcoset_ty_global=>ty_prog_cs_settings.

  PRIVATE SECTION.
    TYPES ty_prognames TYPE STANDARD TABLE OF progname WITH EMPTY KEY.

    DATA custom_settings TYPE zif_adcoset_ty_global=>ty_prog_cs_settings.

    METHODS search_dependent_includes
      IMPORTING
        src_code_reader   TYPE REF TO zif_adcoset_src_code_reader
        src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher
        !object           TYPE zif_adcoset_ty_global=>ty_tadir_object_info
      RETURNING
        VALUE(result)     TYPE zif_adcoset_ty_global=>ty_search_matches.

    METHODS get_include_infos
      IMPORTING
        !object       TYPE zif_adcoset_ty_global=>ty_tadir_object_info
      RETURNING
        VALUE(result) TYPE ty_prognames.

    METHODS get_adt_type
      IMPORTING
        include_name  TYPE progname
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_adcoset_csp_prog IMPLEMENTATION.
  METHOD constructor.
    me->custom_settings = custom_settings.
  ENDMETHOD.

  METHOD zif_adcoset_code_search_prov~search.
    TRY.
        DATA(source) = src_code_reader->get_source_code( name = object-name
                                                         type = object-type ).

        LOOP AT src_code_searcher->search( source ) ASSIGNING FIELD-SYMBOL(<match>).
          <match>-include = object-name.
          result = VALUE #( BASE result ( <match> ) ).
        ENDLOOP.

        IF custom_settings-expand_includes = abap_true.
          result = VALUE #( BASE result
                            ( LINES OF search_dependent_includes( src_code_reader   = src_code_reader
                                                                  src_code_searcher = src_code_searcher
                                                                  object            = object-info ) ) ).
        ENDIF.
      CATCH zcx_adcoset_src_code_read.
    ENDTRY.

    zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
  ENDMETHOD.

  METHOD get_include_infos.
    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING  program    = object-name
      TABLES     includetab = result
      EXCEPTIONS OTHERS     = 1.
  ENDMETHOD.

  METHOD search_dependent_includes.
    LOOP AT get_include_infos( object ) ASSIGNING FIELD-SYMBOL(<include>).
      TRY.
          DATA(source) = src_code_reader->get_source_code( name = <include>
                                                           type = object-type ).

          DATA(matches) = src_code_searcher->search( source ).

          IF matches IS NOT INITIAL.
            DATA(adt_type) = get_adt_type( <include> ).

            LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
              <match>-include          = <include>.
              <match>-display_name     = <include>.
              <match>-adt_include_type = adt_type.
              result = VALUE #( BASE result ( <match> ) ).
            ENDLOOP.

          ENDIF.
        CATCH zcx_adcoset_src_code_read.
      ENDTRY.

      zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_adt_type.
    " get correct adt type
    DATA is_fugr_include TYPE abap_bool.

    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING  progname_with_namespace = include_name
      IMPORTING  fugr_is_include_name    = is_fugr_include
      EXCEPTIONS delimiter_error         = 1
                 OTHERS                  = 2.

    result = COND #( WHEN sy-subrc = 0 AND is_fugr_include = abap_true THEN 'FUGR/I' ELSE 'PROG/I' ).
  ENDMETHOD.
ENDCLASS.
