"! <p class="shorttext synchronized" lang="en">Code Search Provider for Programs/Includes</p>
CLASS zcl_adcoset_csp_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    METHODS:
      constructor
        IMPORTING
          !custom_settings TYPE zif_adcoset_ty_global=>ty_prog_cs_settings.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
        custom_settings TYPE zif_adcoset_ty_global=>ty_prog_cs_settings.

    METHODS:
      search_dependent_includes
        IMPORTING
          src_code_reader   TYPE REF TO zif_adcoset_src_code_reader
          src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher
          object            TYPE zif_adcoset_ty_global=>ty_tadir_object
        RETURNING
          VALUE(result)     TYPE zif_adcoset_ty_global=>ty_search_matches,
      get_include_infos
        IMPORTING
          object        TYPE zif_adcoset_ty_global=>ty_tadir_object
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects.
ENDCLASS.



CLASS zcl_adcoset_csp_prog IMPLEMENTATION.

  METHOD constructor.
    me->custom_settings = custom_settings.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.

    TRY.
        DATA(source) = src_code_reader->get_source_code(
          name = object-name
          type = object-type ).

        LOOP AT src_code_searcher->search( source ) ASSIGNING FIELD-SYMBOL(<match>).
          <match>-include = object-name.
          result = VALUE #( BASE result ( <match> ) ).
        ENDLOOP.

        IF custom_settings-expand_includes = abap_true.
          result = VALUE #( BASE result
            ( LINES OF search_dependent_includes( src_code_reader   = src_code_reader
                                                  src_code_searcher = src_code_searcher
                                                  object            = object  ) ) ).
        ENDIF.
      CATCH zcx_adcoset_src_code_read.
    ENDTRY.

    zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
  ENDMETHOD.


  METHOD get_include_infos.
    DATA: includes TYPE STANDARD TABLE OF progname.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = object-name
      TABLES
        includetab = includes
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR includes IS INITIAL.
      RETURN.
    ENDIF.

    " select additional information about includes
    SELECT obj_name AS name,
           object AS type,
           devclass AS package_name,
           author AS owner
      FROM tadir
      FOR ALL ENTRIES IN @includes
      WHERE obj_name = @includes-table_line
        AND object = @zif_adcoset_c_global=>c_source_code_type-program
      INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.


  METHOD search_dependent_includes.

    LOOP AT get_include_infos( object ) ASSIGNING FIELD-SYMBOL(<include_info>).
      TRY.
          DATA(source) = src_code_reader->get_source_code(
            name = <include_info>-name
            type = object-type ).

          LOOP AT src_code_searcher->search( source ) ASSIGNING FIELD-SYMBOL(<match>).
            <match>-include = <include_info>-name.
            <match>-display_name = <include_info>-name.
            <match>-adt_include_type = 'PROG/I'.
            result = VALUE #( BASE result ( <match> ) ).
          ENDLOOP.
        CATCH zcx_adcoset_src_code_read.
          "handle exception
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
