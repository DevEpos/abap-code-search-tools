"! <p class="shorttext synchronized" lang="en">Default search provider for repository source code search</p>
CLASS zcl_adcoset_csp_repsrc_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of a default search provider</p>
      constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_include
        IMPORTING
          object        TYPE zif_adcoset_ty_global=>ty_tadir_object
        RETURNING
          VALUE(result) TYPE progname,
      search_dependent_includes
        IMPORTING
          src_code_reader   TYPE REF TO zif_adcoset_src_code_reader
          src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher
          object            TYPE zif_adcoset_ty_global=>ty_tadir_object
        RETURNING
          VALUE(result)     TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.



CLASS zcl_adcoset_csp_repsrc_default IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.
    DATA(include_name) = get_include( object ).

    IF include_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(source) = src_code_reader->get_source_code(
          name = include_name
          type = object-type ).

        LOOP AT src_code_searcher->search( source ) ASSIGNING FIELD-SYMBOL(<match>).
          <match>-include = include_name.
          result = VALUE #( BASE result ( <match> ) ).
        ENDLOOP.

        IF object-type = zif_adcoset_c_global=>c_source_code_type-program.
          result = VALUE #( BASE result ( LINES OF search_dependent_includes( src_code_reader   = src_code_reader
                                                                              src_code_searcher = src_code_searcher
                                                                              object            = object  ) ) ).
        ENDIF.

      CATCH zcx_adcoset_src_code_read.
        "handle exception
    ENDTRY.

    zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
  ENDMETHOD.


  METHOD get_include.
    DATA: include_suffix  TYPE string.

    CASE object-type.

      WHEN zif_adcoset_c_global=>c_source_code_type-interface.
        include_suffix = 'IU'.

      WHEN zif_adcoset_c_global=>c_source_code_type-behavior_definition.
        include_suffix = 'BD'.

      WHEN zif_adcoset_c_global=>c_source_code_type-simple_transformation.
        include_suffix = 'XT'.

      WHEN zif_adcoset_c_global=>c_source_code_type-type_group.
        result = |%_C{ object-name }|.

      WHEN zif_adcoset_c_global=>c_source_code_type-program.
        result = object-name.

    ENDCASE.

    IF result IS INITIAL AND include_suffix IS NOT INITIAL.
      result = object-name.
      TRANSLATE result(30) USING ' ='.
      result+30 = include_suffix.
    ENDIF.

  ENDMETHOD.


  METHOD search_dependent_includes.
    DATA: includes      TYPE STANDARD TABLE OF progname,
          include_infos TYPE zif_adcoset_ty_global=>ty_tadir_objects.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = object-name
      TABLES
        includetab = includes
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc = 0 AND includes IS NOT INITIAL.
      " select additional information about includes
      SELECT obj_name AS name,
             object AS type,
             devclass AS package_name,
             author AS owner
        FROM tadir
        FOR ALL ENTRIES IN @includes
        WHERE obj_name = @includes-table_line
          AND object = @zif_adcoset_c_global=>c_source_code_type-program
        INTO CORRESPONDING FIELDS OF TABLE @include_infos.

      LOOP AT include_infos ASSIGNING FIELD-SYMBOL(<include_info>).
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

    ENDIF.
  ENDMETHOD.
ENDCLASS.
