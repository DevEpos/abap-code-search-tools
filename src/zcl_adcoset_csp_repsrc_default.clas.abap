"! <p class="shorttext synchronized">Default search provider for repository source code search</p>
CLASS zcl_adcoset_csp_repsrc_default DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    "! <p class="shorttext synchronized">Creates new instance of a default search provider</p>
    METHODS constructor.

  PRIVATE SECTION.
    METHODS get_include
      IMPORTING
        object_info   TYPE zif_adcoset_ty_global=>ty_tadir_object_info
      RETURNING
        VALUE(result) TYPE progname.
ENDCLASS.


CLASS zcl_adcoset_csp_repsrc_default IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD zif_adcoset_code_search_prov~search.
    DATA(include_name) = get_include( object-info ).

    IF include_name IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(source) = src_code_reader->get_source_code( name = include_name
                                                         type = object-type ).

        LOOP AT src_code_searcher->search( source ) ASSIGNING FIELD-SYMBOL(<match>).
          <match>-include = include_name.
          result = VALUE #( BASE result
                            ( <match> ) ).
        ENDLOOP.

      CATCH zcx_adcoset_src_code_read.
    ENDTRY.

    zcl_adcoset_search_protocol=>increment_searched_srcs_count( ).
  ENDMETHOD.

  METHOD get_include.
    DATA include_suffix TYPE string.

    CASE object_info-type.

      WHEN zif_adcoset_c_global=>c_source_code_type-interface.
        include_suffix = 'IU'.

      WHEN zif_adcoset_c_global=>c_source_code_type-behavior_definition.
        include_suffix = 'BD'.

      WHEN zif_adcoset_c_global=>c_source_code_type-simple_transformation.
        include_suffix = 'XT'.

      WHEN zif_adcoset_c_global=>c_source_code_type-type_group.
        result = |%_C{ object_info-name }|.

    ENDCASE.

    IF result IS INITIAL AND include_suffix IS NOT INITIAL.
      result = object_info-name.
      TRANSLATE result(30) USING ' ='.
      result+30 = include_suffix.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
