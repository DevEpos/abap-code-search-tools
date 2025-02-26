"! <p class="shorttext synchronized">Search Provider for Classes</p>
CLASS zcl_adcoset_csp_clas DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    "! <p class="shorttext synchronized">Creates new instance of a class search provider</p>
    METHODS constructor
      IMPORTING
        custom_settings TYPE zif_adcoset_ty_global=>ty_clas_cs_settings.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_include_types,
        method TYPE string VALUE 'CLAS/OM',
        include TYPE string VALUE 'CLAS/I',
      END OF c_include_types.

    CONSTANTS:
      BEGIN OF c_section_texts,
        main_source       TYPE string VALUE `Main Source`,
        locals_def        TYPE string VALUE `Local Type Definitions`,
        local_impl        TYPE string VALUE `Local Class Implementation`,
        macros            TYPE string VALUE `Macros`,
        test_cls          TYPE string VALUE `Local Test Classes`,
        public_section    TYPE string VALUE 'Public Section',
        protected_section TYPE string VALUE 'Protected Section',
        private_section   TYPE string VALUE 'Private Section',
        method            TYPE string VALUE `Method `,
      END OF c_section_texts.

    TYPES:
      BEGIN OF ty_class_incl,
        name        TYPE progname,
        method_name TYPE seocpdname,
        adt_type    TYPE string,
      END OF ty_class_incl,
      ty_class_includes TYPE STANDARD TABLE OF ty_class_incl WITH KEY name.

    DATA custom_settings TYPE zif_adcoset_ty_global=>ty_clas_cs_settings.

    METHODS get_class_includes
      IMPORTING
        !name         TYPE sobj_name
      RETURNING
        VALUE(result) TYPE ty_class_includes.

    METHODS assign_objects_to_matches
      IMPORTING
        unassigned_matches TYPE zif_adcoset_ty_global=>ty_search_matches
        !include           TYPE ty_class_incl
      CHANGING
        all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.


CLASS zcl_adcoset_csp_clas IMPLEMENTATION.
  METHOD constructor.
    me->custom_settings = custom_settings.
  ENDMETHOD.

  METHOD zif_adcoset_code_search_prov~search.
    DATA searched_sources_count TYPE i.

    DATA(class_includes) = get_class_includes( name = object-name ).
    IF class_includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT class_includes ASSIGNING FIELD-SYMBOL(<include>).

      IF NOT (    object-limu_objects IS INITIAL
               OR line_exists( object-limu_objects[ name = <include>-method_name ] )
               OR line_exists( object-limu_objects[ name = <include>-name ] ) ).
        CONTINUE.
      ENDIF.

      searched_sources_count = searched_sources_count + 1.
      TRY.
          DATA(source_code) = src_code_reader->get_source_code( name = <include>-name ).
          DATA(matches) = src_code_searcher->search( source_code = source_code ).

          IF matches IS NOT INITIAL.
            assign_objects_to_matches( EXPORTING unassigned_matches = matches
                                                 include            = <include>
                                       CHANGING  all_matches        = result ).
          ENDIF.
        CATCH zcx_adcoset_src_code_read.
      ENDTRY.
    ENDLOOP.

    zcl_adcoset_search_protocol=>increase_searchd_sources_count( searched_sources_count ).
  ENDMETHOD.

  METHOD get_class_includes.
    DATA(class_name) = CONV classname( name ).

    IF custom_settings-include_flags-public_section = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_pubsec_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-protected_section = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_prosec_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-private_section = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_prisec_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-methods = abap_true.
      cl_oo_classname_service=>get_all_method_includes( EXPORTING  clsname            = class_name
                                                        RECEIVING  result             = DATA(method_includes)
                                                        EXCEPTIONS class_not_existing = 1 ).

      SORT method_includes BY cpdkey-cpdname.
      result = VALUE #( BASE result FOR method IN method_includes
                        ( name        = method-incname
                          method_name = method-cpdkey-cpdname
                          adt_type    = c_include_types-method ) ).
    ENDIF.

    IF custom_settings-include_flags-local_def = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_ccdef_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-local_impl = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_ccimp_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-test = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_ccau_name( class_name ) ) ).
    ENDIF.

    IF custom_settings-include_flags-macro = abap_true.
      result = VALUE #( BASE result ( name = cl_oo_classname_service=>get_ccmac_name( class_name ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD assign_objects_to_matches.
    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<unassigned_match>).
      APPEND <unassigned_match> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-object_name     = include-name.
      <match>-adt_object_type = COND #(
        WHEN include-adt_type IS INITIAL THEN c_include_types-include ELSE include-adt_type ).

      " set the display name
      IF include-method_name IS NOT INITIAL.
        <match>-display_name = include-method_name.
      ELSEIF include-name+31 = seop_inccode_public.
        <match>-display_name = c_section_texts-public_section.
      ELSEIF include-name+31 = seop_inccode_private.
        <match>-display_name = c_section_texts-private_section.
      ELSEIF include-name+31 = seop_inccode_protected.
        <match>-display_name = c_section_texts-protected_section.
      ELSEIF include-name+30 = seop_incextapp_macros.
        <match>-display_name = c_section_texts-macros.
      ELSEIF include-name+30 = seop_incextapp_definition.
        <match>-display_name = c_section_texts-locals_def.
      ELSEIF include-name+30 = seop_incextapp_implementation.
        <match>-display_name = c_section_texts-local_impl.
      ELSEIF include-name+30 = seop_incextapp_testclasses.
        <match>-display_name = c_section_texts-test_cls.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
