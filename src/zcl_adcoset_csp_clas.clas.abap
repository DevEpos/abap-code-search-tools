"! <p class="shorttext synchronized" lang="en">Search Provider for Classes</p>
CLASS zcl_adcoset_csp_clas DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_code_search_prov.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of a class search provider</p>
      constructor
        IMPORTING
          search_settings TYPE zif_adcoset_ty_global=>ty_search_settings
          custom_settings TYPE zif_adcoset_ty_global=>ty_cls_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_section_texts,
        main_source       TYPE string VALUE `Main Source`,
        locals_def        TYPE string VALUE `Local Class Definitions`,
        local_impl        TYPE string VALUE `Local Types`,
        macros            TYPE string VALUE `Macros`,
        test_cls          TYPE string VALUE `Test Class Definitions`,
        public_section    TYPE string VALUE 'Public Section',
        protected_section TYPE string VALUE 'Protected Section',
        private_section   TYPE string VALUE 'Private Section',
        method            TYPE string VALUE `Method `,
      END OF c_section_texts.

    TYPES:
      BEGIN OF ty_class_incl,
        name        TYPE ris_v_prog_tadir-program_name,
        method_name TYPE ris_v_prog_tadir-method_name,
      END OF ty_class_incl,
      ty_class_includes TYPE STANDARD TABLE OF ty_class_incl WITH KEY name.

    DATA:
      custom_settings TYPE zif_adcoset_ty_global=>ty_cls_search_settings,
      search_settings TYPE zif_adcoset_ty_global=>ty_search_settings,
      matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
    METHODS:
      get_class_includes
        IMPORTING
          name          TYPE sobj_name
        RETURNING
          VALUE(result) TYPE ty_class_includes,
      assign_objects_to_matches
        IMPORTING
          unassigned_matches TYPE zif_adcoset_ty_global=>ty_search_matches
          object             TYPE zif_adcoset_ty_global=>ty_tadir_object
          include            TYPE ty_class_incl
        CHANGING
          all_matches        TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.



CLASS zcl_adcoset_csp_clas IMPLEMENTATION.


  METHOD constructor.
    me->search_settings = search_settings.
    me->custom_settings = custom_settings.
    me->matchers = matchers.
  ENDMETHOD.


  METHOD zif_adcoset_code_search_prov~search.

    DATA(class_includes) = get_class_includes( name = object-name ).
    IF class_includes IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT class_includes ASSIGNING FIELD-SYMBOL(<include>).
      TRY.
          DATA(source_code) = src_code_reader->get_source_code( name = <include>-name ).
          DATA(matches) = source_code->find_matches(
            matchers             = matchers
            match_all            = search_settings-match_all_patterns
            ignore_comment_lines = search_settings-ignore_comment_lines ).

          IF matches IS NOT INITIAL.
            assign_objects_to_matches(
              EXPORTING
                unassigned_matches = matches
                object             = object
                include            = <include>
              CHANGING
                all_matches        = result ).
          ENDIF.
        CATCH zcx_adcoset_src_code_read.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_class_includes.
    SELECT program_name AS name,
           method_name
      FROM ris_v_prog_tadir
      WHERE object_name = @name
        AND object_type = @zif_adcoset_c_global=>c_source_code_type-class
      INTO CORRESPONDING FIELDS OF TABLE @result.

    DELETE result WHERE name CP '*CP'.
  ENDMETHOD.


  METHOD assign_objects_to_matches.

    LOOP AT unassigned_matches ASSIGNING FIELD-SYMBOL(<unassigned_match>).
      APPEND <unassigned_match> TO all_matches ASSIGNING FIELD-SYMBOL(<match>).

      <match>-include = include-name.
      <match>-object_name = object-name.
      <match>-object_type = object-type.

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
