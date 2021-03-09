*&---------------------------------------------------------------------*
*& Report zcearch_scanner
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcearch_scanner.

"! <p class="shorttext synchronized" lang="en">Code Scanner</p>
CLASS zcl_codesrch_code_scanner DEFINITION.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_include,
        program_name TYPE progname,
        object_type  TYPE trobjtype,
        object_name  TYPE sobj_name,
        method_name  TYPE seocpdname,

        func_name    TYPE rs38l_fnam,
        display_name TYPE string,
      END OF ty_include.

    TYPES BEGIN OF ty_match.
    INCLUDE TYPE ty_include AS include.
    TYPES: line_start TYPE i,
           col_start  TYPE i,
           line_end   TYPE i,
           col_end    TYPE i,
           match      TYPE string.
    TYPES END OF ty_match.

    TYPES:
      ty_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING
          packname     TYPE devclass
          include_subs TYPE abap_bool OPTIONAL,
      start_scan
        IMPORTING
          find_string TYPE string
          ignore_case TYPE abap_bool DEFAULT abap_true,
      print_results.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_bitmask TYPE x LENGTH 4,
      BEGIN OF ty_line_index,
        number               TYPE i,
        offset               TYPE i,
        method_name          TYPE seocpdname,
        previous_method_line TYPE i,
        is_class_def_end     TYPE abap_bool,
      END OF ty_line_index,
      ty_line_indexes TYPE TABLE OF ty_line_index WITH KEY number
                                                  WITH UNIQUE HASHED KEY offset COMPONENTS offset,
      BEGIN OF ty_source_code,
        code                    TYPE string,
        class_def_end_line      TYPE i,
        first_method_begin_line TYPE i,
        last_method_begin_line  TYPE i,
        line_indexes            TYPE ty_line_indexes,
      END OF ty_source_code,

      BEGIN OF ty_parse_settings,
        determine_class_def_begin TYPE abap_bool,
        determine_method_begin    TYPE abap_bool,
      END OF ty_parse_settings,

      BEGIN OF ty_method_pos,
        offset TYPE i,
        method TYPE seocpdname,
      END OF ty_method_pos.

    CONSTANTS:
      c_cls_locals_def        TYPE string VALUE `Local Class Definitions`,
      c_cls_local_impl        TYPE string VALUE `Local Types`,
      c_cls_macros            TYPE string VALUE `Macros`,
      c_cls_main_source       TYPE string VALUE `Main Source`,
      c_cls_test_cls          TYPE string VALUE `Test Class Definitions`,
      c_cls_public_section    TYPE string VALUE 'Public Section',
      c_cls_protected_section TYPE string VALUE 'Protected Section',
      c_cls_private_section   TYPE string VALUE 'Private Section',
      c_cls_method            TYPE string VALUE `Method `,

      BEGIN OF c_types,
        class TYPE trobjtype VALUE 'CLAS',
      END OF c_types.

    DATA:
      include_sub_packages  TYPE abap_bool,
      packname              TYPE devclass,
      tadir_objs            TYPE zif_dutils_ty_global=>ty_tadir_objects,
      matches               TYPE ty_matches,
      pattern               TYPE REF TO cl_abap_regex,
      out                   TYPE REF TO if_demo_output,
      line_sep              TYPE string,
      duration              TYPE timestamp,
      line_sep_length       TYPE i,
      scanned_include_count TYPE i.

    METHODS:
      load_classes,
      create_regex
        IMPORTING
          find_string TYPE string
          ignore_case TYPE abap_bool
        RAISING
          cx_sy_regex,
      scan_clas
        IMPORTING
          name        TYPE classname
          find_string TYPE string,
      search_include
        IMPORTING
          incl_info   TYPE ty_include
          find_string TYPE string
        RAISING
          cx_sy_matcher,
      get_include_source
        IMPORTING
          include       TYPE progname
          settings      TYPE ty_parse_settings OPTIONAL
        RETURNING
          VALUE(result) TYPE ty_source_code,
      get_line_index
        IMPORTING
          offset        TYPE i
          line_indexes  TYPE ty_line_indexes
        RETURNING
          VALUE(result) TYPE ty_line_index,
      get_end_line_of_match
        IMPORTING
          offset        TYPE i
          length        TYPE i
          start_line    TYPE ty_line_index
          line_indexes  TYPE ty_line_indexes
          line_count    TYPE i
        RETURNING
          VALUE(result) TYPE ty_line_index,
      search_main_class_source
        IMPORTING
          program_name TYPE programm
          class_name   TYPE classname
          find_string  TYPE string.

ENDCLASS.

CLASS zcl_codesrch_code_scanner IMPLEMENTATION.

  METHOD constructor.
    me->include_sub_packages = include_subs.
    me->packname = packname.

    load_classes( ).

    " currently hard coded
    me->line_sep = cl_abap_char_utilities=>cr_lf.
    me->line_sep_length = strlen( me->line_sep ).
  ENDMETHOD.

  METHOD start_scan.
    TRY.
        me->out = cl_demo_output=>new( ).
        create_regex(
          find_string = find_string
          ignore_case = ignore_case ).
        IF me->pattern IS INITIAL.
          RETURN.
        ENDIF.

        GET TIME STAMP FIELD DATA(start_time).

        LOOP AT me->tadir_objs INTO DATA(tadir_obj).
          CASE tadir_obj-type.

            WHEN c_types-class.
              scan_clas(
                name        = CONV #( tadir_obj-name )
                find_string = find_string ).

          ENDCASE.
        ENDLOOP.

        GET TIME STAMP FIELD DATA(end_time).
        me->duration = cl_abap_timestamp_util=>get_instance( )->tstmp_seconds_between(
          iv_timestamp0 = start_time
          iv_timestamp1 = end_time ).
      CATCH cx_sy_matcher
            cx_sy_regex INTO DATA(regex_error).
        me->out->write( regex_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD load_classes.

    DATA(tadir_reader) = zcl_dutils_reader_factory=>create_repo_reader( ).
    me->tadir_objs = tadir_reader->include_by_package(
        packages            = VALUE #( ( me->packname ) )
        resolve_subpackages = abap_true
    )->include_by_type( VALUE #( ( c_types-class ) )
    )->select( ).

  ENDMETHOD.

  METHOD create_regex.
    me->pattern = NEW cl_abap_regex(
      pattern       = find_string
      ignore_case   = ignore_case ).
  ENDMETHOD.

  METHOD scan_clas.
    DATA(cls_naming) = CAST if_oo_class_incl_naming( cl_oo_include_naming=>get_instance_by_cifkey( VALUE #( clsname = name ) ) ).

*    DATA(meth_includes) = cls_naming->get_all_method_includes( ).
*    LOOP AT meth_includes ASSIGNING FIELD-SYMBOL(<meth_incl>).
*      search_include(
*        incl_info   = VALUE #(
*          program_name = <meth_incl>-incname
*          object_type  = c_types-class
*          object_name  = name
*          method_name  = <meth_incl>-cpdkey-cpdname
*          display_name = c_cls_method && <meth_incl>-cpdkey-cpdname )
*        find_string = find_string ).
*    ENDLOOP.

    search_main_class_source(
      program_name = cls_naming->main_source
      class_name   = name
      find_string  = find_string ).
*    search_include(
*      incl_info   = VALUE #(
*        program_name = cls_naming->public_section
*        object_type  = c_types-class
*        object_name  = name
*        display_name = c_cls_public_section )
*      find_string = find_string ).
*    search_include(
*      incl_info   = VALUE #(
*        program_name = cls_naming->protected_section
*        object_type  = c_types-class
*        object_name  = name
*        display_name = c_cls_protected_section )
*      find_string = find_string ).
*    search_include(
*      incl_info   = VALUE #(
*        program_name = cls_naming->private_section
*        object_type  = c_types-class
*        object_name  = name
*        display_name = c_cls_private_section )
*      find_string = find_string ).
    search_include(
      incl_info   = VALUE #(
        program_name = cls_naming->locals_def
        object_type  = c_types-class
        object_name  = name
        display_name = c_cls_locals_def )
      find_string = find_string ).
    search_include(
      incl_info   = VALUE #(
        program_name = cls_naming->locals_imp
        object_type  = c_types-class
        object_name  = name
        display_name = c_cls_local_impl )
      find_string = find_string ).
    search_include(
      incl_info   = VALUE #(
        program_name = cls_naming->macros
        object_type  = c_types-class
        object_name  = name
        display_name = c_cls_macros )
      find_string = find_string ).
    search_include(
      incl_info   = VALUE #(
        program_name = cls_naming->tests
        object_type  = c_types-class
        object_name  = name
        display_name = c_cls_test_cls )
      find_string = find_string ).
  ENDMETHOD.

  METHOD search_main_class_source.
    DATA: next_line         TYPE ty_line_index,
          line_of_match     TYPE ty_line_index,
          end_line_of_match TYPE ty_line_index,
          start_col         TYPE i,
          end_col           TYPE i.

    DATA(source) = get_include_source(
     include  = program_name
     settings = VALUE #(
      determine_class_def_begin = abap_true
      determine_method_begin    = abap_true ) ).
    DATA(source_line_count) = lines( source-line_indexes ).
    me->scanned_include_count = me->scanned_include_count + 1.

    DATA(matcher) = me->pattern->create_matcher( text = source-code ).

    DATA(match_results) = matcher->find_all( ).

    LOOP AT match_results INTO DATA(match).
      CLEAR: line_of_match,
             next_line.

      line_of_match = get_line_index(
        line_indexes = source-line_indexes
        offset       = match-offset ).

      end_line_of_match = get_end_line_of_match(
        offset       = match-offset
        length       = match-length
        start_line   = line_of_match
        line_indexes = source-line_indexes
        line_count   = source_line_count ).

      start_col = match-offset - line_of_match-offset.
      end_col = match-offset + match-length - end_line_of_match-offset.



      APPEND INITIAL LINE TO me->matches ASSIGNING FIELD-SYMBOL(<match_ext>).
      <match_ext> = VALUE #(
        include     = VALUE #(
          program_name = program_name
          object_type  = c_types-class
          object_name  = class_name )
        line_start  = line_of_match-number
        col_start   = start_col
        line_end    = end_line_of_match-number
        col_end     = end_col
        match       = condense(
          replace(
            val  = source-code+match-offset(match-length)
            sub  = me->line_sep
            with = space occ = 0 ) ) ).

      IF line_of_match-is_class_def_end = abap_true OR
         line_of_match-method_name IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      " check if match is inside a method
      LOOP AT source-line_indexes ASSIGNING FIELD-SYMBOL(<method_line>) WHERE method_name IS NOT INITIAL
                                                                          AND number > <match_ext>-line_start.
        EXIT.
      ENDLOOP.

      IF sy-subrc <> 0.
        " check if line is after last method
        IF <match_ext>-line_start > source-last_method_begin_line.
          <match_ext>-method_name = source-line_indexes[ source-last_method_begin_line ]-method_name.
        ENDIF.
      ELSE.
        IF <method_line>-number > source-first_method_begin_line.
          <match_ext>-method_name = source-line_indexes[ <method_line>-previous_method_line ]-method_name.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD search_include.
    DATA: next_line         TYPE ty_line_index,
          line_of_match     TYPE ty_line_index,
          end_line_of_match TYPE ty_line_index,
          start_col         TYPE i,
          end_col           TYPE i.

    DATA(source) = get_include_source( incl_info-program_name ).
    DATA(source_line_count) = lines( source-line_indexes ).
    me->scanned_include_count = me->scanned_include_count + 1.

    DATA(matcher) = me->pattern->create_matcher( text = source-code ).

    DATA(match_results) = matcher->find_all( ).

    LOOP AT match_results INTO DATA(match).
      CLEAR: line_of_match,
             next_line.

      line_of_match = get_line_index(
        line_indexes = source-line_indexes
        offset       = match-offset ).

      end_line_of_match = get_end_line_of_match(
        offset       = match-offset
        length       = match-length
        start_line   = line_of_match
        line_indexes = source-line_indexes
        line_count   = source_line_count ).

      start_col = match-offset - line_of_match-offset.
      end_col = match-offset + match-length - end_line_of_match-offset.

      APPEND VALUE #(
        include     = incl_info
        line_start  = line_of_match-number
        col_start   = start_col
        line_end    = end_line_of_match-number
        col_end     = end_col
        match       = condense(
          replace(
            val  = source-code+match-offset(match-length)
            sub  = me->line_sep
            with = space occ = 0 ) ) ) TO me->matches.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_line_index.
    LOOP AT line_indexes ASSIGNING FIELD-SYMBOL(<line_index>) WHERE offset > offset.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      result = line_indexes[ <line_index>-number - 1 ].
    ELSE.
      " offset must be in the first number
      result = line_indexes[ 1 ].
    ENDIF.
  ENDMETHOD.

  METHOD get_end_line_of_match.
    IF start_line-number = line_count.
      result = start_line.
    ELSE.
      DATA(match_end) = offset + length.
      DATA(next_line) = line_indexes[ start_line-number + 1 ].
      IF match_end <= next_line-offset.
        result = start_line.
      ELSE.
        result = get_line_index(
          offset       = match_end
          line_indexes = line_indexes ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_include_source.
    DATA: include_source         TYPE TABLE OF string,
          line_offset            TYPE i,
          is_class_def_end       TYPE abap_bool,
          is_class_def_end_found TYPE abap_bool,
          method_name            TYPE seocpdname,
          previous_method_line   TYPE i,
          method_match           TYPE match_result.

    READ REPORT include INTO include_source.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    line_offset = 1.

    LOOP AT include_source ASSIGNING FIELD-SYMBOL(<code_line>).
      CLEAR: method_name,
             method_match,
             is_class_def_end.

      DATA(line_number) = sy-tabix.

      IF line_number = 1.
        result-code = <code_line>.
      ELSE.
        result-code = |{ result-code }{ me->line_sep }{ <code_line> }|.
      ENDIF.


      APPEND INITIAL LINE TO result-line_indexes ASSIGNING FIELD-SYMBOL(<line_index>).
      <line_index> = VALUE #(
        number           = line_number
        offset           = line_offset
        method_name      = method_name
        is_class_def_end = is_class_def_end ).

      line_offset = strlen( result-code ) + me->line_sep_length.

      IF is_class_def_end_found = abap_false AND settings-determine_class_def_begin = abap_true.
        IF <code_line> CP 'ENDCLASS.'.
          <line_index>-is_class_def_end = abap_true.
          is_class_def_end_found = abap_true.
          result-class_def_end_line = line_number.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF settings-determine_method_begin = abap_true.
        FIND FIRST OCCURRENCE OF REGEX `^\s*method\s+([\w~]+)\b` IN <code_line>
          IGNORING CASE
          RESULTS method_match.
        IF method_match IS NOT INITIAL.
          DATA(sub_match) = method_match-submatches[ 1 ].
          <line_index>-method_name = to_upper( <code_line>+sub_match-offset(sub_match-length) ).
          result-last_method_begin_line = line_number.
          <line_index>-previous_method_line = previous_method_line.
          previous_method_line = line_number.
          IF result-first_method_begin_line IS INITIAL.
            result-first_method_begin_line = line_number.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD print_results.
    CHECK me->out IS BOUND.

    IF me->scanned_include_count > 0.
      me->out->write( `Duration: ` && me->duration && `s` ).
      me->out->write( |Scanned Includes: { me->scanned_include_count }| ).
      me->out->write( |Matches { lines( me->matches ) }| ).
      me->out->write( me->matches ).
    ELSE.
      me->out->write( |No Matches found| ).
    ENDIF.
    me->out->display( ).
  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.
  DATA(scanner) = NEW zcl_codesrch_code_scanner( '$DB_BROWSER' ).
*  scanner->print_results( ).
  DATA(search_string) = `stuff`.
  DATA(regex) = `[^"]*+abc`.
  FIND ALL OCCURRENCES OF REGEX regex IN search_string
   RESULTS DATA(results).
