"! <p class="shorttext synchronized" lang="en">Code Search Engine</p>
CLASS zcl_adcoset_search_engine DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves instance of search engine</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_adcoset_search_engine.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Search source code</p>
      search_code
        IMPORTING
          search_config TYPE zif_adcoset_ty_global=>ty_search_settings_extended.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zcl_adcoset_search_engine.
ENDCLASS.



CLASS zcl_adcoset_search_engine IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD search_code.
    " 1) validate patterns
    " 2) create scope
    " 3) create processing packages (if parallel processing)
    " 4) process objects in scope
    " 5) collect / enrich results
  ENDMETHOD.

ENDCLASS.
