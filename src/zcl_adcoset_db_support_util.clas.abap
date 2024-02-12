"! <p class="shorttext synchronized">Utility for DB Support</p>
CLASS zcl_adcoset_db_support_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Checks if current database is supported by the Code Search</p>
    CLASS-METHODS is_db_supported
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_adcoset_db_support_util IMPLEMENTATION.
  METHOD is_db_supported.
    result = xsdbool(    sy-dbsys = zif_adcoset_c_global=>c_dbsys-oracle
                      OR sy-dbsys = zif_adcoset_c_global=>c_dbsys-hana
                      OR sy-dbsys = zif_adcoset_c_global=>c_dbsys-ms_sql ).
  ENDMETHOD.
ENDCLASS.
