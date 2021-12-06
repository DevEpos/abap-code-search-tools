"! <p class="shorttext synchronized" lang="en">Utilities for parallel processing</p>
CLASS zcl_adcoset_parl_proc_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Determines the maximum number of threads for group</p>
      determine_max_tasks
        IMPORTING
          server_group  TYPE rzlli_apcl OPTIONAL
        RETURNING
          VALUE(result) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_server_group_info,
        group     TYPE rzlli_apcl,
        max_tasks TYPE i,
      END OF ty_server_group_info.

    CONSTANTS:

      c_max_allowed_tasks TYPE i VALUE 32.

    CLASS-DATA:
      group_infos TYPE HASHED TABLE OF ty_server_group_info WITH UNIQUE KEY group.
ENDCLASS.



CLASS zcl_adcoset_parl_proc_utils IMPLEMENTATION.


  METHOD determine_max_tasks.
    DATA: group_info TYPE ty_server_group_info.

    ASSIGN group_infos[ group = server_group ] TO FIELD-SYMBOL(<group>).
    IF sy-subrc = 0.
      result = <group>-max_tasks.
    ELSE.

      group_info-group = server_group.

      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING
          group_name                     = server_group
        IMPORTING
          free_pbt_wps                   = group_info-max_tasks
        EXCEPTIONS
          invalid_group_name             = 1
          internal_error                 = 2
          pbt_env_already_initialized    = 3
          currently_no_resources_avail   = 4
          no_pbt_resources_found         = 5
          cant_init_different_pbt_groups = 6
          OTHERS                         = 7.
      IF sy-subrc <> 0.
        " fallback to running sequentially. If SPBT_INITIALIZE fails, check transactions
        " RZ12, SM50, SM21, SARFC
        group_info-max_tasks = 1.
      ENDIF.

      IF group_info-max_tasks > 1.
        group_info-max_tasks = group_info-max_tasks - 1.
      ENDIF.

      ASSERT group_info-max_tasks >= 1.

      " https://en.wikipedia.org/wiki/Amdahl%27s_law
      IF group_info-max_tasks > c_max_allowed_tasks.
        group_info-max_tasks = c_max_allowed_tasks.
      ENDIF.

      INSERT group_info INTO TABLE group_infos.
      result = group_info-max_tasks.
    ENDIF.

  ENDMETHOD.


ENDCLASS.
