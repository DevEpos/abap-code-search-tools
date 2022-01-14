"! <p class="shorttext synchronized" lang="en">Configurable Package Processor</p>
INTERFACE zif_adcoset_conf_pack_proc
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Configures the package size of the processor</p>
    "! The driving factor will be the available number of parallel tasks
    configure_package_size
      IMPORTING
        max_task_count TYPE i.
ENDINTERFACE.
