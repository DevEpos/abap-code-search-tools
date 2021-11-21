"! <p class="shorttext synchronized" lang="en">Default class search settings</p>
CLASS zcl_adcoset_cls_se_set_default DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      zif_adcoset_cls_search_settngs.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_cls_se_set_default IMPLEMENTATION.


  METHOD zif_adcoset_cls_search_settngs~is_search_local_def_incl.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_adcoset_cls_search_settngs~is_search_local_impl_incl.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_adcoset_cls_search_settngs~is_search_macro_incl.
    result = abap_true.
  ENDMETHOD.


  METHOD zif_adcoset_cls_search_settngs~is_search_test_incl.
    result = abap_true.
  ENDMETHOD.


ENDCLASS.
