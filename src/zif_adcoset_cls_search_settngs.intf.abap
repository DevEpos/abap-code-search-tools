"! <p class="shorttext synchronized" lang="en">Code search settings for classes</p>
INTERFACE zif_adcoset_cls_search_settngs
  PUBLIC .

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'search_test_incl'</p>
    "! If this setting is active the test include of a class is considered during
    "! the search
    is_search_test_incl
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'search_macro_incl'</p>
    "! If this setting is active the 'Macros' include of a class is considered during
    "! the search
    is_search_macro_incl
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'search_local_def_incl'</p>
    "! If this setting is active the 'Class-relevant Local Types' include of a class is considered during
    "! the search
    is_search_local_def_incl
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns the setting value of 'search_local_impl_incl'</p>
    "! If this setting is active the 'Local Types' include of a class is considered during
    "! the search
    is_search_local_impl_incl
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.
