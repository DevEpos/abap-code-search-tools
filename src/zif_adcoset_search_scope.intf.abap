"! <p class="shorttext synchronized" lang="en">Defines the scope of a code search</p>
INTERFACE zif_adcoset_search_scope
  PUBLIC .

  METHODS:
**    "! <p class="shorttext synchronized" lang="en">Returns the packages of the scope</p>
**    get_packages
**      RETURNING
**        VALUE(result) TYPE zif_adcoset_ty_global=>ty_package_names,
**
**    "! <p class="shorttext synchronized" lang="en">Returns the types of the scope</p>
**    get_object_types
**      RETURNING
**        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_types,
**
**    "! <p class="shorttext synchronized" lang="en">Returns the object names of the scope</p>
**    get_object_names
**      RETURNING
**        VALUE(result) TYPE zif_adcoset_ty_global=>ty_obj_names.
    "! <p class="shorttext synchronized" lang="en">Retrieve relevant objects for search</p>
    get_objects
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_objects.
ENDINTERFACE.
