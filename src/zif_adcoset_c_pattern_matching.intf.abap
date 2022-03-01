"! <p class="shorttext synchronized" lang="en">Constants for pattern matching</p>
INTERFACE zif_adcoset_c_pattern_matching
  PUBLIC .

  CONSTANTS:
    "! Control sequences
    BEGIN OF c_pattern_ctrl_sequence,
      boundary_start TYPE string VALUE '(#b-start)',
      boundary_end   TYPE string VALUE '(#b-end)',
      match_start    TYPE string VALUE '(#m-start)',
      match_end      TYPE string VALUE '(#m-end)',
      exclude        TYPE string VALUE '(#exclude)',
    END OF c_pattern_ctrl_sequence,

    "! Control sequence flags
    BEGIN OF c_pattern_ctrl_flag,
      boundary_start TYPE zif_adcoset_ty_global=>ty_control_flags VALUE '00000001',
      boundary_end   TYPE zif_adcoset_ty_global=>ty_control_flags VALUE '00000002',
      match_start    TYPE zif_adcoset_ty_global=>ty_control_flags VALUE '00000004',
      match_end      TYPE zif_adcoset_ty_global=>ty_control_flags VALUE '00000008',
      exclude        TYPE zif_adcoset_ty_global=>ty_control_flags VALUE '00000010',
    END OF c_pattern_ctrl_flag.
ENDINTERFACE.
