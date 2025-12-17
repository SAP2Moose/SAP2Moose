"! I am the abstract super class of all elements.
"! My subclasses know the details of elements.
CLASS z2mse_extr3_elements DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3.

  PUBLIC SECTION.

    "! True if further informations are collected
    DATA infos_are_collected TYPE abap_bool.
    "! Collect further informations
    METHODS collect_infos IMPORTING sysid TYPE string.

    DATA type TYPE c LENGTH 30.

    CONSTANTS: package_type          LIKE type VALUE 'package',
               table_type            LIKE type VALUE 'table',
               class_type            LIKE type VALUE 'class',
               program_type          LIKE type VALUE 'program',
               web_dynpro_comps_type LIKE type VALUE 'web_dynpro_components'.

    METHODS make_model
      IMPORTING
        element_id   TYPE z2mse_extr3_element_manager=>element_id_type
        associations TYPE z2mse_extr3_element_manager=>associations_type.

    METHODS name
      IMPORTING
        element_id   TYPE z2mse_extr3_element_manager=>element_id_type
      EXPORTING
        element_type TYPE string
        parent_name  TYPE string
        name         TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS z2mse_extr3_elements IMPLEMENTATION.


  METHOD collect_infos.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.


  METHOD make_model.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.


  METHOD name.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.
