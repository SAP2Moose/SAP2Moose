CLASS z2mse_sap_program DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add
      IMPORTING
        name      TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    "! Call once to set the parent package of a program
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.
  PRIVATE SECTION.
    DATA g_famix_module TYPE REF TO z2mse_famix_module.
ENDCLASS.

CLASS z2mse_sap_program IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_module EXPORTING model = model.
  ENDMETHOD.


  METHOD add.

    " SAP_2_FAMIX_5     Map program to FAMIX.Module
    g_famix_module->add( EXPORTING name = name IMPORTING id = id ).

  ENDMETHOD.


  METHOD set_parent_package.
    g_famix_module->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.

ENDCLASS.
