

CLASS z2mse_sap_class DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    "! Add global class
    "! @parameters modifiers | will be available in FAMIX in the attribute modifiers
    METHODS add
      IMPORTING name_group                    TYPE clike OPTIONAL
                name                          TYPE clike
                modifiers                     TYPE clike
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i.
    "! Specify the parent program for a local class
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_program
      IMPORTING
        element_id  TYPE i
        sap_program TYPE clike.
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.
    METHODS is_interface
      IMPORTING
        element_id TYPE i.
    "! Add local class of a program
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter program | the name of the program the local class is part of
    "! @parameters modifiers | will be available in FAMIX in the attribute modifiers
    METHODS add_local
      IMPORTING
        program   TYPE clike
        name      TYPE any
        modifiers TYPE clike
      RETURNING
        VALUE(id) TYPE i.
  PRIVATE SECTION.
    DATA: g_famix_class TYPE REF TO Z2MSE_famix_class.
ENDCLASS.

CLASS z2mse_sap_class IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_class EXPORTING model = model.
  ENDMETHOD.

  METHOD add.
    " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
    " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
    g_famix_class->add( EXPORTING name_group             = ''
                                  name                   = name
                                  modifiers              = modifiers
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id                     = id ).
  ENDMETHOD.

  METHOD set_parent_program.

    " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

    g_famix_class->set_container( EXPORTING element_id = element_id
                                            container_element = 'FAMIX.Module'
                                            parent_container  = sap_program ).
  ENDMETHOD.

  METHOD set_parent_package.
    g_famix_class->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.


  METHOD is_interface.
    g_famix_class->is_interface( element_id = element_id ).
  ENDMETHOD.


  METHOD add_local.
    g_famix_class->add( EXPORTING name_group = program
                                  name       = name
                                  modifiers  = modifiers
                        IMPORTING id = id ).
  ENDMETHOD.

ENDCLASS.
