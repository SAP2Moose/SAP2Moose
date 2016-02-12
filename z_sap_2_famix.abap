******************************************** Begin Include Z_SAP_2_FAMIX ****************************

CLASS cl_sap_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING name_group                    TYPE string OPTIONAL
                name                          TYPE string
      EXPORTING VALUE(exists_already_with_id) TYPE i
      RETURNING VALUE(id)                     TYPE i.
    "! Specify the parent program for a local class
    METHODS set_parent_program
      IMPORTING
        sap_program TYPE string.
    METHODS set_parent_package
      IMPORTING
        parent_package TYPE string.
    METHODS is_interface.
  PRIVATE SECTION.
    DATA: g_famix_class TYPE REF TO cl_famix_class.
ENDCLASS.

CLASS cl_sap_class IMPLEMENTATION.
  METHOD constructor.
    g_famix_class = NEW cl_famix_class( model = model ).
  ENDMETHOD.

  METHOD add.
    id = g_famix_class->add( EXPORTING name_group             = name_group
                                       name                   = name
                             IMPORTING exists_already_with_id = exists_already_with_id ).
  ENDMETHOD.

  METHOD set_parent_program.

    " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

    g_famix_class->set_container( EXPORTING container_element = 'FAMIX.Module'
                                            parent_container  = sap_program ).
  ENDMETHOD.

  METHOD set_parent_package.
    g_famix_class->set_parent_package( parent_package = parent_package ).
  ENDMETHOD.


  METHOD is_interface.
    g_famix_class->is_interface( ).
  ENDMETHOD.

ENDCLASS.

******************************************** End Include Z_SAP_2_FAMIX ******************************