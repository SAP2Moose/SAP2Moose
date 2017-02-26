
CLASS z2mse_sap_inheritance DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add RETURNING VALUE(id) TYPE i.
    METHODS set_sub_and_super_class
      IMPORTING
        element_id      TYPE i
        subclass_name   TYPE clike
        superclass_name TYPE clike.
    METHODS set_interface_for_class
      IMPORTING
        element_id     TYPE i
        interface_name TYPE clike
        class_name     TYPE clike.
    METHODS set_local_sub_and_super_class
      IMPORTING
        element_id      TYPE i
        program         TYPE clike
        subclass_name   TYPE any
        superclass_name TYPE any.
  PRIVATE SECTION.
    DATA: g_famix_inheritance TYPE REF TO Z2MSE_famix_inheritance.
ENDCLASS.

CLASS z2mse_sap_inheritance IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_inheritance EXPORTING model = model.
  ENDMETHOD.


  METHOD add.
    id = g_famix_inheritance->add( ).
  ENDMETHOD.


  METHOD set_sub_and_super_class.

    " SAP_2_FAMIX_39     Map all inheritances between classes in selected packages to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = ''
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = ''
                                                            superclass_name       = superclass_name ).
  ENDMETHOD.


  METHOD set_interface_for_class.

    " SAP_2_FAMIX_40        Map all interface implementations of interfaces in selected packages by classes of selected packages by FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = ''
                                                            subclass_name         = interface_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = ''
                                                            superclass_name       = class_name ).
  ENDMETHOD.


  METHOD set_local_sub_and_super_class.

    " SAP_2_FAMIX_38        Map local inheritances of classes to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = program
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = program
                                                            superclass_name       = superclass_name ).
  ENDMETHOD.

ENDCLASS.
