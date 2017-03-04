"! Specifies a SAP method
CLASS z2mse_sap_method DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    "! Returns the ID for a given method of a global class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
        class     TYPE clike
        method    TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    "! Add a method for a global SAP class or a global SAP instance
    METHODS add
      IMPORTING
        class     TYPE clike
        method    TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    METHODS add_local_method
      IMPORTING
        class_name  TYPE clike
        class_id    TYPE i
        method_name TYPE clike
      RETURNING
        VALUE(id)   TYPE i.
  PRIVATE SECTION.
    DATA: g_famix_method TYPE REF TO z2mse_famix_method.

ENDCLASS.

CLASS z2mse_sap_method IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_method EXPORTING model = model.
  ENDMETHOD.


  METHOD get_id.
    id = g_famix_method->get_id( class  = class
                                 method = method ).
  ENDMETHOD.


  METHOD add.
    " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
    " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
    g_famix_method->add( EXPORTING name = method IMPORTING id = id ).

    " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
    " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( element_id = id
                                   signature = method ).

    g_famix_method->set_parent_type( EXPORTING element_id = id
                                               parent_element = 'FAMIX.Class'
                                               parent_name    = class ).

    g_famix_method->store_id( EXPORTING class  = class
                                        method = method ).

  ENDMETHOD.


  METHOD add_local_method.

    " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method
    g_famix_method->add( EXPORTING name_group = class_name " TBD Why name of class in name_group?
                                        name       = method_name
                                        IMPORTING id = id ).
    " SAP_2_FAMIX_43        Fill the attribute signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( element_id = id
                                   signature = method_name ).

    " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class
    g_famix_method->set_parent_type( EXPORTING element_id = id
                                               parent_element = 'FAMIX.Class'
                                               parent_id      =  class_id ).
  ENDMETHOD.

ENDCLASS.
