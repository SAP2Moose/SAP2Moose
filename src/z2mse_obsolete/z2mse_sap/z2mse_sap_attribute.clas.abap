

CLASS z2mse_sap_attribute DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS get_id
      IMPORTING
        class     TYPE clike
        attribute TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    METHODS add
      IMPORTING
        class     TYPE clike
        attribute TYPE clike.
  PRIVATE SECTION.
    DATA: g_famix_attribute TYPE REF TO Z2MSE_famix_attribute.
ENDCLASS.

CLASS z2mse_sap_attribute IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_attribute EXPORTING model = model.
  ENDMETHOD.


  METHOD get_id.
    " SAP_2_FAMIX_13        Mapp attributes of classes to FAMIX.Attribute
    " SAP_2_FAMIX_14        Mapp attributes of interfaces to FAMIX.Attribute
    id = g_famix_attribute->get_id( class     = class
                                    attribute = attribute ).
  ENDMETHOD.


  METHOD add.
    DATA last_id TYPE i.
    g_famix_attribute->add( EXPORTING name = attribute IMPORTING id = last_id ).
    g_famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                  parent_element = 'FAMIX.Class'
                                                  parent_name    = class ).
    g_famix_attribute->store_id( EXPORTING class     = class
                                           attribute = attribute ).

  ENDMETHOD.

ENDCLASS.
