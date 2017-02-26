CLASS z2mse_sap_access DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add_access
      IMPORTING
        used_attribute TYPE i
        using_method   TYPE i.
  PRIVATE SECTION.
    DATA: g_famix_access TYPE REF TO Z2MSE_famix_access.
ENDCLASS.

CLASS z2mse_sap_access IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_access EXPORTING model = model.
  ENDMETHOD.


  METHOD add_access.
    " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

    IF g_famix_access->is_new_access( accessor_id = using_method
                                      variable_id = used_attribute )
       EQ abap_true.
      DATA last_id TYPE i.
      last_id = g_famix_access->add( ).
      g_famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id
                                                                accessor_id = using_method
                                                                variable_id = used_attribute ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
