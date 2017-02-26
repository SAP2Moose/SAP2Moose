
CLASS z2mse_sap_invocation DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add_invocation
      IMPORTING
        used_method  TYPE i
        using_method TYPE i.
  PRIVATE SECTION.
    DATA: g_famix_invocation TYPE REF TO z2mse_famix_invocation.
ENDCLASS.

CLASS z2mse_sap_invocation IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_invocation EXPORTING model = model.
  ENDMETHOD.


  METHOD add_invocation.
    " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
    IF g_famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method
                                                           candidates_id = used_method )
       EQ abap_true.

      DATA invocation_id TYPE i.
      invocation_id = g_famix_invocation->add( ).
      g_famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                 sender_id     = using_method
                                                                 candidates_id = used_method
                                                                 signature     = 'DUMMY' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
