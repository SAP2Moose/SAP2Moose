CLASS z2mse_somix_call DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_coupling
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS set_caller_called_relation
      IMPORTING
        element_id TYPE i
        caller_id  TYPE i
        called_id  TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_caller_called,
             caller_id TYPE i,
             called_id TYPE i,
           END OF ty_caller_called.

    DATA g_caller_called_ids TYPE HASHED TABLE OF ty_caller_called WITH UNIQUE KEY caller_id called_id.
ENDCLASS.



CLASS z2mse_somix_call IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Call'.
  ENDMETHOD.
  METHOD set_caller_called_relation.

    DATA ls_caller_called_id LIKE LINE OF g_caller_called_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_caller_called_id.
    ls_caller_called_id-caller_id = caller_id.
    ls_caller_called_id-called_id = called_id.
    INSERT ls_caller_called_id INTO TABLE g_caller_called_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'caller'
                                            reference_id   = caller_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'called'
                                            reference_id   = called_id ).

  ENDMETHOD.

ENDCLASS.
