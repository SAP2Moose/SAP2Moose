

CLASS z2mse_famix_namespace DEFINITION INHERITING FROM Z2MSE_famix_container_entity
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.

ENDCLASS.

CLASS z2mse_famix_namespace IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Namespace'.
  ENDMETHOD.

ENDCLASS.
