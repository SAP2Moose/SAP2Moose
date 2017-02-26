

CLASS z2mse_famix_entity DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
  PROTECTED SECTION.
    DATA g_elementname TYPE string.
    DATA g_model TYPE REF TO z2mse_model.
    DATA g_last_used_id TYPE i.
ENDCLASS.

CLASS z2mse_famix_entity IMPLEMENTATION.

  METHOD constructor.
    g_model = model.
  ENDMETHOD.

ENDCLASS.
