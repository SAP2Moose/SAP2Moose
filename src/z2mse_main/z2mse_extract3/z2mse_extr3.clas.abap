CLASS z2mse_extr3 DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
  PROTECTED SECTION.
    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO z2mse_extr3_element_manager.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3 IMPLEMENTATION.
  METHOD constructor.
    element_manager = i_element_manager.
  ENDMETHOD.
ENDCLASS.
