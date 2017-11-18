CLASS z2mse_extr3_model_builder_mock DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_model_builder
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS new_element_id REDEFINITION.
    DATA last_reported_new_element_id TYPE i READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_EXTR3_MODEL_BUILDER_MOCK IMPLEMENTATION.


  METHOD new_element_id.
    last_reported_new_element_id = i_element_id.
  ENDMETHOD.
ENDCLASS.
