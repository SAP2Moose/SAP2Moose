CLASS z2mse_extr3_elements_mock DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: last_element_id   TYPE z2mse_extr3_element_manager=>element_id_type READ-ONLY,
          last_associations TYPE z2mse_extr3_element_manager=>associations_type READ-ONLY.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3_elements_mock IMPLEMENTATION.
  METHOD make_model.
    last_element_id = element_id.
    last_associations = associations.
  ENDMETHOD.

ENDCLASS.
