"! I describe elements of type table
CLASS z2mse_extr3_tables DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id  TYPE z2mse_extr3_element_manager=>element_id_type,
             tabname TYPE tabname,
           END OF element_type.
ENDCLASS.

CLASS z2mse_extr3_tables IMPLEMENTATION.
ENDCLASS.
