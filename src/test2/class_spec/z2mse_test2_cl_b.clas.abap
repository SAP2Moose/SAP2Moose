CLASS z2mse_test2_cl_b DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS method.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_test2_cl_b IMPLEMENTATION.
  METHOD method.
    DATA(a) = NEW z2mse_test2_cl_a( ).
    a->method( ).
    a->attribute = 'A'.
  ENDMETHOD.

ENDCLASS.
