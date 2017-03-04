CLASS z2mse_test_cl_b2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS method_a.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_test_cl_b2 IMPLEMENTATION.
  METHOD method_a.
    DATA(b1) = NEW z2mse_test_cl_b1( ).
    b1->method_a( ).
  ENDMETHOD.

ENDCLASS.
