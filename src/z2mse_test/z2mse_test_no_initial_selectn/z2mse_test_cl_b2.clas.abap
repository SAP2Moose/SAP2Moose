class Z2MSE_TEST_CL_B2 definition
  public
  final
  create public .

public section.

  interfaces Z2MSE_TEST_IF_A_00000000000000 .

  methods METHOD_A .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_TEST_CL_B2 IMPLEMENTATION.


  METHOD method_a.
    DATA(b1) = NEW z2mse_test_cl_b1( ).
    b1->method_a( ).
  ENDMETHOD.
ENDCLASS.
