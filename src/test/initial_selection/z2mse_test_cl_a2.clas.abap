class Z2MSE_TEST_CL_A2 definition
  public
  inheriting from Z2MSE_TEST_CL_A
  create public .

public section.

  methods METHOD_A
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_CL_A2 IMPLEMENTATION.


  METHOD method_a.

    DATA class_b1 TYPE REF TO z2mse_test_cl_b1.

    class_b1->method_b( ).

  ENDMETHOD.
ENDCLASS.
