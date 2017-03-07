class Z2MSE_TEST_CL_A definition
  public
  create public .

public section.

  interfaces Z2MSE_TEST_IF_A .

  methods METHOD_A .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_CL_A IMPLEMENTATION.


  METHOD METHOD_A.

    DATA z2mse_test_a TYPE z2mse_test_a.

    SELECT SINGLE * FROM z2mse_test_a INTO z2mse_test_a.

  ENDMETHOD.
ENDCLASS.
