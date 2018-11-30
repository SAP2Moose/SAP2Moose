class Z2MSE_TEST_CL_A3 definition
  public
  create public .

public section.
protected section.
private section.

  methods METHOD_A .
ENDCLASS.



CLASS Z2MSE_TEST_CL_A3 IMPLEMENTATION.


  METHOD method_a.
    DATA: class TYPE REF TO z2mse_test_cl_b1.
    class->method_a( ).

    DATA z2mse_test_db_b TYPE z2mse_test_db_b.

    SELECT SINGLE * FROM z2mse_test_db_b INTO z2mse_test_db_b.

  ENDMETHOD.
ENDCLASS.
