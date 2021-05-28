class Z2MSE_TEST_CL_CC definition
  public
  inheriting from Z2MSE_TEST_CL_CP
  create public .

public section.

  methods Z2MSE_TEST_IF_C1~METHOD2
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_CL_CC IMPLEMENTATION.


  method Z2MSE_TEST_IF_C1~METHOD2.
*CALL METHOD SUPER->Z2MSE_TEST_IF_C1~METHOD2
*    .
    " Method is redefined
  endmethod.
ENDCLASS.
