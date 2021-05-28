class Z2MSE_TEST_CL_CP definition
  public
  create public .

public section.

  interfaces Z2MSE_TEST_IF_C1 .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_CL_CP IMPLEMENTATION.


  method Z2MSE_TEST_IF_C1~METHOD2.
    " Method is redefined
  endmethod.
ENDCLASS.
