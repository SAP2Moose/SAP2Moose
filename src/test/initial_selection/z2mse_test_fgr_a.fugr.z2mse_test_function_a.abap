FUNCTION Z2MSE_TEST_FUNCTION_A.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  DATA class_a TYPE REF TO z2mse_test_cl_a.
  class_a->method_a( ).

  perform test2.

ENDFUNCTION.
