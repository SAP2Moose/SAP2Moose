FUNCTION Z2MSE_TEST_FUNCTION_A.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  DATA class_a TYPE REF TO z2mse_test_cl_a.
  class_a->method_a( ).

  PERFORM test2.

  " This function is not in the selected package and has to be found with down search
  CALL FUNCTION 'Z2MSE_TEST_FUNCTION_B'.

ENDFUNCTION.
