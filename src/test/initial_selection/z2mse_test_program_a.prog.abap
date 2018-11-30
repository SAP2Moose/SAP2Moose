*&---------------------------------------------------------------------*
*& Report Z2MSE_TEST_PROGRAM_A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z2mse_test_program_a.

DATA class_a TYPE REF TO z2mse_test_cl_a.
class_a->method_a( ).

SUBMIT z2mse_test_program_b AND RETURN.

CALL FUNCTION 'Z2MSE_TEST_FUNCTION_A'.

" This program is not in the selected package and has to be found with down search
SUBMIT z2mse_test_program_c AND RETURN.

" This include is not in the selected package and has to be found with down search
INCLUDE z2mse_test_include_a.

" This function is not in the selected package and has to be found with down search
call FUNCTION 'Z2MSE_TEST_FUNCTION_B'.
