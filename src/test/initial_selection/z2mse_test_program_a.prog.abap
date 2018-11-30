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
CALL FUNCTION 'Z2MSE_TEST_FUNCTION_B'.

" This class, attributes and methods are not in the selected package and have to be found with down search
DATA class_b1 TYPE REF TO z2mse_test_cl_b1.
class_b1->method_a( ).
DATA(t) = class_b1->attribute_a.

" This table is not in the selected package and has to be found with down search
SELECT * FROM z2mse_test_db_b INTO TABLE @DATA(db_b).
