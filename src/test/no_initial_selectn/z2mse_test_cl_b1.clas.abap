CLASS z2mse_test_cl_b1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    EVENTS event_a .

    METHODS method_a .
    METHODS method_b .

    DATA attribute_a.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_TEST_CL_B1 IMPLEMENTATION.


  METHOD method_a.

    DATA(a) = NEW z2mse_test_cl_a( ).
    a->method_a( ).
    DATA(interface_attribute) = a->z2mse_test_if_a_00000000000000~attribute_a_000000000000000000.
    a->z2mse_test_if_a_00000000000000~method_a_000000000000000000000( ).

    DATA z2mse_test_a TYPE z2mse_test_a.

    SELECT SINGLE * FROM z2mse_test_a INTO z2mse_test_a.

  ENDMETHOD.


  METHOD method_b.

    method_a( ).
    " Dynamic usage:
*    data(a) = new z2mse_test_cl_a( ).
*    a->method_a( ).

  ENDMETHOD.
ENDCLASS.
