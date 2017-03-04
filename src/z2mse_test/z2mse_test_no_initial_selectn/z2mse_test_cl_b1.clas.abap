CLASS z2mse_test_cl_b1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS method_a.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_test_cl_b1 IMPLEMENTATION.
  METHOD method_a.

    DATA(a) = NEW z2mse_test_cl_a( ).
    a->method_a( ).
    DATA(interface_attribute) = a->z2mse_test_if_a~attribute_a.
    a->z2mse_test_if_a~method_a( ).

  ENDMETHOD.

ENDCLASS.
