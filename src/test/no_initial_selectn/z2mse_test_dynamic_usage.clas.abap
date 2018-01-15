CLASS z2mse_test_dynamic_usage DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS where_used
      EXPORTING
        !data TYPE ANY TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_test_dynamic_usage IMPLEMENTATION.


  METHOD where_used.

    " Example of a method to transfer dynamic usages to the extractor
    DATA: dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
          line      TYPE wbcrossgt.

    CLEAR line.
    line-otype = 'ME'.
    line-name = 'Z2MSE_TEST_CL_A\ME:CONSTRUCTOR'.
    line-include = 'Z2MSE_TEST_CL_B1==============CM002'.
    INSERT line INTO TABLE dyn_usage.

    CLEAR line.
    line-otype = 'ME'.
    line-name = 'Z2MSE_TEST_CL_A\ME:METHOD_A'.
    line-include = 'Z2MSE_TEST_CL_B1==============CM002'.
    INSERT line INTO TABLE dyn_usage.

    data = dyn_usage.

  ENDMETHOD.
ENDCLASS.
