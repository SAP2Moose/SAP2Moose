class Z2MSE_TEST_DYNAMIC_USAGE definition
  public
  create public .

public section.

  class-methods WHERE_USED
    exporting
      !DATA type ANY TABLE .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_DYNAMIC_USAGE IMPLEMENTATION.


  METHOD WHERE_USED.

    " Example of a method to transfer dynamic usages to the extractor
    DATA: dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
          line      TYPE wbcrossgt.

    CLEAR line.
    line-otype = 'ME'.
    line-name = 'Z2MSE_TEST_CL_A\ME:METHOD_A'.
    line-include = 'Z2MSE_TEST_CL_B1==============CM002'.
    INSERT line INTO TABLE dyn_usage.

    data = dyn_usage.

  ENDMETHOD.
ENDCLASS.
