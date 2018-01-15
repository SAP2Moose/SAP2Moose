class Z2MSE_TEST_DYNAMIC_USAGE definition
  public
  create public .

public section.

  methods WHERE_USED
    returning
      value(DATA) type ref to DATA .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_TEST_DYNAMIC_USAGE IMPLEMENTATION.


  METHOD where_used.

    " Example of a method to transfer dynamic usages to the extractor

    DATA: dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
          line      TYPE wbcrossgt.

    CLEAR line.
    line-otype = 'ME'.
    line-name = 'Z2MSE_TEST_CL_A\ME:METHOD_A'.
    line-include = 'Z2MSE_TEST_CL_B1==============CM002'.
    INSERT line INTO TABLE dyn_usage.

    GET REFERENCE OF dyn_usage INTO data.

  ENDMETHOD.
ENDCLASS.
