class Z2MSE_DYNAMIC_USAGE definition
  public
  create public .

public section.

    CLASS-METHODS where_used
      EXPORTING
        !data TYPE ANY TABLE .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_DYNAMIC_USAGE IMPLEMENTATION.
  METHOD where_used.

    " The dynamic usages of the SAP2Moose Extractor
    " Add name of this class in field "Classes to read dynmic usages" when
    " package Z2MSE (SAP2Moose) is extracted with SAP2Moose itself.

    DATA: dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY,
          line      TYPE wbcrossgt.

    CLEAR line.
    line-otype = 'ME'.
    line-name = 'Z2MSE_TEST_DYNAMIC_USAGE\ME:WHERE_USED'.
    line-include = 'Z2MSE_EXTR3_WHERE_USED_BUILDERCM003'.
    INSERT line INTO TABLE dyn_usage.

    data = dyn_usage.

  ENDMETHOD.
ENDCLASS.
