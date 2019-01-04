CLASS ltcl_main DEFINITION DEFERRED.
"! Contains the main integration test of SAP2Moose
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      z2mse_test_initial_selection FOR TESTING RAISING cx_static_check,
      specific_search FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD z2mse_test_initial_selection.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_initial_selection( ).

  ENDMETHOD.



  METHOD specific_search.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search( ).

  ENDMETHOD.

  METHOD setup.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->setup( ).

  ENDMETHOD.


ENDCLASS.
