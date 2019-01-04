CLASS ltcl_main DEFINITION DEFERRED.
"! Contains the main integration test of SAP2Moose
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      z2mse_test_initial_selection FOR TESTING RAISING cx_static_check,
      z2mse_test_initial_selection2 FOR TESTING RAISING cx_static_check,
      specific_search FOR TESTING RAISING cx_static_check,
      specific_search_program FOR TESTING RAISING cx_static_check,
      specific_search_function FOR TESTING RAISING cx_static_check,
      to_do FOR TESTING RAISING cx_static_check.
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


  METHOD to_do.

    cl_abap_unit_assert=>fail( msg = 'Implement Improve searching specific elements #103' ).

    cl_abap_unit_assert=>fail( msg = 'Decide what to do with the not found parent package by specific search: "Z2MSE_TEST2_M1' ).

  ENDMETHOD.

  METHOD z2mse_test_initial_selection2.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_initial_selection2( ).

  ENDMETHOD.

  METHOD specific_search_program.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_program( ).

  ENDMETHOD.

  METHOD specific_search_function.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_function( ).

  ENDMETHOD.

ENDCLASS.
