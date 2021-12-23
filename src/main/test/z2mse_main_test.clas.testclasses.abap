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
      z2mse_test_2021 FOR TESTING RAISING cx_static_check,
      specific_search_method FOR TESTING RAISING cx_static_check,
      specific_search_attribute FOR TESTING RAISING cx_static_check,
      specific_search_class FOR TESTING RAISING cx_static_check,
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

  METHOD specific_search_method.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_method( ).

  ENDMETHOD.

  METHOD specific_search_attribute.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_attribute( ).

  ENDMETHOD.

  METHOD specific_search_class.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_class( ).

  ENDMETHOD.

  METHOD setup.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->setup( ).

  ENDMETHOD.


  METHOD to_do.

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

  METHOD z2mse_test_2021.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_2021( ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_main_somix DEFINITION DEFERRED.
"! Contains the main integration test of SAP2Moose
CLASS ltcl_main_somix DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      z2mse_test_init_select_somix FOR TESTING RAISING cx_static_check,
      z2mse_test_init_select_somix2 FOR TESTING RAISING cx_static_check,
      z2mse_test_2021_somix FOR TESTING RAISING cx_static_check,
      specific_search_method_somix FOR TESTING RAISING cx_static_check,
      specific_search_attr_somix FOR TESTING RAISING cx_static_check,
      specific_search_class_somix FOR TESTING RAISING cx_static_check,
      specific_search_program_somix FOR TESTING RAISING cx_static_check,
      specific_search_function_somix FOR TESTING RAISING cx_static_check,
      to_do FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main_somix IMPLEMENTATION.

  METHOD z2mse_test_init_select_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_init_select_somix( ).

  ENDMETHOD.

  METHOD specific_search_method_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_method( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD specific_search_attr_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_attribute( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD specific_search_class_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_class( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD setup.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->setup( ).

  ENDMETHOD.


  METHOD to_do.

  ENDMETHOD.

  METHOD z2mse_test_init_select_somix2.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_initial_selection2( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD specific_search_program_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_program( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD specific_search_function_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->specific_search_function( ).##TODO " Reimplement

  ENDMETHOD.

  METHOD z2mse_test_2021_somix.

    " The SAP2Moose extractor does not analyze test methods.
    " Performing the test in the normal methods add them to the extracted model data.

    DATA(test) = NEW z2mse_main_test( ).
    test->z2mse_test_2021( ).##TODO " Reimplement
  ENDMETHOD.

ENDCLASS.
