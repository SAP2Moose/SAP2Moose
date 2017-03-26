CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extract3 DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO z2mse_extract3.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check,
      z2mse_test_initial_selection FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD first_test.
    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  ENDMETHOD.

  METHOD z2mse_test_initial_selection.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA: top_packages        TYPE z2mse_extr3_initial_elements=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extr3_initial_elements=>ty_s_pack.
    top_packages = VALUE #( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_INITIAL_SELECTION' ) ).
    sub_packages_filter = VALUE #( ).

    DATA: initial_elements TYPE REF TO z2mse_extr3_initial_elements.
    initial_elements = NEW #( ).
    initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                 sub_packages_filter    = sub_packages_filter
                                                 including_sub_packages = abap_true ).

    f_cut = NEW #( ).
    f_cut->extract( EXPORTING initial_elements         = initial_elements
                              i_search_up              = -1
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
( |FAMIX.Package| )
( |FAMIX.Package Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Package Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_A modifiers DBTable| )
( |FAMIX.Class Z2MSE_TEST_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Class Z2MSE_TEST_CL_A modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_IF_A isInterface true| )
( |FAMIX.Class Z2MSE_TEST_IF_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class Z2MSE_TEST_IF_A parentPackage| )
( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A isInterface true| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_A>>METHOD_A variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_B1>>METHOD_A variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_B1>>METHOD_A variable Z2MSE_TEST_IF_A>>ATTRIBUTE_A| )
( |FAMIX.Access accessor Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Attribute Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Attribute Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~ATTRIBUTE_A| )
( |FAMIX.Attribute Z2MSE_TEST_IF_A>>ATTRIBUTE_A| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>CONSTRUCTOR candidates Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>EVENT_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>CONSTRUCTOR signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_IF_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A>>EVENT_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~EVENT_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates ZIWCI_2MSE_TEST_WDY_A>>WD_GET_API signature DUMMY| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR signature CONSTRUCTOR| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature EVENTHANDLER_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~EVENT_A signature Z2MSE_TEST_IF_A~EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~METHOD_A signature Z2MSE_TEST_IF_A~METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER signature COMPONENTCONTROLLER| )
( |FAMIX.Method ZIWCI_2MSE_TEST_WDY_A>>WD_GET_API signature WD_GET_API| )
    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

ENDCLASS.
