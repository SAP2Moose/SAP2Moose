CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extract3 DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup FOR TESTING,
      z2mse_test_initial_selection FOR TESTING RAISING cx_static_check,
      specific_search FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD z2mse_test_initial_selection.

    z2mse_extr3=>clear_all( ).

    DATA: f_cut TYPE REF TO z2mse_extract3.
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


    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.
    CREATE OBJECT model_builder.


    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    CREATE OBJECT element_manager
      EXPORTING
        i_model_builder          = model_builder
        i_exclude_found_sap_intf = abap_true.

    model_builder->initialize( i_element_manager = element_manager ).

    f_cut = NEW #( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = -1
                              i_search_down            = -1
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
*( |FAMIX.Package| )
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
( |FAMIX.Class Z2MSE_TEST_IF_A parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
" New in 0.3.0 (Web Dynpros now also initially collected, not only added if used by something):
( |FAMIX.Class Z2MSE_TEST_WDY_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
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
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_B candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
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
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B signature METHOD_B| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER signature COMPONENTCONTROLLER| )
" New in 0.3.0 (Web Dynpros now also initially collected, not only added if used by something, so also not used controllers now in model):
( |FAMIX.Method Z2MSE_TEST_WDY_A>>EMPTYVIEW signature EMPTYVIEW| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>V_MAIN signature V_MAIN| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>W_MAIN signature W_MAIN| )

( |FAMIX.Method ZIWCI_2MSE_TEST_WDY_A>>WD_GET_API signature WD_GET_API| )
" New in 0.3.0 Programs are displayed
( |FAMIX.Class Z2MSE_TEST_PROGRAM_A modifiers ABAPProgram| )
( |FAMIX.Class Z2MSE_TEST_PROGRAM_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A signature Z2MSE_TEST_PROGRAM_A| )
( |FAMIX.Invocation sender Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
" New in 0.3.0 Functions are displayed as external name of the found include program
" The parent package of functions is not yet found, because the include is not in the TADIR
( |FAMIX.Class F-Z2MSE_TEST_FUNCTION_A modifiers ABAPProgram| )
( |FAMIX.Method F-Z2MSE_TEST_FUNCTION_A>>F-Z2MSE_TEST_FUNCTION_A signature F-Z2MSE_TEST_FUNCTION_A| )
( |FAMIX.Invocation sender F-Z2MSE_TEST_FUNCTION_A>>F-Z2MSE_TEST_FUNCTION_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
" New in 0.3.0 SAP BW transformations are partly extracted.
" Currently only if a usage to the generated program is found.
( |FAMIX.Class BW-ODSO-Z2MSET001-CUBE-Z2MSET002 modifiers ABAPProgram| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 signature BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>CONSTRUCTOR signature DUMMY| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )

    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.



  METHOD specific_search.

    z2mse_extr3=>clear_all( ).

    DATA: f_cut TYPE REF TO z2mse_extract3.
    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA: top_packages        TYPE z2mse_extr3_initial_elements=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extr3_initial_elements=>ty_s_pack.
    top_packages = VALUE #( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_INITIAL_SELECTION' ) ).
    sub_packages_filter = VALUE #( ).

    DATA: initial_elements TYPE REF TO z2mse_extr3_initial_elements.
    initial_elements = NEW #( ).

    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.
    CREATE OBJECT model_builder.


    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    CREATE OBJECT element_manager
      EXPORTING
        i_model_builder          = model_builder
        i_exclude_found_sap_intf = abap_true.

    model_builder->initialize( i_element_manager = element_manager ).


    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = 'class'
                                                 i_parent_name_filter  = 'Z2MSE_TEST_CL_A'
                                                 i_name_filter         = 'EVENT_A' ).

    f_cut = NEW #( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = -1
                              i_search_down            = -1
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: fes_act TYPE z2mse_extr3_model_builder=>found_elements_type,
          fes_exp TYPE z2mse_extr3_model_builder=>found_elements_type.


    model_builder->write_found_elements( EXPORTING write = abap_false
                                         IMPORTING fes   = fes_act ).

    fes_exp = VALUE #(
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassEvent| parent_name = |Z2MSE_TEST_CL_A| name = |EVENT_A| specific = |X| )
( where = |I| level = 1 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_A| name = |METHOD_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |F-Z2MSE_TEST_FUNCTION_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |Z2MSE_TEST_PROGRAM_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |WebDynproController| parent_name = |Z2MSE_TEST_WDY_A| name = |COMPONENTCONTROLLER| specific = |X| )
( where = |S| level = 3 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_B| specific = |X| )
( where = |S| level = 3 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B2| name = |METHOD_A| specific = |X| )
 ).

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct list of found elements' exp = fes_exp act = fes_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
*( |FAMIX.Package| )
( |FAMIX.Attribute Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~ATTRIBUTE_A| )
( |FAMIX.Attribute Z2MSE_TEST_IF_A>>ATTRIBUTE_A| )
( |FAMIX.Class BW-ODSO-Z2MSET001-CUBE-Z2MSET002 modifiers ABAPProgram| )
( |FAMIX.Class F-Z2MSE_TEST_FUNCTION_A modifiers ABAPProgram| )
( |FAMIX.Class Z2MSE_TEST_CL_A modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_IF_A isInterface true| )
( |FAMIX.Class Z2MSE_TEST_IF_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class Z2MSE_TEST_PROGRAM_A modifiers ABAPProgram| )
( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender F-Z2MSE_TEST_FUNCTION_A>>F-Z2MSE_TEST_FUNCTION_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>EVENT_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_B candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A>>EVENT_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~EVENT_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 signature BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )
( |FAMIX.Method F-Z2MSE_TEST_FUNCTION_A>>F-Z2MSE_TEST_FUNCTION_A signature F-Z2MSE_TEST_FUNCTION_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR signature CONSTRUCTOR| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature EVENTHANDLER_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~EVENT_A signature Z2MSE_TEST_IF_A~EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A~METHOD_A signature Z2MSE_TEST_IF_A~METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B signature METHOD_B| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_IF_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A signature Z2MSE_TEST_PROGRAM_A | )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER signature COMPONENTCONTROLLER| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>EMPTYVIEW signature EMPTYVIEW| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>V_MAIN signature V_MAIN| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>W_MAIN signature W_MAIN| )
    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

  METHOD setup.
    z2mse_extr3=>clear_all( ).
  ENDMETHOD.

ENDCLASS.
