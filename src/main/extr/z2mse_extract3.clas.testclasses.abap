CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extract3 DEFINITION LOCAL FRIENDS ltcl_main.
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
( |FAMIX.Class Z2MSE_TEST_CL_A2 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_A2 parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 isInterface true| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 modifiers ABAPGlobalInterface| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
" New in 0.3.0 (Web Dynpros now also initially collected, not only added if used by something):
( |FAMIX.Class Z2MSE_TEST_WDY_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A isInterface true| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )

( |FAMIX.FileAnchor ATTRIBUTE_A_000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=5,1| )
( |FAMIX.FileAnchor CONSTRUCTOR fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=11,1| )
( |FAMIX.FileAnchor EVENTHANDLER_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=13,1| )
( |FAMIX.FileAnchor EVENT_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=9,1| )
( |FAMIX.FileAnchor EVENT_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=8,1| )
( |FAMIX.FileAnchor EVENT_A_0000000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=7,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=12,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main#start=8,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=10,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main#start=12,1| )
( |FAMIX.FileAnchor METHOD_A_000000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=9,1| )
( |FAMIX.FileAnchor METHOD_B fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=11,1| )
( |FAMIX.FileAnchor WD_GET_API fileName adt://NPL/sap/bc/adt/oo/interfaces/ziwci_2mse_test_wdy_a/source/main#start=17,1| )

( |FAMIX.FileAnchor Z2MSE_TEST_CL_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_A2 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_B1 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_B2 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_IF_A_00000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main| )
( |FAMIX.FileAnchor ZIWCI_2MSE_TEST_WDY_A fileName adt://NPL/sap/bc/adt/oo/interfaces/ziwci_2mse_test_wdy_a/source/main| )
( |FAMIX.FileAnchor BW-ODSO-Z2MSET001-CUBE-Z2MSET002 fileName bwmt://NPL/sap/bw/modeling/trfn/123| )
( |FAMIX.FileAnchor F-Z2MSE_TEST_FUNCTION_A fileName adt://NPL/sap/bc/adt/functions/groups/z2mse_test_fgr_a/fmodules/z2mse_test_function_a| )
( |FAMIX.FileAnchor LZ2MSE_TEST_FGR_AF01 fileName adt://NPL/sap/bc/adt/functions/groups/z2mse_test_fgr_a/includes/lz2mse_test_fgr_af01| )
( |FAMIX.FileAnchor Z2MSE_TEST_PROGRAM_A fileName adt://NPL/sap/bc/adt/programs/programs/z2mse_test_program_a| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_A>>METHOD_A variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_B1>>METHOD_A variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Access accessor Z2MSE_TEST_CL_B1>>METHOD_A variable Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000| )
" New with #48:
( |FAMIX.Access accessor Z2MSE_TEST_CL_B1>>METHOD_A variable Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
( |FAMIX.Access accessor Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER variable Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Attribute Z2MSE_TEST_A>>Z2MSE_TEST_A| )
( |FAMIX.Attribute Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
"( |FAMIX.Attribute Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000| )
( |FAMIX.Attribute Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>CONSTRUCTOR candidates Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>EVENT_A signature DUMMY| )
" New with #62
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_B2>>METHOD_A signature DUMMY| )

" Outcomment the next to prepare issue #76
* ( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 candidates Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 signature DUMMY| )


" End new with #62
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A2>>METHOD_A signature DUMMY| ) " Correct order
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_B signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>CONSTRUCTOR signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 signature DUMMY| )
" New with #48:
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_B candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 candidates Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates Z2MSE_TEST_CL_A>>CONSTRUCTOR signature DUMMY| ) " Suddenly needed 04.11.2017 after regenerating WebDynpro
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates ZIWCI_2MSE_TEST_WDY_A>>WD_GET_API signature DUMMY| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR signature CONSTRUCTOR| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature EVENTHANDLER_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_A2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B signature METHOD_B| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 signature EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 signature METHOD_A_000000000000000000000| )
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
( |FAMIX.Class FGR-Z2MSE_TEST_FGR_A modifiers ABAPFunktionGroup| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A signature F-Z2MSE_TEST_FUNCTION_A| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 signature LZ2MSE_TEST_FGR_AF01| )
( |FAMIX.Invocation sender FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
" New in 0.3.0 SAP BW transformations are partly extracted.
" Currently only if a usage to the generated program is found.
( |FAMIX.Class BW-ODSO-Z2MSET001-CUBE-Z2MSET002 modifiers BWTransformation| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 signature BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>CONSTRUCTOR signature DUMMY| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )

( |FAMIX.Attribute Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_A sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_A2 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 sourceAnchor| )
( |FAMIX.Class ZIWCI_2MSE_TEST_WDY_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A2>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 sourceAnchor| )
( |FAMIX.Method ZIWCI_2MSE_TEST_WDY_A>>WD_GET_API sourceAnchor| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 sourceAnchor| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A sourceAnchor| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A sourceAnchor| )
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
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_A2| name = |METHOD_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |F-Z2MSE_TEST_FUNCTION_A| specific = |X| )
( where = |S| level = 3 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |LZ2MSE_TEST_FGR_AF01| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |Z2MSE_TEST_PROGRAM_A| specific = |X| )
( where = |S| level = 2 alternate_level = 0 element_type = |WebDynproController| parent_name = |Z2MSE_TEST_WDY_A| name = |COMPONENTCONTROLLER| specific = |X| )
( where = |S| level = 3 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_B| specific = |X| )
( where = |S| level = 3 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B2| name = |METHOD_A| specific = |X| )
( where = |I| level = 4 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_A| name = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| specific = |X| )
 ).

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct list of found elements' exp = fes_exp act = fes_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
*( |FAMIX.Package| )
( |FAMIX.Attribute Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
( |FAMIX.Class Z2MSE_TEST_CL_A2 modifiers ABAPGlobalClass| )
( |FAMIX.Attribute Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
"( |FAMIX.Attribute Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000| )
( |FAMIX.Class BW-ODSO-Z2MSET001-CUBE-Z2MSET002 modifiers BWTransformation| )
( |FAMIX.Class FGR-Z2MSE_TEST_FGR_A modifiers ABAPFunktionGroup| )
*( |FAMIX.Class F-Z2MSE_TEST_FUNCTION_A modifiers ABAPFunktionGroup| )
*( |FAMIX.Class LZ2MSE_TEST_FGR_AF01 modifiers ABAPFunktionGroup| )
( |FAMIX.Class Z2MSE_TEST_CL_A modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 modifiers ABAPGlobalClass| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 isInterface true| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 modifiers ABAPGlobalInterface| )
( |FAMIX.Class Z2MSE_TEST_PROGRAM_A modifiers ABAPProgram| )
( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_B1 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_B2 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main| )
( |FAMIX.FileAnchor Z2MSE_TEST_IF_A_00000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main| )

( |FAMIX.FileAnchor ATTRIBUTE_A_000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=5,1| )
( |FAMIX.FileAnchor CONSTRUCTOR fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=11,1| )
( |FAMIX.FileAnchor EVENTHANDLER_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=13,1| )
( |FAMIX.FileAnchor EVENT_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=9,1| )
( |FAMIX.FileAnchor EVENT_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=8,1| )
( |FAMIX.FileAnchor EVENT_A_0000000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=7,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=12,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main#start=8,1| )
( |FAMIX.FileAnchor Z2MSE_TEST_CL_A2 fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=10,1| )
( |FAMIX.FileAnchor METHOD_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main#start=12,1| )
( |FAMIX.FileAnchor METHOD_A_000000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=9,1| )
( |FAMIX.FileAnchor METHOD_B fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=11,1| )
( |FAMIX.FileAnchor BW-ODSO-Z2MSET001-CUBE-Z2MSET002 fileName bwmt://NPL/sap/bw/modeling/trfn/123| )
( |FAMIX.FileAnchor F-Z2MSE_TEST_FUNCTION_A fileName adt://NPL/sap/bc/adt/functions/groups/z2mse_test_fgr_a/fmodules/z2mse_test_function_a| )
( |FAMIX.FileAnchor LZ2MSE_TEST_FGR_AF01 fileName adt://NPL/sap/bc/adt/functions/groups/z2mse_test_fgr_a/includes/lz2mse_test_fgr_af01| )
( |FAMIX.FileAnchor Z2MSE_TEST_PROGRAM_A fileName adt://NPL/sap/bc/adt/programs/programs/z2mse_test_program_a| )
( |FAMIX.Invocation sender BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A>>EVENT_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_B2>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A>>METHOD_A candidates Z2MSE_TEST_CL_A2>>METHOD_A signature DUMMY| ) " Correct arrow here
( |FAMIX.Invocation sender Z2MSE_TEST_CL_A2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_B signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_B candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_CL_B2>>METHOD_A candidates Z2MSE_TEST_CL_B1>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 candidates Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 candidates Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Invocation sender Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 signature BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A signature F-Z2MSE_TEST_FUNCTION_A| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 signature LZ2MSE_TEST_FGR_AF01| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR signature CONSTRUCTOR| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature EVENTHANDLER_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_A2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A signature EVENT_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B signature METHOD_B| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A signature METHOD_A| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000 signature Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 signature EVENT_A_0000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 signature METHOD_A_000000000000000000000| )
( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A signature Z2MSE_TEST_PROGRAM_A | )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>COMPONENTCONTROLLER signature COMPONENTCONTROLLER| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>EMPTYVIEW signature EMPTYVIEW| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>V_MAIN signature V_MAIN| )
( |FAMIX.Method Z2MSE_TEST_WDY_A>>W_MAIN signature W_MAIN| )
( |FAMIX.Attribute Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_A sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_A2 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_B1 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_CL_B2 sourceAnchor| )
( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>CONSTRUCTOR sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENT_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_A2>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>EVENT_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B1>>METHOD_B sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_CL_B2>>METHOD_A sourceAnchor| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>F-Z2MSE_TEST_FUNCTION_A sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>EVENT_A_0000000000000000000000 sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 sourceAnchor| )
( |FAMIX.Method BW-ODSO-Z2MSET001-CUBE-Z2MSET002>>BW-ODSO-Z2MSET001-CUBE-Z2MSET002 sourceAnchor| )
( |FAMIX.Method FGR-Z2MSE_TEST_FGR_A>>LZ2MSE_TEST_FGR_AF01 sourceAnchor| )
( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A sourceAnchor| )
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
