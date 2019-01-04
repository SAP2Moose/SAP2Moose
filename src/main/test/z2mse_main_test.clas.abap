CLASS z2mse_main_test DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: setup,
      z2mse_test_initial_selection,
      z2mse_test_initial_selection2,
      specific_search,
      specific_search_program,
      specific_search_function.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_main_test IMPLEMENTATION.


  METHOD setup.

    " This is mandatory to decouple the separate unit tests
    z2mse_extr3=>clear_all( ).

  ENDMETHOD.


  METHOD specific_search.

    " This test is a stub, it will probably be replaced by new tests

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = |Z2MSE_TEST_DYNAMIC_USAGE| ).


    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = z2mse_extr3_initial_elements=>select_class_method
                                                 i_parent_name_filter  = 'Z2MSE_TEST_CL_A'
                                                 i_name_filter         = 'EVENT_A' ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 1
                              i_search_down            = 1
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
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_A2| name = |METHOD_A| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_A| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |F-Z2MSE_TEST_FUNCTION_A| specific = |X| )
*( where = |S| level = 3 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |LZ2MSE_TEST_FGR_AF01| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = || name = |Z2MSE_TEST_PROGRAM_A| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |WebDynproController| parent_name = |Z2MSE_TEST_WDY_A| name = |COMPONENTCONTROLLER| specific = |X| )
*( where = |S| level = 2 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B1| name = |METHOD_B| specific = |X| )
*( where = |S| level = 3 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_B2| name = |METHOD_A| specific = |X| )
*( where = |I| level = 4 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST_CL_A| name = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| specific = |X| )
 ).

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct list of found elements' exp = fes_exp act = fes_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #( ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_class(      name = |Z2MSE_TEST_CL_A| parentpackage = || ).
    maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).

    maker->add_method(            method  = |CONSTRUCTOR| at_line = 11 ).

    maker->add_method(            method  = |EVENTHANDLER_A| at_line = 13 ).
    maker->add_method(            method  = |EVENT_A| at_line = 9 ).

    maker->add_method(            method  = |METHOD_A| at_line = 12 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |EVENT_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A2| used = |METHOD_A| ).

    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).

    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |METHOD_A| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_A2| parentpackage = || ).

    maker->add_method(            method  = |METHOD_A| at_line = 8 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_B| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_B1| parentpackage = || ).
    maker->add_method(            method  = |EVENT_A| at_line = 8 ).

    maker->add_attribute(         attribute = |ATTRIBUTE_A| at_line = 13 ).

    maker->add_method(            method  = |METHOD_A| at_line = 10 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).

    maker->add_method(            method  = |METHOD_B| at_line = 11 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_B2| parentpackage = || ).
    maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).
    maker->add_method(            method  = |METHOD_A| at_line = 12 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).

    maker->add_interface(  name = |Z2MSE_TEST_IF_A_00000000000000| parentpackage = || ).
    maker->add_interface_method(  method  = |EVENT_A_0000000000000000000000| at_line = 7 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->add_interface_method(  method  = |METHOD_A_000000000000000000000| at_line = 9 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->add_interface_attribute( attribute = |ATTRIBUTE_A_000000000000000000| at_line = 5 ).

    maker->add_program(   name = |Z2MSE_TEST_PROGRAM_A| parentpackage = || ).
    maker->usage(                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->usage(                       used_group  = |FGR-Z2MSE_TEST_FGR_A| used = |F-Z2MSE_TEST_FUNCTION_A| ).

    maker->add_bw_transformation( bw_transformation = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| at_line = 123 ).
    maker->usage(                                     used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).

    maker->add_function_group( name = |Z2MSE_TEST_FGR_A| parentpackage = || ). " TBD Add Parentpackage
    maker->add_function(              function = |Z2MSE_TEST_FUNCTION_A| ).
    maker->usage(                                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->add_function_group_include( include = |LZ2MSE_TEST_FGR_AF01| ).
    maker->usage(                                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).

    maker->add_web_dynpro_component( wda_name = |Z2MSE_TEST_WDY_A| parentpackage = || ).
    maker->add_web_dynpro_component_view(       view = |COMPONENTCONTROLLER| ).
    maker->usage(                                      used_group  = 'Z2MSE_TEST_CL_A' used = |METHOD_A| ).
    maker->add_web_dynpro_component_view(       view = |EMPTYVIEW| ).
    maker->add_web_dynpro_component_view(       view = |V_MAIN| ).
    maker->add_web_dynpro_component_view(       view = |W_MAIN| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

  ENDMETHOD.


  METHOD specific_search_function.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = |Z2MSE_TEST_DYNAMIC_USAGE| ).


    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = z2mse_extr3_initial_elements=>select_function
                                                 i_parent_name_filter  = ''
                                                 i_name_filter         = 'Z2MSE_TEST2_I_FUNCTION_A' ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 2
                              i_search_down            = 2
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: fes_act TYPE z2mse_extr3_model_builder=>found_elements_type,
          fes_exp TYPE z2mse_extr3_model_builder=>found_elements_type.


    model_builder->write_found_elements( EXPORTING write = abap_false
                                         IMPORTING fes   = fes_act ).

    fes_exp = VALUE #(
( where = |S| level = -2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_M2_FUNCTION_A| specific = |X| )
( where = |S| level = -1 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_M1_FUNCTION_A| specific = |X| )
( where = |I| level =  0 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_I_FUNCTION_A|  specific = |X| )
( where = |S| level =  1 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_P1_FUNCTION_A| specific = |X| )
( where = |S| level =  2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_P2_FUNCTION_A| specific = |X| )
).

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct list of found elements' exp = fes_exp act = fes_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #( ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_package( package = |Z2MSE_TEST2_INITIAL| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_I_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_I_FUNCTION_A| ).

*    " This is to be found with the down search for functions
*
    maker->add_package( package = |Z2MSE_TEST2_M1| ).
*
    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M1_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M1| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M1_FUNCTION_A| ).
*
    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_I_FGR_A|
                                         using       = |F-Z2MSE_TEST2_I_FUNCTION_A|
                                         used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

*    " This is now found with the up search for functions:
*
    maker->add_package( package = |Z2MSE_TEST2_P1| ).
*
    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P1_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_P1| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P1_FUNCTION_A| ).

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_I_FGR_A|
                                         used        = |F-Z2MSE_TEST2_I_FUNCTION_A| ).

    " Test downsearch for programs

*    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_I_PROGRAM|
*                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).

    " Test function used by program is found

*    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
*                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

    " Test class used by program is found

*    maker->add_class( EXPORTING          name          = |Z2MSE_TEST2_M1_CL_A|
*                                         parentpackage = || ). "Z2MSE_TEST2_M1

*    maker->add_method( EXPORTING         method  = |STATIC_METHOD_A|
*                                         at_line = 7 ).

*    maker->usage( EXPORTING              using_group = ||
*                                         using       = |Z2MSE_TEST2_I_PROGRAM|
*                                         used_group  = |Z2MSE_TEST2_M1_CL_A|
*                                         used        = |STATIC_METHOD_A| ).

*    " Test function used by function is found in down search
*
    maker->add_package( package = |Z2MSE_TEST2_M2| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M2_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M2| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M2_FUNCTION_A| ).

    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         using       = |F-Z2MSE_TEST2_M1_FUNCTION_A|
                                         used_group  = |FGR-Z2MSE_TEST2_M2_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M2_FUNCTION_A| ).

*    " Test function used by function is found in up search
*
    maker->add_package( package = |Z2MSE_TEST2_P2| ).
*
    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P2_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_P2| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P2_FUNCTION_A| ).

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_P1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_P1_FUNCTION_A| ).

    " Test program that uses a program is found in up search

*    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_P1_PROGRAM|
*                                         parentpackage = |Z2MSE_TEST2_P1| ).
*
*    maker->usage(  EXPORTING             used_group  = ||
*                                         used        = |Z2MSE_TEST2_I_PROGRAM| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.


  METHOD specific_search_program.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = |Z2MSE_TEST_DYNAMIC_USAGE| ).


    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = z2mse_extr3_initial_elements=>select_program
                                                 i_parent_name_filter  = ''
                                                 i_name_filter         = 'Z2MSE_TEST2_I_PROGRAM' ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 2
                              i_search_down            = 2
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: fes_act TYPE z2mse_extr3_model_builder=>found_elements_type,
          fes_exp TYPE z2mse_extr3_model_builder=>found_elements_type.


    model_builder->write_found_elements( EXPORTING write = abap_false
                                         IMPORTING fes   = fes_act ).

    fes_exp = VALUE #(
( where = |S| level = -2 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_M2_FUNCTION_A| specific = |X| )
( where = |S| level = -1 alternate_level = 0 element_type = |ABAPClassMethod|              parent_name = |Z2MSE_TEST2_M1_CL_A| name = |STATIC_METHOD_A| specific = |X| )
( where = |S| level = -1 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |F-Z2MSE_TEST2_M1_FUNCTION_A| specific = |X| )
( where = |I| level =  0 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |Z2MSE_TEST2_I_PROGRAM| specific = |X| )
( where = |S| level =  1 alternate_level = 0 element_type = |ABAPProgramOrFunctionOrSAPBW| parent_name = ||                    name = |Z2MSE_TEST2_P1_PROGRAM| specific = |X| )
).

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct list of found elements' exp = fes_exp act = fes_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #( ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_package( package = |Z2MSE_TEST2_INITIAL| ).

*    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_I_FGR_A|
*                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).
*
*    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_I_FUNCTION_A| ).

*    " This is to be found with the down search for functions
*
    maker->add_package( package = |Z2MSE_TEST2_M1| ).
*
    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M1_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M1| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M1_FUNCTION_A| ).
*
*    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_I_FGR_A|
*                                         using       = |F-Z2MSE_TEST2_I_FUNCTION_A|
*                                         used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
*                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

*    " This is now found with the up search for functions:
*
    maker->add_package( package = |Z2MSE_TEST2_P1| ).
*
*    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P1_FGR_A|
*                                         parentpackage = |Z2MSE_TEST2_P1| ).
*
*    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P1_FUNCTION_A| ).
*
*    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_I_FGR_A|
*                                         used        = |F-Z2MSE_TEST2_I_FUNCTION_A| ).

    " Test downsearch for programs

    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_I_PROGRAM|
                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).

    " Test function used by program is found

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

    " Test class used by program is found

    maker->add_class( EXPORTING          name          = |Z2MSE_TEST2_M1_CL_A|
                                         parentpackage = |Z2MSE_TEST2_M1| ). "

    maker->add_method( EXPORTING         method  = |STATIC_METHOD_A|
                                         at_line = 7 ).

    maker->usage( EXPORTING              using_group = ||
                                         using       = |Z2MSE_TEST2_I_PROGRAM|
                                         used_group  = |Z2MSE_TEST2_M1_CL_A|
                                         used        = |STATIC_METHOD_A| ).

*    " Test function used by function is found in down search
*
    maker->add_package( package = |Z2MSE_TEST2_M2| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M2_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M2| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M2_FUNCTION_A| ).

    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         using       = |F-Z2MSE_TEST2_M1_FUNCTION_A|
                                         used_group  = |FGR-Z2MSE_TEST2_M2_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M2_FUNCTION_A| ).

*    " Test function used by function is found in up search
*
*    maker->add_package( package = |Z2MSE_TEST2_P2| ).
*
*    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P2_FGR_A|
*                                         parentpackage = |Z2MSE_TEST2_P2| ).
*
*    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P2_FUNCTION_A| ).
*
*    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_P1_FGR_A|
*                                         used        = |F-Z2MSE_TEST2_P1_FUNCTION_A| ).

    " Test program that uses a program is found in up search

    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_P1_PROGRAM|
                                         parentpackage = |Z2MSE_TEST2_P1| ).

    maker->usage(  EXPORTING             used_group  = ||
                                         used        = |Z2MSE_TEST2_I_PROGRAM| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.


  METHOD z2mse_test_initial_selection.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA(top_packages) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_INITIAL_SELECTION' ) ).
    DATA(sub_packages_filter) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ).

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).
    initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                 sub_packages_filter    = sub_packages_filter
                                                 including_sub_packages = abap_true ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = |Z2MSE_TEST_DYNAMIC_USAGE| ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = -1
                              i_search_down            = 2
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #( ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_package( package = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_package( package = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_db_table(   name = |Z2MSE_TEST_A|     parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_db_table(   name = |Z2MSE_TEST_DB_B|  parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_A|  parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).

    maker->add_method(            method  = |CONSTRUCTOR| at_line = 11 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |EVENTHANDLER_A| ).

    maker->add_method(            method  = |EVENTHANDLER_A| at_line = 13 ).
    maker->add_method(            method  = |EVENT_A| at_line = 9 ).

    maker->add_method(            method  = |METHOD_A| at_line = 12 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |EVENT_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A2| used = |METHOD_A| ).
    maker->access(                          used_group  = |Z2MSE_TEST_A| used = |Z2MSE_TEST_A| ).

    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).

    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |METHOD_A| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_A2| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    maker->add_method(            method  = |METHOD_A| at_line = 8 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_B| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_A3| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    maker->add_method(            method  = |METHOD_A| at_line = 9 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->access(                          used = |Z2MSE_TEST_DB_B| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_B1| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_method(            method  = |EVENT_A| at_line = 8 ).

    maker->add_attribute(         attribute = |ATTRIBUTE_A| at_line = 13 ).

    maker->add_method(            method  = |METHOD_A| at_line = 10 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->access(                          used = |Z2MSE_TEST_A| ).
    maker->access(                          used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |ATTRIBUTE_A_000000000000000000| ).
    maker->access(                          used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).

    maker->add_method(            method  = |METHOD_B| at_line = 11 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).

    maker->add_class(      name = |Z2MSE_TEST_CL_B2| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).
    maker->add_method(            method  = |METHOD_A| at_line = 12 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).

    maker->add_interface(  name = |Z2MSE_TEST_IF_A_00000000000000| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_interface_method(  method  = |EVENT_A_0000000000000000000000| at_line = 7 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
    maker->add_interface_method(  method  = |METHOD_A_000000000000000000000| at_line = 9 ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
    maker->add_interface_attribute( attribute = |ATTRIBUTE_A_000000000000000000| at_line = 5 ).

    maker->add_interface(  name = |ZIWCI_2MSE_TEST_WDY_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_interface_method(  method = |WD_GET_API| at_line = 17 ).

    maker->add_web_dynpro_component( wda_name = |Z2MSE_TEST_WDY_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_web_dynpro_component_view(       view = |COMPONENTCONTROLLER| ).
    maker->usage(                                      used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                                      used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
    maker->usage(                                      used_group  = |ZIWCI_2MSE_TEST_WDY_A| used = |WD_GET_API| ).
    maker->access(                                     used = |Z2MSE_TEST_A| ).
    maker->add_web_dynpro_component_view(       view = |EMPTYVIEW| ).
    maker->add_web_dynpro_component_view(       view = |V_MAIN| ).
    maker->add_web_dynpro_component_view(       view = |W_MAIN| ).

    maker->add_program(   name = |Z2MSE_TEST_PROGRAM_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->usage(                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
    maker->access(               used_group  = |Z2MSE_TEST_CL_B1| used = |ATTRIBUTE_A| ).
    maker->access(               used = |Z2MSE_TEST_DB_B| ).
    maker->usage(                used = |Z2MSE_TEST_PROGRAM_B| ).
    maker->usage(                used = |Z2MSE_TEST_PROGRAM_C| ).
    maker->usage(                used = |Z2MSE_TEST_INCLUDE_A| ).
    maker->usage(                       used_group  = |FGR-Z2MSE_TEST_FGR_A| used = |F-Z2MSE_TEST_FUNCTION_A| ).
    maker->usage(                       used_group  = |FGR-Z2MSE_TEST_FGR_B| used = |F-Z2MSE_TEST_FUNCTION_B| ).
    maker->add_program(   name = |Z2MSE_TEST_PROGRAM_B| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_program(   name = |Z2MSE_TEST_PROGRAM_C| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->usage(                used = |Z2MSE_TEST_PROGRAM_D| ).
    maker->add_program(   name = |Z2MSE_TEST_PROGRAM_D| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_program(   name = |Z2MSE_TEST_INCLUDE_A| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_function_group( name = |Z2MSE_TEST_FGR_B| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
    maker->add_function(              function = |Z2MSE_TEST_FUNCTION_B| ).

    maker->add_bw_transformation( bw_transformation = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| at_line = 123 ).
    maker->usage(                                     used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
    maker->usage(                                     used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).

    maker->add_function_group( name = |Z2MSE_TEST_FGR_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
    maker->add_function(              function = |Z2MSE_TEST_FUNCTION_A| ).
    maker->usage(                                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
    maker->usage(                                used_group  = |FGR-Z2MSE_TEST_FGR_B| used = |F-Z2MSE_TEST_FUNCTION_B| ).
    maker->add_function_group_include( include = |LZ2MSE_TEST_FGR_AF01| ).
    maker->usage(                                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.


  METHOD z2mse_test_initial_selection2.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA(top_packages) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST2_INITIAL' ) ).
    DATA(sub_packages_filter) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ).

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).
    initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                 sub_packages_filter    = sub_packages_filter
                                                 including_sub_packages = abap_true ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = |Z2MSE_TEST_DYNAMIC_USAGE| ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 2 " There are elements 3 layers up, these are not to be found here
                              i_search_down            = 2 " There are elements 3 layers down, these are not to be found here
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #( ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_package( package = |Z2MSE_TEST2_INITIAL| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_I_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_I_FUNCTION_A| ).

    " This is to be found with the down search for functions

    maker->add_package( package = |Z2MSE_TEST2_M1| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M1_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M1| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M1_FUNCTION_A| ).

    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_I_FGR_A|
                                         using       = |F-Z2MSE_TEST2_I_FUNCTION_A|
                                         used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

    " This is now found with the up search for functions:

    maker->add_package( package = |Z2MSE_TEST2_P1| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P1_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_P1| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P1_FUNCTION_A| ).

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_I_FGR_A|
                                         used        = |F-Z2MSE_TEST2_I_FUNCTION_A| ).

    " Test downsearch for programs

    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_I_PROGRAM|
                                         parentpackage = |Z2MSE_TEST2_INITIAL| ).

    " Test function used by program is found

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M1_FUNCTION_A| ).

    " Test class used by program is found

    maker->add_class( EXPORTING          name          = |Z2MSE_TEST2_M1_CL_A|
                                         parentpackage = |Z2MSE_TEST2_M1| ).

    maker->add_method( EXPORTING         method  = |STATIC_METHOD_A|
                                         at_line = 7 ).

    maker->usage( EXPORTING              using_group = ||
                                         using       = |Z2MSE_TEST2_I_PROGRAM|
                                         used_group  = |Z2MSE_TEST2_M1_CL_A|
                                         used        = |STATIC_METHOD_A| ).

    " Test function used by function is found in down search

    maker->add_package( package = |Z2MSE_TEST2_M2| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_M2_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_M2| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_M2_FUNCTION_A| ).

    maker->usage( EXPORTING              using_group = |FGR-Z2MSE_TEST2_M1_FGR_A|
                                         using       = |F-Z2MSE_TEST2_M1_FUNCTION_A|
                                         used_group  = |FGR-Z2MSE_TEST2_M2_FGR_A|
                                         used        = |F-Z2MSE_TEST2_M2_FUNCTION_A| ).

    " Test function used by function is found in up search

    maker->add_package( package = |Z2MSE_TEST2_P2| ).

    maker->add_function_group( EXPORTING name          = |Z2MSE_TEST2_P2_FGR_A|
                                         parentpackage = |Z2MSE_TEST2_P2| ).

    maker->add_function( EXPORTING       function      = |Z2MSE_TEST2_P2_FUNCTION_A| ).

    maker->usage( EXPORTING              used_group  = |FGR-Z2MSE_TEST2_P1_FGR_A|
                                         used        = |F-Z2MSE_TEST2_P1_FUNCTION_A| ).

    " Test program that uses a program is found in up search

    maker->add_program( EXPORTING        name          = |Z2MSE_TEST2_P1_PROGRAM|
                                         parentpackage = |Z2MSE_TEST2_P1| ).

    maker->usage(  EXPORTING             used_group  = ||
                                         used        = |Z2MSE_TEST2_I_PROGRAM| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file' ).

  ENDMETHOD.
ENDCLASS.
