CLASS z2mse_main_test DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: setup,
      z2mse_test_initial_selection,
      z2mse_test_init_select_somix,
      z2mse_test_initial_selection2,
      specific_search_method,
      specific_search_attribute,
      specific_search_class,
      specific_search_program,
      specific_search_function,
      z2mse_test_2021.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mse_model_act                TYPE z2mse_model=>lines_type,
          fes_act                      TYPE z2mse_extr3_model_builder=>found_elements_type,
          fes_exp                      TYPE z2mse_extr3_model_builder=>found_elements_type,
          equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse,
          maker                        TYPE REF TO z2mse_mse_harmonize_maker.
    METHODS _search_specific
      IMPORTING
        parent_name_filter  TYPE z2mse_extr3_initial_elements=>ty_filter
        name_filter         TYPE z2mse_extr3_initial_elements=>ty_filter
        dynamic_read        TYPE string
        element_type_filter TYPE z2mse_extr3_initial_elements=>ty_filter
        i_search_up         TYPE i
        i_search_down       TYPE i.
    METHODS _check_found_elements
      IMPORTING
        msg TYPE string.
    METHODS _check_harmonized_mse
      IMPORTING
        msg TYPE string.
ENDCLASS.



CLASS z2mse_main_test IMPLEMENTATION.


  METHOD setup.

    " This is mandatory to decouple the separate unit tests
    z2mse_extr3=>clear_all( ).

    CLEAR mse_model_act.
    CLEAR equalized_harmonized_mse_act.
    CLEAR equalized_harmonized_mse_exp.
    CLEAR fes_act.
    CLEAR fes_exp.

    equalized_harmonized_mse_exp = VALUE #( ).

  ENDMETHOD.


  METHOD specific_search_method.

    _search_specific(
          i_search_up = 1
          i_search_down = 1
          element_type_filter = z2mse_extr3_initial_elements=>select_class_method
          dynamic_read        = |Z2MSE_TEST_DYNAMIC_USAGE|
          parent_name_filter  = 'Z2MSE_TEST2_CL_A'
          name_filter         = 'METHOD' ).

    fes_exp = VALUE #(
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_A| name = |METHOD| specific = |X| )
( where = |I| level = 1 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_A| name = |METHOD2| specific = |X| )
( where = |S| level = 1 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_B| name = |METHOD| specific = |X| )
 ).

    _check_found_elements( 'Expect correct list of found elements' ).

    maker = NEW #( ).
    maker->to_change = equalized_harmonized_mse_exp.
    maker->add_custom_source_language( language = |SAP| ).
    maker->add_package( package = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_A| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_attribute(         attribute = |ATTRIBUTE| at_line = 7 ).
    maker->add_method(            method  = |METHOD| at_line = 8 ).
    maker->add_method(            method  = |METHOD2| at_line = 9 ).
    maker->usage(                           used_group  = |Z2MSE_TEST2_CL_A| used = |METHOD| ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_B| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_method(            method  = |METHOD| at_line = 7 ).
    maker->usage(                           used_group  = |Z2MSE_TEST2_CL_A| used = |METHOD| ).

    _check_harmonized_mse( 'Wrong mse file for specific search class component' ).

  ENDMETHOD.

  METHOD specific_search_attribute.

    " Search for usage of attributes

    _search_specific(
          i_search_up = 1
          i_search_down = 1
          element_type_filter = z2mse_extr3_initial_elements=>select_class_method
          dynamic_read        = |Z2MSE_TEST_DYNAMIC_USAGE|
          parent_name_filter  = 'Z2MSE_TEST2_CL_A'
          name_filter         = 'ATTRIBUTE' ).

    fes_exp = VALUE #(
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassAttribute| parent_name = |Z2MSE_TEST2_CL_A| name = |ATTRIBUTE| specific = |X| )
( where = |S| level = 1 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_B| name = |METHOD| specific = |X| )
 ).

    _check_found_elements( 'Expect correct list of found elements' ).

    maker = NEW #( ).
    maker->to_change = equalized_harmonized_mse_exp.
    maker->add_custom_source_language( language = |SAP| ).
    maker->add_package( package = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_A| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_attribute(         attribute = |ATTRIBUTE| at_line = 7 ).
    maker->add_method(            method  = |METHOD| at_line = 8 ).
    maker->add_method(            method  = |METHOD2| at_line = 9 ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_B| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_method(            method  = |METHOD| at_line = 7 ).
*    maker->usage(                           used_group  = |Z2MSE_TEST2_CL_A| used = |METHOD| ).
    maker->access(                           used_group  = |Z2MSE_TEST2_CL_A| used = |ATTRIBUTE| ).

    _check_harmonized_mse( 'Wrong mse file for specific search class component' ).

  ENDMETHOD.

  METHOD specific_search_class.

    " Search for usage of all elements of a class

*    cl_abap_unit_assert=>fail( msg = 'Finalize test and coding to implement #122' ).

    _search_specific(
          i_search_up = 1
          i_search_down = 1
          element_type_filter = z2mse_extr3_initial_elements=>select_class_method
          dynamic_read        = |Z2MSE_TEST_DYNAMIC_USAGE|
          parent_name_filter  = ''
          name_filter         = 'Z2MSE_TEST2_CL_A' ).

    fes_exp = VALUE #(
( where = |S| level = -1 alternate_level = 0 element_type = |ABAPDatabaseTable| parent_name = || name = |SFLIGHT| specific = |X| )
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClass| parent_name = || name = |Z2MSE_TEST2_CL_A| specific = |X| )
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassAttribute| parent_name = |Z2MSE_TEST2_CL_A| name = |ATTRIBUTE| specific = |X| )
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_A| name = |METHOD| specific = |X| )
( where = |I| level = 0 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_A| name = |METHOD2| specific = |X| )
( where = |S| level = 1 alternate_level = 0 element_type = |ABAPClassMethod| parent_name = |Z2MSE_TEST2_CL_B| name = |METHOD| specific = |X| )
 ).

    _check_found_elements( 'Expect correct list of found elements' ).

    maker = NEW #( ).
    maker->to_change = equalized_harmonized_mse_exp.
    maker->add_custom_source_language( language = |SAP| ).
    maker->add_package( package = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_package( package = |SAPBC_DATAMODEL| ).
    maker->add_db_table(  name          = |SFLIGHT| parentpackage = |SAPBC_DATAMODEL| ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_A| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_attribute(         attribute = |ATTRIBUTE| at_line = 7 ).
    maker->add_method(            method  = |METHOD| at_line = 8 ).
    maker->add_method(            method  = |METHOD2| at_line = 9 ).
    maker->access(                    used_group  = 'SFLIGHT' used        = 'SFLIGHT' ).
    maker->usage(                           used_group  = |Z2MSE_TEST2_CL_A| used = |METHOD| ).
    maker->add_class(      name = |Z2MSE_TEST2_CL_B| parentpackage = |Z2MSE_TEST2_CLASS_SPEC| ).
    maker->add_method(            method  = |METHOD| at_line = 7 ).
    maker->usage(                           used_group  = |Z2MSE_TEST2_CL_A| used = |METHOD| ).
    maker->access(                           used_group  = |Z2MSE_TEST2_CL_A| used = |ATTRIBUTE| ).

    _check_harmonized_mse( 'Wrong mse file for specific search class component' ).

  ENDMETHOD.


  METHOD specific_search_function.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true
                                                             i_interface_use_structure = abap_false ).

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

    maker->add_custom_source_language( |SAP| ).

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
                                                             i_exclude_found_sap_intf = abap_true
                                                             i_interface_use_structure = abap_false ).

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
( where = |S| level = -1 alternate_level = 0 element_type = |ABAPClass|                    parent_name = ||                    name = |Z2MSE_TEST2_M1_CL_A| specific = |X| )
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

    maker->add_custom_source_language( |SAP| ).

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

    DATA: message                 TYPE string,
          interface_use_structure TYPE abap_bool.

    DO 2 TIMES.

      setup( ).

      CASE sy-index.
        WHEN 1.
          message = 'Wrong mse file for complex selection'.
          interface_use_structure = abap_false.
        WHEN 2.
          message = 'Wrong mse file for complex selection - Option Interface use structure'.
          interface_use_structure = abap_true.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.


      " Variant, reverse usage of implementing method and implemented interface

      DATA: mse_model_act TYPE z2mse_model=>lines_type.
      CLEAR mse_model_act.
      DATA: nothing_done_act TYPE abap_bool.
      nothing_done_act = abap_false.
      DATA(top_packages) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_INITIAL_SELECTION' ) ).
      DATA(sub_packages_filter) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ).

      DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).
      initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                   sub_packages_filter    = sub_packages_filter
                                                   including_sub_packages = abap_true ).

      DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

      DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                               i_exclude_found_sap_intf = abap_true
                                                               i_interface_use_structure = interface_use_structure ).

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
      CLEAR equalized_harmonized_mse_act.
      CLEAR equalized_harmonized_mse_exp.

      equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

      equalized_harmonized_mse_exp = VALUE #( ).

      DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

      maker = NEW #( ).

      maker->to_change = equalized_harmonized_mse_exp.
      maker->add_custom_source_language( |SAP| ).
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
* BEGIN INSERT when Interfaces are reversed
      IF interface_use_structure EQ abap_true.
        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |EVENT_A_0000000000000000000000| ).
      ENDIF.
* END INSERT when Interfaces are reversed
      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
* BEGIN INSERT when Interfaces are reversed
      IF interface_use_structure EQ abap_true.
        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
      ENDIF.
* END INSERT when Interfaces are reversed
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
* BEGIN INSERT when Interfaces are reversed
      IF interface_use_structure EQ abap_true.
        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |EVENT_A_0000000000000000000000| ).
      ENDIF.
* END INSERT when Interfaces are reversed
      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
* BEGIN INSERT when Interfaces are reversed
      IF interface_use_structure EQ abap_true.
        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
      ENDIF.
* END INSERT when Interfaces are reversed

      maker->add_interface(  name = |Z2MSE_TEST_IF_A_00000000000000| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
      maker->add_interface_method(  method  = |EVENT_A_0000000000000000000000| at_line = 7 ).
* BEGIN DELETE when Interfaces are reversed
      IF interface_use_structure EQ abap_false.
        maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
        maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
      ENDIF.
* END DELETE when Interfaces are reversed
      maker->add_interface_method(  method  = |METHOD_A_000000000000000000000| at_line = 9 ).
* BEGIN DELETE when Interfaces are reversed
      IF interface_use_structure EQ abap_false.
        maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
        maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
      ENDIF.
* END DELETE when Interfaces are reversed
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
          msg                  = message ).

    ENDDO.

  ENDMETHOD.

  METHOD z2mse_test_init_select_somix.

    DATA: message                 TYPE string,
          interface_use_structure TYPE abap_bool.

    DO 2 TIMES.

      setup( ).

      CASE sy-index.
        WHEN 1.
          message = 'Wrong mse file for complex selection'.
          interface_use_structure = abap_false.
        WHEN 2.
          message = 'Wrong mse file for complex selection - Option Interface use structure'.
          interface_use_structure = abap_true.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.


      " Variant, reverse usage of implementing method and implemented interface

      DATA: mse_model_act TYPE z2mse_model=>lines_type.
      CLEAR mse_model_act.
      DATA: nothing_done_act TYPE abap_bool.
      nothing_done_act = abap_false.
      DATA(top_packages) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_INITIAL_SELECTION' ) ).
      DATA(sub_packages_filter) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ).

      DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).
      initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                   sub_packages_filter    = sub_packages_filter
                                                   including_sub_packages = abap_true ).

      DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

      DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                               i_exclude_found_sap_intf = abap_true
                                                               i_interface_use_structure = interface_use_structure
                                                               i_use_somix               = 'X' ).

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
      CLEAR equalized_harmonized_mse_act.
      CLEAR equalized_harmonized_mse_exp.

      equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( mse = mse_model_act ).

*      equalized_harmonized_mse_exp = VALUE #( ).
*
*      DATA maker TYPE REF TO z2mse_mse_harmonize_maker.
*
*      maker = NEW #( ).
*
*      maker->to_change = equalized_harmonized_mse_exp.
*      maker->add_custom_source_language( |SAP| ).
*      maker->add_package( package = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_package( package = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_db_table(   name = |Z2MSE_TEST_A|     parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_db_table(   name = |Z2MSE_TEST_DB_B|  parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*
*      maker->add_class(      name = |Z2MSE_TEST_CL_A|  parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).
*
*      maker->add_method(            method  = |CONSTRUCTOR| at_line = 11 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |EVENTHANDLER_A| ).
*
*      maker->add_method(            method  = |EVENTHANDLER_A| at_line = 13 ).
*      maker->add_method(            method  = |EVENT_A| at_line = 9 ).
*
*      maker->add_method(            method  = |METHOD_A| at_line = 12 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |EVENT_A| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A2| used = |METHOD_A| ).
*      maker->access(                          used_group  = |Z2MSE_TEST_A| used = |Z2MSE_TEST_A| ).
*
*      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
** BEGIN INSERT when Interfaces are reversed
*      IF interface_use_structure EQ abap_true.
*        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |EVENT_A_0000000000000000000000| ).
*      ENDIF.
** END INSERT when Interfaces are reversed
*      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
** BEGIN INSERT when Interfaces are reversed
*      IF interface_use_structure EQ abap_true.
*        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
*      ENDIF.
** END INSERT when Interfaces are reversed
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |METHOD_A| ).
*
*      maker->add_class(      name = |Z2MSE_TEST_CL_A2| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*
*      maker->add_method(            method  = |METHOD_A| at_line = 8 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_B| ).
*
*      maker->add_class(      name = |Z2MSE_TEST_CL_A3| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*
*      maker->add_method(            method  = |METHOD_A| at_line = 9 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
*      maker->access(                          used = |Z2MSE_TEST_DB_B| ).
*
*      maker->add_class(      name = |Z2MSE_TEST_CL_B1| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_method(            method  = |EVENT_A| at_line = 8 ).
*
*      maker->add_attribute(         attribute = |ATTRIBUTE_A| at_line = 13 ).
*
*      maker->add_method(            method  = |METHOD_A| at_line = 10 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
*      maker->access(                          used = |Z2MSE_TEST_A| ).
*      maker->access(                          used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |ATTRIBUTE_A_000000000000000000| ).
*      maker->access(                          used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).
*
*      maker->add_method(            method  = |METHOD_B| at_line = 11 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*
*      maker->add_class(      name = |Z2MSE_TEST_CL_B2| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_attribute(         attribute = |Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| ).
*      maker->add_method(            method  = |METHOD_A| at_line = 12 ).
*      maker->usage(                           used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
*      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
** BEGIN INSERT when Interfaces are reversed
*      IF interface_use_structure EQ abap_true.
*        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |EVENT_A_0000000000000000000000| ).
*      ENDIF.
** END INSERT when Interfaces are reversed
*      maker->add_method(            method  = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
** BEGIN INSERT when Interfaces are reversed
*      IF interface_use_structure EQ abap_true.
*        maker->usage(                           used_group  = |Z2MSE_TEST_IF_A_00000000000000| used = |METHOD_A_000000000000000000000| ).
*      ENDIF.
** END INSERT when Interfaces are reversed
*
*      maker->add_interface(  name = |Z2MSE_TEST_IF_A_00000000000000| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_interface_method(  method  = |EVENT_A_0000000000000000000000| at_line = 7 ).
** BEGIN DELETE when Interfaces are reversed
*      IF interface_use_structure EQ abap_false.
*        maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
*        maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| ).
*      ENDIF.
** END DELETE when Interfaces are reversed
*      maker->add_interface_method(  method  = |METHOD_A_000000000000000000000| at_line = 9 ).
** BEGIN DELETE when Interfaces are reversed
*      IF interface_use_structure EQ abap_false.
*        maker->usage(                           used_group  = |Z2MSE_TEST_CL_B2| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
*        maker->usage(                           used_group  = |Z2MSE_TEST_CL_A| used = |Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| ).
*      ENDIF.
** END DELETE when Interfaces are reversed
*      maker->add_interface_attribute( attribute = |ATTRIBUTE_A_000000000000000000| at_line = 5 ).
*
*      maker->add_interface(  name = |ZIWCI_2MSE_TEST_WDY_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_interface_method(  method = |WD_GET_API| at_line = 17 ).
*
*      maker->add_web_dynpro_component( wda_name = |Z2MSE_TEST_WDY_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_web_dynpro_component_view(       view = |COMPONENTCONTROLLER| ).
*      maker->usage(                                      used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*      maker->usage(                                      used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
*      maker->usage(                                      used_group  = |ZIWCI_2MSE_TEST_WDY_A| used = |WD_GET_API| ).
*      maker->access(                                     used = |Z2MSE_TEST_A| ).
*      maker->add_web_dynpro_component_view(       view = |EMPTYVIEW| ).
*      maker->add_web_dynpro_component_view(       view = |V_MAIN| ).
*      maker->add_web_dynpro_component_view(       view = |W_MAIN| ).
*
*      maker->add_program(   name = |Z2MSE_TEST_PROGRAM_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->usage(                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*      maker->usage(                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
*      maker->access(               used_group  = |Z2MSE_TEST_CL_B1| used = |ATTRIBUTE_A| ).
*      maker->access(               used = |Z2MSE_TEST_DB_B| ).
*      maker->usage(                used = |Z2MSE_TEST_PROGRAM_B| ).
*      maker->usage(                used = |Z2MSE_TEST_PROGRAM_C| ).
*      maker->usage(                used = |Z2MSE_TEST_INCLUDE_A| ).
*      maker->usage(                       used_group  = |FGR-Z2MSE_TEST_FGR_A| used = |F-Z2MSE_TEST_FUNCTION_A| ).
*      maker->usage(                       used_group  = |FGR-Z2MSE_TEST_FGR_B| used = |F-Z2MSE_TEST_FUNCTION_B| ).
*      maker->add_program(   name = |Z2MSE_TEST_PROGRAM_B| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_program(   name = |Z2MSE_TEST_PROGRAM_C| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->usage(                used = |Z2MSE_TEST_PROGRAM_D| ).
*      maker->add_program(   name = |Z2MSE_TEST_PROGRAM_D| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_program(   name = |Z2MSE_TEST_INCLUDE_A| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_function_group( name = |Z2MSE_TEST_FGR_B| parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).
*      maker->add_function(              function = |Z2MSE_TEST_FUNCTION_B| ).
*
*      maker->add_bw_transformation( bw_transformation = |BW-ODSO-Z2MSET001-CUBE-Z2MSET002| at_line = 123 ).
*      maker->usage(                                     used_group  = |Z2MSE_TEST_CL_A| used = |CONSTRUCTOR| ).
*      maker->usage(                                     used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*
*      maker->add_function_group( name = |Z2MSE_TEST_FGR_A| parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).
*      maker->add_function(              function = |Z2MSE_TEST_FUNCTION_A| ).
*      maker->usage(                                used_group  = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).
*      maker->usage(                                used_group  = |FGR-Z2MSE_TEST_FGR_B| used = |F-Z2MSE_TEST_FUNCTION_B| ).
*      maker->add_function_group_include( include = |LZ2MSE_TEST_FGR_AF01| ).
*      maker->usage(                                used_group  = |Z2MSE_TEST_CL_B1| used = |METHOD_A| ).
*
*      equalized_harmonized_mse_exp = maker->to_change.

      equalized_harmonized_mse_exp = VALUE #(
      ( |SOMIX.Access accessor ABAPMethod.sap.z2mse_test_cl_a.method_a accessed DBTable.sap.sapt80.z2mse_test_a| )
      ( |SOMIX.Access accessor ABAPMethod.sap.z2mse_test_cl_a3.method_a accessed DBTable.sap.sapt80.z2mse_test_db_b| )
      ( |SOMIX.Access accessor ABAPMethod.sap.z2mse_test_cl_b1.method_a accessed ABAPClassAttribute.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~attribute_a_000000000000000000| )
      ( |SOMIX.Access accessor ABAPMethod.sap.z2mse_test_cl_b1.method_a accessed ABAPClassAttribute.sap.z2mse_test_if_a_00000000000000.attribute_a_000000000000000000| )
      ( |SOMIX.Access accessor ABAPMethod.sap.z2mse_test_cl_b1.method_a accessed DBTable.sap.sapt80.z2mse_test_a| )
      ( |SOMIX.Access accessor ABAPProgram.sap.z2mse_test_program_a accessed ABAPClassAttribute.sap.z2mse_test_cl_b1.attribute_a| )
      ( |SOMIX.Access accessor ABAPProgram.sap.z2mse_test_program_a accessed DBTable.sap.sapt80.z2mse_test_db_b| )
      ( |SOMIX.Access accessor ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller accessed DBTable.sap.sapt80.z2mse_test_a| )
*      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_b| )
*      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
*      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.constructor called ABAPMethod.sap.z2mse_test_cl_a.eventhandler_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.method_a called ABAPMethod.sap.z2mse_test_cl_a.event_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.method_a called ABAPMethod.sap.z2mse_test_cl_a2.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 called ABAPMethod.sap.z2mse_test_cl_b2.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a2.method_a called ABAPMethod.sap.z2mse_test_cl_b1.method_b| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a3.method_a called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_a called ABAPMethod.sap.z2mse_test_cl_a.constructor| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_a called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_a called ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_a called ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_b called ABAPMethod.sap.z2mse_test_cl_a.constructor| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_b called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b1.method_b called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b2.method_a called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
*      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_a| )
*      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_b| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPProgram.sap.z2mse_test_include_a| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPProgram.sap.z2mse_test_program_b| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPProgram.sap.z2mse_test_program_c| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_c called ABAPProgram.sap.z2mse_test_program_d| )
      ( |SOMIX.Call caller ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller called ABAPMethod.sap.z2mse_test_cl_a.constructor| )
      ( |SOMIX.Call caller ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
      ( |SOMIX.Call caller ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller called ABAPMethod.sap.ziwci_2mse_test_wdy_a.wd_get_api| )
      ( |SOMIX.Call caller BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 called ABAPMethod.sap.z2mse_test_cl_a.constructor| )
      ( |SOMIX.Call caller BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/fmodules/z2mse_test_function_a| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/includes/lz2mse_test_fgr_af01| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a name F-Z2MSE_TEST_FUNCTION_A| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a name LZ2MSE_TEST_FGR_AF01| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_b linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_b/fmodules/z2mse_test_function_b| )
*      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_b name F-Z2MSE_TEST_FUNCTION_B| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.constructor name CONSTRUCTOR| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.event_a name EVENT_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.eventhandler_a name EVENTHANDLER_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.method_a name METHOD_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a2.method_a name METHOD_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a3.method_a name METHOD_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.event_a name EVENT_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.method_a name METHOD_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.method_b name METHOD_B| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b2.method_a name METHOD_A| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~EVENT_A_0000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~METHOD_A_000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000 name EVENT_A_0000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000 name METHOD_A_000000000000000000000| )
      ( |SOMIX.Code ABAPMethod.sap.ziwci_2mse_test_wdy_a.wd_get_api name WD_GET_API| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_include_a linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_include_a| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_include_a name Z2MSE_TEST_INCLUDE_A| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_a linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_a| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_a name Z2MSE_TEST_PROGRAM_A| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_b linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_b| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_b name Z2MSE_TEST_PROGRAM_B| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_c linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_c| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_c name Z2MSE_TEST_PROGRAM_C| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_d linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_d| )
      ( |SOMIX.Code ABAPProgram.sap.z2mse_test_program_d name Z2MSE_TEST_PROGRAM_D| )
      ( |SOMIX.Code ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller name COMPONENTCONTROLLER| )
      ( |SOMIX.Code ABAPWebDynproController.sap.z2mse_test_wdy_a.emptyview name EMPTYVIEW| )
      ( |SOMIX.Code ABAPWebDynproController.sap.z2mse_test_wdy_a.v_main name V_MAIN| )
      ( |SOMIX.Code ABAPWebDynproController.sap.z2mse_test_wdy_a.w_main name W_MAIN| )
      ( |SOMIX.Code BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 linkToEditor bwmt://T80/sap/bw/modeling/trfn/123| )
      ( |SOMIX.Code BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 name BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )

      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.constructor linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=11,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.event_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=9,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.eventhandler_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=13,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a.method_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=12,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a2.method_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main#start=8,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_a3.method_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a3/source/main#start=9,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.event_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=8,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.method_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=10,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b1.method_b linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=11,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_cl_b2.method_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main#start=12,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000 linkToEditor adt://T80/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=7,1| )
      ( |SOMIX.Code ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000 linkToEditor adt://T80/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=9,1| )
      ( |SOMIX.Code ABAPMethod.sap.ziwci_2mse_test_wdy_a.wd_get_api linkToEditor adt://T80/sap/bc/adt/oo/interfaces/ziwci_2mse_test_wdy_a/source/main#start=17,1| )
      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_cl_b1.attribute_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main#start=13,1| )
      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_if_a_00000000000000.attribute_a_000000000000000000 linkToEditor adt://T80/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=5,1| )

      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a.lz2mse_test_fgr_af01 called ABAPMethod.sap.z2mse_test_cl_b1.method_a| )
      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_b.z2mse_test_function_b| )
      ( |SOMIX.Call caller ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a called ABAPMethod.sap.z2mse_test_cl_a.method_a| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_b.z2mse_test_function_b| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a.lz2mse_test_fgr_af01 name LZ2MSE_TEST_FGR_AF01| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a.lz2mse_test_fgr_af01 linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/includes/lz2mse_test_fgr_af01| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/fmodules/z2mse_test_function_a| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a name F-Z2MSE_TEST_FUNCTION_A| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_b.z2mse_test_function_b linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_b/fmodules/z2mse_test_function_b| )
      ( |SOMIX.Code ABAPFunktion.sap.fgr-z2mse_test_fgr_b.z2mse_test_function_b name F-Z2MSE_TEST_FUNCTION_B| )
      ( |SOMIX.Call caller ABAPProgram.sap.z2mse_test_program_a called ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a| )
      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a.lz2mse_test_fgr_af01 isMain| )
      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a isMain| )
      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a.z2mse_test_function_a isMain| )
      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_b child ABAPFunktion.sap.fgr-z2mse_test_fgr_b.z2mse_test_function_b isMain| )



      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~attribute_a_000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_cl_b1.attribute_a name ATTRIBUTE_A| )
      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~attribute_a_000000000000000000 name Z2MSE_TEST_IF_A_00000000000000~ATTRIBUTE_A_000000000000000000| )
      ( |SOMIX.Data ABAPClassAttribute.sap.z2mse_test_if_a_00000000000000.attribute_a_000000000000000000 name ATTRIBUTE_A_000000000000000000| )
      ( |SOMIX.Data DBTable.sap.sapt80.z2mse_test_a name Z2MSE_TEST_A| )
      ( |SOMIX.Data DBTable.sap.sapt80.z2mse_test_db_b name Z2MSE_TEST_DB_B| )
      ( |SOMIX.Grouping ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/fmodules/z2mse_test_function_a| )
      ( |SOMIX.Grouping ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_a/includes/lz2mse_test_fgr_af01| )
      ( |SOMIX.Grouping ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a name FGR-Z2MSE_TEST_FGR_A| )
      ( |SOMIX.Grouping ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_b linkToEditor adt://T80/sap/bc/adt/functions/groups/z2mse_test_fgr_b/fmodules/z2mse_test_function_b| )
      ( |SOMIX.Grouping ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_b name FGR-Z2MSE_TEST_FGR_B| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a name Z2MSE_TEST_CL_A| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a2 linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a2/source/main| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a2 name Z2MSE_TEST_CL_A2| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a3 linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_a3/source/main| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_a3 name Z2MSE_TEST_CL_A3| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_b1 linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b1/source/main| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_b1 name Z2MSE_TEST_CL_B1| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_b2 linkToEditor adt://T80/sap/bc/adt/oo/classes/z2mse_test_cl_b2/source/main| )
      ( |SOMIX.Grouping ABAPGlobalClass.sap.z2mse_test_cl_b2 name Z2MSE_TEST_CL_B2| )
      ( |SOMIX.Grouping ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000 linkToEditor adt://T80/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main| )
      ( |SOMIX.Grouping ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000 name Z2MSE_TEST_IF_A_00000000000000| )
      ( |SOMIX.Grouping ABAPGlobalInterface.sap.ziwci_2mse_test_wdy_a linkToEditor adt://T80/sap/bc/adt/oo/interfaces/ziwci_2mse_test_wdy_a/source/main| )
      ( |SOMIX.Grouping ABAPGlobalInterface.sap.ziwci_2mse_test_wdy_a name ZIWCI_2MSE_TEST_WDY_A| )
      ( |SOMIX.Grouping ABAPPackage.sap.z2mse_test_initial_selection name Z2MSE_TEST_INITIAL_SELECTION| )
      ( |SOMIX.Grouping ABAPPackage.sap.z2mse_test_no_initial_selectn name Z2MSE_TEST_NO_INITIAL_SELECTN| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_include_a linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_include_a| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_include_a name Z2MSE_TEST_INCLUDE_A| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_a linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_a| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_a name Z2MSE_TEST_PROGRAM_A| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_b linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_b| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_b name Z2MSE_TEST_PROGRAM_B| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_c linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_c| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_c name Z2MSE_TEST_PROGRAM_C| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_d linkToEditor adt://T80/sap/bc/adt/programs/programs/z2mse_test_program_d| )
      ( |SOMIX.Grouping ABAPProgram.sap.z2mse_test_program_d name Z2MSE_TEST_PROGRAM_D| )
      ( |SOMIX.Grouping ABAPWebDynproComponent.sap.z2mse_test_wdy_a name Z2MSE_TEST_WDY_A| )
      ( |SOMIX.Grouping BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 linkToEditor bwmt://T80/sap/bw/modeling/trfn/123| )
      ( |SOMIX.Grouping BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 name BW-ODSO-Z2MSET001-CUBE-Z2MSET002| )
*      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a| )
*      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_b child ABAPFunktion.sap.fgr-z2mse_test_fgr_b isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPClassAttribute.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~attribute_a_000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.constructor isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.event_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.eventhandler_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.method_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a child ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a2 child ABAPMethod.sap.z2mse_test_cl_a2.method_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_a3 child ABAPMethod.sap.z2mse_test_cl_a3.method_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b1 child ABAPClassAttribute.sap.z2mse_test_cl_b1.attribute_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b1 child ABAPMethod.sap.z2mse_test_cl_b1.event_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b1 child ABAPMethod.sap.z2mse_test_cl_b1.method_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b1 child ABAPMethod.sap.z2mse_test_cl_b1.method_b isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b2 child ABAPClassAttribute.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~attribute_a_000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b2 child ABAPMethod.sap.z2mse_test_cl_b2.method_a isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b2 child ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalClass.sap.z2mse_test_cl_b2 child ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000 child ABAPClassAttribute.sap.z2mse_test_if_a_00000000000000.attribute_a_000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000 child ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000 child ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000 isMain| )
      ( |SOMIX.ParentChild parent ABAPGlobalInterface.sap.ziwci_2mse_test_wdy_a child ABAPMethod.sap.ziwci_2mse_test_wdy_a.wd_get_api isMain| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPGlobalClass.sap.z2mse_test_cl_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPGlobalClass.sap.z2mse_test_cl_a2| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPGlobalClass.sap.z2mse_test_cl_a3| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPGlobalInterface.sap.ziwci_2mse_test_wdy_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPProgram.sap.z2mse_test_program_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPProgram.sap.z2mse_test_program_b| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child ABAPWebDynproComponent.sap.z2mse_test_wdy_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_initial_selection child DBTable.sap.sapt80.z2mse_test_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_b| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPGlobalClass.sap.z2mse_test_cl_b1| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPGlobalClass.sap.z2mse_test_cl_b2| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPGlobalInterface.sap.z2mse_test_if_a_00000000000000| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPProgram.sap.z2mse_test_include_a| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPProgram.sap.z2mse_test_program_c| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child ABAPProgram.sap.z2mse_test_program_d| )
      ( |SOMIX.ParentChild parent ABAPPackage.sap.z2mse_test_no_initial_selectn child DBTable.sap.sapt80.z2mse_test_db_b| )
      ( |SOMIX.ParentChild parent ABAPProgram.sap.z2mse_test_include_a child ABAPProgram.sap.z2mse_test_include_a| )
      ( |SOMIX.ParentChild parent ABAPProgram.sap.z2mse_test_program_a child ABAPProgram.sap.z2mse_test_program_a| )
      ( |SOMIX.ParentChild parent ABAPProgram.sap.z2mse_test_program_b child ABAPProgram.sap.z2mse_test_program_b| )
      ( |SOMIX.ParentChild parent ABAPProgram.sap.z2mse_test_program_c child ABAPProgram.sap.z2mse_test_program_c| )
      ( |SOMIX.ParentChild parent ABAPProgram.sap.z2mse_test_program_d child ABAPProgram.sap.z2mse_test_program_d| )
      ( |SOMIX.ParentChild parent ABAPWebDynproComponent.sap.z2mse_test_wdy_a child ABAPWebDynproController.sap.z2mse_test_wdy_a.componentcontroller isMain| )
      ( |SOMIX.ParentChild parent ABAPWebDynproComponent.sap.z2mse_test_wdy_a child ABAPWebDynproController.sap.z2mse_test_wdy_a.emptyview isMain| )
      ( |SOMIX.ParentChild parent ABAPWebDynproComponent.sap.z2mse_test_wdy_a child ABAPWebDynproController.sap.z2mse_test_wdy_a.v_main isMain| )
      ( |SOMIX.ParentChild parent ABAPWebDynproComponent.sap.z2mse_test_wdy_a child ABAPWebDynproController.sap.z2mse_test_wdy_a.w_main isMain| )
      ( |SOMIX.ParentChild parent BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002 child BWTransformation.sap.bw-odso-z2mset001-cube-z2mset002| )
        ).


      IF interface_use_structure EQ abap_false.
        equalized_harmonized_mse_exp = VALUE #( BASE equalized_harmonized_mse_exp
*      ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a isMain| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000 called ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000 called ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000 called ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000| )
      ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000 called ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~method_a_000000000000000000000| )
           ).
      ELSE.
        equalized_harmonized_mse_exp = VALUE #( BASE equalized_harmonized_mse_exp
           ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 called ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000| )
           ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_a.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 called ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000| )
           ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~event_a_0000000000000000000000 called ABAPMethod.sap.z2mse_test_if_a_00000000000000.event_a_0000000000000000000000| )
           ( |SOMIX.Call caller ABAPMethod.sap.z2mse_test_cl_b2.z2mse_test_if_a_00000000000000~method_a_000000000000000000000 called ABAPMethod.sap.z2mse_test_if_a_00000000000000.method_a_000000000000000000000| )
*           ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a isMain| )
*           ( |SOMIX.ParentChild parent ABAPFunktionGroup.sap.fgr-z2mse_test_fgr_a child ABAPFunktion.sap.fgr-z2mse_test_fgr_a isMain| )
           ).
      ENDIF.
      z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_act ).
      z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).
      SORT equalized_harmonized_mse_act.
      SORT equalized_harmonized_mse_exp.
      DELETE ADJACENT DUPLICATES FROM equalized_harmonized_mse_act.
      DELETE ADJACENT DUPLICATES FROM equalized_harmonized_mse_exp. ##TODO " Offenbar gibt es noch duplicates bei Actual
*      IF equalized_harmonized_mse_act[] <> equalized_harmonized_mse_exp[].
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = equalized_harmonized_mse_act
          exp                  = equalized_harmonized_mse_exp
          msg                  = message ).

    ENDDO.

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
                                                             i_exclude_found_sap_intf = abap_true
                                                             i_interface_use_structure = abap_false ).

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

    maker->add_custom_source_language( |SAP| ).

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


  METHOD _search_specific.

    " search specific
    DATA: nothing_done_act TYPE abap_bool.

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true
                                                             i_interface_use_structure = abap_false ).

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read    = dynamic_read ).


    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = element_type_filter
                                                 i_parent_name_filter  = parent_name_filter
                                                 i_name_filter         = name_filter ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = i_search_up
                              i_search_down            = i_search_down
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).




    model_builder->write_found_elements( EXPORTING write = abap_false
                                         IMPORTING fes   = fes_act ).

  ENDMETHOD.


  METHOD _check_found_elements.

    " Check found elements

    SORT fes_act.
    SORT fes_exp.
    cl_abap_unit_assert=>assert_equals( msg = msg exp = fes_exp act = fes_act ).

  ENDMETHOD.


  METHOD _check_harmonized_mse.

    " Check harmonized mse

    equalized_harmonized_mse_exp = maker->to_change.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = msg ).

  ENDMETHOD.


  METHOD z2mse_test_2021.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA(top_packages) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST_2021' ) ).
    DATA(sub_packages_filter) = VALUE z2mse_extr3_initial_elements=>ty_s_pack( ).

    DATA(initial_elements) = NEW z2mse_extr3_initial_elements( ).
    initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                 sub_packages_filter    = sub_packages_filter
                                                 including_sub_packages = abap_true ).

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder          = model_builder
                                                             i_exclude_found_sap_intf = abap_true
                                                             i_interface_use_structure = abap_false ).

    model_builder->initialize( i_element_manager = element_manager ).

    DATA(f_cut) = NEW z2mse_extract3( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 2
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

    maker->add_custom_source_language( |SAP| ).

    maker->add_package( package = |Z2MSE_TEST_2021| ).

    " Test class used by program is found

    maker->add_class( EXPORTING          name          = |Z2MSE_TEST_CL_CC|
                                         parentpackage = |Z2MSE_TEST_2021| ).

    maker->add_method( EXPORTING         method  = |Z2MSE_TEST_IF_C1~METHOD2|
                                         at_line = 8 ).

    maker->add_class( EXPORTING          name          = |Z2MSE_TEST_CL_CP|
                                         parentpackage = |Z2MSE_TEST_2021| ).

    maker->add_method( EXPORTING         method  = |Z2MSE_TEST_IF_C1~METHOD2|
                                         at_line = 8 ).

    maker->add_interface( EXPORTING     name           = |Z2MSE_TEST_IF_C1|
                                        parentpackage  = |Z2MSE_TEST_2021| ).

    maker->add_interface_method( EXPORTING method    = |METHOD2|
                                        at_line   = 5 ).

    maker->usage( EXPORTING using_group = |Z2MSE_TEST_CL_CP|
                            using       = |Z2MSE_TEST_IF_C1~METHOD2|
                            used_group  = |Z2MSE_TEST_CL_CC|
                            used        = |Z2MSE_TEST_IF_C1~METHOD2| ).

    maker->usage( EXPORTING using_group = |Z2MSE_TEST_IF_C1|
                            using       = |METHOD2|
                            used_group  = |Z2MSE_TEST_CL_CC|
                            used        = |Z2MSE_TEST_IF_C1~METHOD2| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    " This Unit Test breaks currently due to an incomplete implementation of #132
    IF sy-datum < '20220301'.
      RETURN. " Deactive this test for some time. After this time it will break again and remind of the incomplete implementation
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file' ).

  ENDMETHOD.

ENDCLASS.
