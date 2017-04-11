CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_programs.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true ).
    model_builder->initialize( i_element_manager = element_manager ).
    f_cut = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).
  ENDMETHOD.

  METHOD simple.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: program_name_act TYPE progname,
          program_name_exp TYPE progname,
          subc_act         TYPE subc,
          subc_exp         TYPE subc,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION progdir.
      found_program = 'PROGRAM_A'.
      found_subc = 1.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING program = 'PROGRAM_A'
                IMPORTING is_added = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Program has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 1' exp = 1 act = new_element_id ).

    " Add an existing id

    f_cut->add( EXPORTING program = 'PROGRAM_A'
                IMPORTING is_added = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Program has to be marked as found if entered a second time' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 1 if added a second time' exp = 1 act = new_element_id ).

    f_cut->program_name( EXPORTING i_element_id = 1
                         IMPORTING program = program_name_act
                                   subc    = subc_act ).
    program_name_exp = |PROGRAM_A|.
    subc_exp = |1|.

    cl_abap_unit_assert=>assert_equals( msg = 'Program has to be stored internally' exp = program_name_exp act = program_name_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Subc has to be stored internally' exp = subc_exp act = subc_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type Program'
                                        exp = z2mse_extr3_elements=>program_type
                                        act = r_result->type ).

    " Now add parent package to check correct building of FAMIX element

    DATA package TYPE REF TO z2mse_extr3_packages_mock.
    DATA parent_package TYPE REF TO z2mse_extr3_parent_package.
    package = z2mse_extr3_packages_mock=>get_mock_instance( i_element_manager = element_manager ).
    package->add( EXPORTING package = |PACKAGE1|
                  IMPORTING new_element_id = new_element_id ).

    parent_package = z2mse_extr3_parent_package=>get_instance( i_element_manager = element_manager ).

    parent_package->add( EXPORTING element_id = 1
                                   parent_element_id = new_element_id ).

    " Test model

    DATA: mse_model_act TYPE z2mse_model=>lines_type.

    mse_model_act = element_manager->make_model( ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.


    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).
    equalized_harmonized_mse_exp = VALUE #(
                                            ( |FAMIX.Method PROGRAM_A>>PROGRAM_A signature PROGRAM_A| )
                                            ( |FAMIX.Class PROGRAM_A modifiers ABAPProgram| )
                                            ( |FAMIX.Class PROGRAM_A parentPackage PACKAGE1| )
                                            ( |FAMIX.Package PACKAGE1| )
                                             ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).
  ENDMETHOD.

ENDCLASS.
