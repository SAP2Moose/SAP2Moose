CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr3_programs DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_programs.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check,
      _extract_function_name FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.

    TEST-INJECTION tfdir.
      cl_abap_unit_assert=>fail( EXPORTING msg = |Redefine me| ).
    END-TEST-INJECTION.

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
          is_found         TYPE abap_bool,
          new_element_id   TYPE i.

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
    equalized_harmonized_mse_exp = VALUE #( ( |FAMIX.CustomSourceLanguage SAP| )
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

  METHOD _extract_function_name.

    DATA: function_group_act   TYPE rs38l_area,
          function_act         TYPE rs38l_fnam,
          function_include_act TYPE string,
          function_name        TYPE string.

    function_group_act = |DUMMY|.
    function_act = |DUMMY|.
    function_include_act = |DUMMY|.

    f_cut->_extract_function_name( EXPORTING i_element_program = |ABC|
                                                         IMPORTING function_group    = function_group_act
                                                                   function          = function_act
                                                                   function_include  = function_include_act
                                                                   r_result          = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return name unchanged if it very short'
                                        exp = |ABC| act = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Initial Function Group if name is very short'
                                        exp = || act = function_group_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Initial function if name is very short'
                                        exp = || act = function_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Initial function include if name is very short'
                                        exp = || act = function_include_act ).

    f_cut->_extract_function_name( EXPORTING i_element_program = |LAF01|
                                                   IMPORTING function_group    = function_group_act
                                                             function          = function_act
                                                             function_include  = function_include_act
                                                             r_result          = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return name unchanged thirdlast character is not U'
                                        exp = |LAF01| act = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return Function Group'
                                        exp = |A| act = function_group_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Initial function if thirdlast character is not U'
                                        exp = || act = function_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Correct function include if thirdlast character is F'
                                        exp = |LAF01| act = function_include_act ).

    TEST-INJECTION tfdir.

      IF pname EQ |SAPLA| AND include = |01|.
        funcname = |MY_FUNCTION|.
        sy-subrc = 0.
      ELSE.
        sy-subrc = 4.
      ENDIF.

    END-TEST-INJECTION.

    f_cut->_extract_function_name( EXPORTING i_element_program = |LAU01|
                                                   IMPORTING function_group    = function_group_act
                                                             function          = function_act
                                                             function_include  = function_include_act
                                                             r_result = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Read Function if thirdlast character is U'
                                        exp = |F-MY_FUNCTION| act = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return Function Group if thirdlast character is U'
                                        exp = |A| act = function_group_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return function if thirdlast character is U'
                                        exp = |MY_FUNCTION| act = function_act ).

    cl_abap_unit_assert=>assert_equals( msg = 'Initial function include if thirdlast character is U'
                                        exp = || act = function_include_act ).

    f_cut->_extract_function_name( EXPORTING i_element_program = |BU01|
                                                   IMPORTING function_group    = function_group_act
                                                             function          = function_act
                                                             function_include  = function_include_act
                                                             r_result = function_name ).

    cl_abap_unit_assert=>assert_equals( msg = 'Return the include name if nothing is found in table TFDIR'
                                        exp = |BU01| act = function_name ).

  ENDMETHOD.

ENDCLASS.
