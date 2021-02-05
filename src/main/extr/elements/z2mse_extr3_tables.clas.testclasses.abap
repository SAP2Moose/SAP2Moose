CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_tables.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true
                             i_interface_use_structure = abap_false ).
    model_builder->initialize( i_element_manager = element_manager ).
    f_cut = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
  ENDMETHOD.

  METHOD simple.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: table_name_act TYPE tabname,
          table_name_exp TYPE tabname,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION dd02l.
      found_tabname = 'TABLE_A'.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING table = 'TABLE_A'
                IMPORTING is_added = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Table has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 1' exp = 1 act = new_element_id ).

    " Add an existing id

    f_cut->add( EXPORTING table = 'TABLE_A'
                IMPORTING is_added = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Table has to be marked as found if entered a second time' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 1 if added a second time' exp = 1 act = new_element_id ).

    table_name_act = f_cut->table_name( i_element_id = 1 ).
    table_name_exp = |TABLE_A|.

    cl_abap_unit_assert=>assert_equals( msg = 'Table has to be stored internally' exp = table_name_exp act = table_name_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type table'
                                        exp = z2mse_extr3_elements=>table_type
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
                                            ( |FAMIX.Class TABLE_A modifiers DBTable| )
                                            ( |FAMIX.Attribute TABLE_A>>TABLE_A| )
                                            ( |FAMIX.Class TABLE_A parentPackage PACKAGE1| )
                                            ( |FAMIX.Package PACKAGE1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).
  ENDMETHOD.

ENDCLASS.
