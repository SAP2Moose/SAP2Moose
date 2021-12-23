CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr3_packages DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_packages.
    METHODS:
      setup,
      simple_somix FOR TESTING RAISING cx_static_check,
      _does_package_exists FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.
    CLEAR model_builder.
    CLEAR element_manager.
    CLEAR f_cut.


*    model_builder = NEW #( ).
*    model_builder->initial_selection_started( ).
*    element_manager = NEW #( i_model_builder = model_builder
*                             i_exclude_found_sap_intf = abap_true
*                             i_interface_use_structure = abap_false ).
*    model_builder->initialize( i_element_manager = element_manager ).
*    f_cut = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).
  ENDMETHOD.

  METHOD simple_somix.

*  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true
                             i_interface_use_structure = abap_false
                             i_use_somix = 'X' ).
    model_builder->initialize( i_element_manager = element_manager ).
    f_cut = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).
*  ENDMETHOD.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: devclass_act   TYPE devclass,
          devclass_exp   TYPE devclass,
          new_element_id TYPE i.

    DATA is_added TYPE abap_bool.

    TEST-INJECTION tadir.
      sy-subrc = 0.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING package  = 'PACKAGE_A'
                IMPORTING is_added = is_added
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect to be found' exp = abap_true act = is_added ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect to be 1' exp = 1 act = new_element_id ).

    " Add an existing element

    f_cut->add( EXPORTING package  = 'PACKAGE_A'
                IMPORTING is_added = is_added
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect to be found' exp = abap_true act = is_added ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect to be 1' exp = 1 act = new_element_id ).

    devclass_act = f_cut->devclass( i_element_id = 1 ).
    devclass_exp = |PACKAGE_A|.

    cl_abap_unit_assert=>assert_equals( msg = 'Package has to be stored internally' exp = devclass_exp act = devclass_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type package'
                                        exp = z2mse_extr3_elements=>package_type
                                        act = r_result->type ).

    DATA: mse_model_act TYPE z2mse_model=>lines_type.

    mse_model_act = element_manager->make_model( ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.


    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( mse = mse_model_act ).
*    equalized_harmonized_mse_exp = VALUE #( ( |FAMIX.CustomSourceLanguage SAP| )
*                                            ( |FAMIX.Package PACKAGE_A| ) ).
    equalized_harmonized_mse_exp = VALUE #( ( |SOMIX.Grouping ABAPPackage.sap.package_a name PACKAGE_A| ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).

  ENDMETHOD.

  METHOD _does_package_exists.


*  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true
                             i_interface_use_structure = abap_false ).
    model_builder->initialize( i_element_manager = element_manager ).
    f_cut = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).
*  ENDMETHOD.

    TEST-INJECTION tadir.
      sy-subrc = 4.
    END-TEST-INJECTION.

    " SAP_2_FAMIX_66

    DATA(exists) = f_cut->_does_package_exists( i_package = '$LOCAL_PACKAGE' ).

    cl_abap_unit_assert=>assert_equals( msg = 'Local packages exist always' exp = abap_true act = exists ).

    exists = f_cut->_does_package_exists( i_package = '' ).

    cl_abap_unit_assert=>assert_equals( msg = 'Empty package names exist never' exp = abap_false act = exists ).

  ENDMETHOD.

ENDCLASS.
