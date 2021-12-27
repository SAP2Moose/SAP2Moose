CLASS ltcl_class DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_web_dynpro_comp.
    METHODS:
      setup,
      simple_somix FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_class IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true
                             i_interface_use_structure = abap_false
                             i_use_somix = 'X' ).
    model_builder->initialize( i_element_manager = element_manager ).
    f_cut = z2mse_extr3_web_dynpro_comp=>get_instance( element_manager ).
  ENDMETHOD.

  METHOD simple_somix.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: class_name_act TYPE seoclsname,
          class_name_exp TYPE seoclsname,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION wdy_component.
      found_wdy_component_name = 'WDY_COMP_A'.
    END-TEST-INJECTION.

    TEST-INJECTION wdy_controller_2.

      class_components = VALUE #( ( component_name = 'WDY_COMP_A' controller_name = 'WDY_CONTR_1' ) ).

    END-TEST-INJECTION.

    TEST-INJECTION wdy_controller.
      found_component_name = 'WDY_COMP_A'.
      found_controller_name = 'WDY_CONTR_1'.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING wdy_component_name = 'WDY_COMP_A'
                IMPORTING is_added       = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Web Dynpro Component has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'Web Dynpro Component has to have ID 1' exp = 1 act = new_element_id ).

    " Add an existing class

    f_cut->add( EXPORTING wdy_component_name = 'WDY_COMP_A'
                IMPORTING is_added       = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Web Dynpro Component is to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'Web Dynpro Component has to have ID 1' exp = 1 act = new_element_id ).

    f_cut->wdy_component_name( EXPORTING element_id         = 1
                               IMPORTING wdy_component_name = class_name_act ).

    class_name_exp = |WDY_COMP_A|.

    cl_abap_unit_assert=>assert_equals( msg = 'Web Dynpro Component has to be stored internally' exp = class_name_exp act = class_name_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type web dynpro component'
                                        exp = z2mse_extr3_elements=>web_dynpro_comps_type
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


    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( mse = mse_model_act ).
*    equalized_harmonized_mse_exp = VALUE #( ( |FAMIX.CustomSourceLanguage SAP| )
*                                            ( |FAMIX.Class WDY_COMP_A modifiers ABAPWebDynproComponent| )
*                                            ( |FAMIX.Class WDY_COMP_A parentPackage PACKAGE1| )
*                                            ( |FAMIX.Method WDY_COMP_A>>WDY_CONTR_1 signature WDY_CONTR_1| )
*                                            ( |FAMIX.Package PACKAGE1| )
*                                           ).
    equalized_harmonized_mse_exp = VALUE #( ( |SOMIX.Code ABAPWebDynproController.sap.wdy_comp_a.wdy_contr_1 name WDY_CONTR_1| )
                                            ( |SOMIX.Grouping ABAPPackage.sap.package1 name PACKAGE1| )
                                            ( |SOMIX.Grouping ABAPWebDynproComponent.sap.wdy_comp_a name WDY_COMP_A| )
                                            ( |SOMIX.ParentChild parent ABAPPackage.sap.package1 child ABAPWebDynproComponent.sap.wdy_comp_a| )
                                            ( |SOMIX.ParentChild parent ABAPWebDynproComponent.sap.wdy_comp_a child ABAPWebDynproController.sap.wdy_comp_a.wdy_contr_1 isMain| )
                                           ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).
  ENDMETHOD.

ENDCLASS.
