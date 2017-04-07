CLASS ltcl_class DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_classes.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_class IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true ).
    model_builder->initialize( element_manager = element_manager ).
    f_cut = z2mse_extr3_classes=>get_instance( element_manager ).
  ENDMETHOD.

  METHOD simple.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: class_name_act TYPE seoclsname,
          class_name_exp TYPE seoclsname,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION seoclass.
      IF class EQ 'CLASS_A'.
        found_class_name = 'CLASS_A'.
        found_class_type = 0.
      ELSEIF class EQ 'INTERFACE_A'.
        found_class_name = 'INTERFACE_A'.
        found_class_type = 0.
      ENDIF.
    END-TEST-INJECTION.

    TEST-INJECTION seocompo_2.
      IF class EQ 'CLASS_A'.
        class_components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' ) ).
      ENDIF.
    END-TEST-INJECTION.

    TEST-INJECTION seocompo.
      IF clsname EQ 'CLASS_A'.
        found_class_name = 'CLASS_A'.
        found_cmpname = 'METHOD_A'.
      ENDIF.
    END-TEST-INJECTION.

    TEST-INJECTION seometarel.
      IF clsname EQ 'CLASS_A'.
        relations = VALUE #( ( clsname = |CLASS_A| refclsname = |INTERFACE_A| reltype = |1| ) ).
      ENDIF.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING class = 'CLASS_A'
                IMPORTING is_added       = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be ID 1' exp = 1 act = new_element_id ).

    " Add an existing class

    f_cut->add( EXPORTING class = 'CLASS_A'
                IMPORTING is_added       = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be ID 1' exp = 1 act = new_element_id ).

    f_cut->class_name( EXPORTING element_id = 1
                       IMPORTING class_name = class_name_act ).
    class_name_exp = |CLASS_A|.

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be stored internally' exp = class_name_exp act = class_name_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type class'
                                        exp = z2mse_extr3_elements=>class_type
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
                                            ( |FAMIX.Class CLASS_A modifiers ABAPGlobalClass| )
                                            ( |FAMIX.Class CLASS_A parentPackage PACKAGE1| )
                                            ( |FAMIX.Class INTERFACE_A modifiers ABAPGlobalClass| )
                                            ( |FAMIX.Attribute CLASS_A>>METHOD_A| )
                                            ( |FAMIX.Package PACKAGE1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_interface DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_classes.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_interface IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder = model_builder
                             i_exclude_found_sap_intf = abap_true ).
    model_builder->initialize( element_manager = element_manager ).
    f_cut = z2mse_extr3_classes=>get_instance( element_manager ).
  ENDMETHOD.

  METHOD simple.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: class_name_act TYPE seoclsname,
          class_name_exp TYPE seoclsname,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION seoclass.
      found_class_name = 'INTERFACE_A'.
      found_class_type = 1.
    END-TEST-INJECTION.

    f_cut->add( EXPORTING class = 'INTERFACE_A'
                IMPORTING is_added       = is_found
                          new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be ID 1' exp = 1 act = new_element_id ).

    f_cut->class_name( EXPORTING element_id = 1
                       IMPORTING class_name = class_name_act ).

    class_name_exp = |INTERFACE_A|.

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be stored internally' exp = class_name_exp act = class_name_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type class'
                                        exp = z2mse_extr3_elements=>class_type
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
                                            ( |FAMIX.Class INTERFACE_A modifiers ABAPGlobalInterface| )
                                            ( |FAMIX.Class INTERFACE_A parentPackage PACKAGE1| )
                                            ( |FAMIX.Class INTERFACE_A isInterface true| )
                                            ( |FAMIX.Package PACKAGE1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Expect mse with correct package' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_component DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: model_builder   TYPE REF TO z2mse_extr3_model_builder,
          element_manager TYPE REF TO z2mse_extr3_element_manager,
          f_cut           TYPE REF TO z2mse_extr3_classes.
    METHODS:
      setup,
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_component IMPLEMENTATION.

  METHOD setup.
    model_builder = NEW #( ).
    model_builder->initial_selection_started( ).
    element_manager = NEW #( i_model_builder          = model_builder
                             i_exclude_found_sap_intf = abap_true ).
    model_builder->initialize( element_manager = element_manager ).
    f_cut = z2mse_extr3_classes=>get_instance( element_manager ).
  ENDMETHOD.

  METHOD simple.

    DATA r_result TYPE REF TO z2mse_extr3_elements.

    DATA: class_name_act TYPE seoclsname,
          class_name_exp TYPE seoclsname,
          cmpname_act    TYPE seocmpname,
          cmpname_exp    TYPE seocmpname,
          cmptype_act    TYPE seocmptype,
          cmptype_exp    TYPE seocmptype,
          is_found       TYPE abap_bool,
          new_element_id TYPE i.

    TEST-INJECTION seoclass.
      found_class_name = 'CLASS_A'.
      found_class_type = 0.
    END-TEST-INJECTION.

    TEST-INJECTION seocompo.
      found_class_name = 'CLASS_A'.
      found_cmpname = 'METHOD_A'.
      found_cmptype = me->method_type.
    END-TEST-INJECTION.

    f_cut->add_component( EXPORTING clsname        = 'CLASS_A'
                                    cmpname        = 'METHOD_A'
                          IMPORTING is_added       = is_found
                                    new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Component has to be found' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 2' exp = 2 act = new_element_id ).



    f_cut->comp_name( EXPORTING element_id = 1
                      IMPORTING class_name = class_name_act
                                cmpname = cmpname_act
                                cmptype = cmptype_act ).

    class_name_exp = ||.
    cmpname_exp = ||.
    CLEAR cmptype_exp.

    cl_abap_unit_assert=>assert_equals( msg = 'Expect initial values if the method is called for a class' exp = class_name_exp act = class_name_act ).
    cl_abap_unit_assert=>assert_equals( msg = 'Expect initial values if the method is called for a class' exp = cmpname_exp act = cmpname_act ).
    cl_abap_unit_assert=>assert_equals( msg = 'Expect initial values if the method is called for a class' exp = cmptype_exp act = cmptype_act ).

    " Add an existing id

    f_cut->add_component( EXPORTING clsname        = 'CLASS_A'
                                    cmpname        = 'METHOD_A'
                          IMPORTING is_added       = is_found
                                    new_element_id = new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'Component has to be marked as found if entered a second time' exp = abap_true act = is_found ).

    cl_abap_unit_assert=>assert_equals( msg = 'ID has to be 2 if added a second time' exp = 2 act = new_element_id ).

    f_cut->class_name( EXPORTING element_id = new_element_id
                       IMPORTING class_name = class_name_act ).

    class_name_exp = ||.

    cl_abap_unit_assert=>assert_equals( msg = 'Expect empty class name if method is called for method or attribute' exp = class_name_exp act = class_name_act ).

    f_cut->comp_name( EXPORTING element_id = new_element_id
                      IMPORTING class_name = class_name_act
                                cmpname = cmpname_act
                                cmptype = cmptype_act ).

    class_name_exp = |CLASS_A|.
    cmpname_exp = |METHOD_A|.
    cmptype_exp = f_cut->method_type.

    cl_abap_unit_assert=>assert_equals( msg = 'Class has to be stored internally' exp = class_name_exp act = class_name_act ).
    cl_abap_unit_assert=>assert_equals( msg = 'Component has to be stored internally' exp = cmpname_exp act = cmpname_act ).
    cl_abap_unit_assert=>assert_equals( msg = 'Component Type has to be stored internally' exp = cmptype_exp act = cmptype_act ).

    r_result = element_manager->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect a reference to an element of type class'
                                        exp = z2mse_extr3_elements=>class_type
                                        act = r_result->type ).

  ENDMETHOD.

ENDCLASS.
