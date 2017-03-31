CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA model_builder TYPE REF TO z2mse_extr3_model_builder_mock.
    DATA f_cut TYPE REF TO z2mse_extr3_element_manager.
    METHODS:
      main FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD main.
    model_builder = NEW #( ).
    f_cut = NEW #( i_model_builder = model_builder ).
    DATA element TYPE REF TO z2mse_extr3_elements_mock.
    DATA element_act TYPE REF TO z2mse_extr3_elements.
    element = NEW #( i_element_manager = f_cut ).

    DATA element_id_act TYPE z2mse_extr3_element_manager=>element_id_type.

    element_id_act = f_cut->add_element( element = element ).

    cl_abap_unit_assert=>assert_equals( msg = 'New element has to be reported to the model builder'
                                        exp = element_id_act
                                        act = model_builder->last_reported_new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'New ID has to be 1' exp = 1 act = element_id_act ).

    element_act = f_cut->get_element( i_element_id = 1 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect stored reference back' exp = element act = element_act ).

    " Now build an association

    DATA association TYPE REF TO z2mse_extr3_association_mock.
    association = NEW #( i_element_manager = f_cut ).

    f_cut->add_association( EXPORTING element_1   = 1
                                      element_2   = 2
                                      association = association ).

    DATA: associations_act TYPE z2mse_extr3_element_manager=>associations_type,
          associations_exp TYPE z2mse_extr3_element_manager=>associations_type.

    associations_act = f_cut->get_associations( i_element_id = 1 ).

    associations_exp = VALUE #( ( element_id1 = 1 element_id2 = 2 association = association ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct association when searched for first element'
                                        exp = associations_exp
                                        act = associations_act ).

    associations_act = f_cut->get_associations( i_element_id = 2 ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct association when searched for second element'
                                        exp = associations_exp
                                        act = associations_act ).

    associations_act = f_cut->get_associations( i_element_id = 3 ).

    associations_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect no association for not existing id'
                                        exp = associations_exp
                                        act = associations_act ).

    " Test building model

    f_cut->make_model( ).

    associations_exp = VALUE #( ( element_id1 = 1 element_id2 = 2 association = association ) ).

    cl_abap_unit_assert=>assert_equals( msg = 'The element ID has to be transfered to make the model'
                                        exp = 1
                                        act = element->last_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'The associations have to be transfered to make the model'
                                        exp = associations_exp
                                        act = element->last_associations ).

    " z2mse_extr3_association_mock
  ENDMETHOD.

ENDCLASS.
