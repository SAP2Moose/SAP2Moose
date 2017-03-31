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
    element = NEW #( i_element_manager = f_cut ).

    DATA element_id_act TYPE z2mse_extr3_element_manager=>element_id_type.

    element_id_act = f_cut->add_element( element = element ).

    cl_abap_unit_assert=>assert_equals( msg = 'New element has to be reported to the model builder'
                                        exp = element_id_act
                                        act = model_builder->last_reported_new_element_id ).

    cl_abap_unit_assert=>assert_equals( msg = 'New ID has to be 1' exp = 1 act = element_id_act ).



  ENDMETHOD.

ENDCLASS.
