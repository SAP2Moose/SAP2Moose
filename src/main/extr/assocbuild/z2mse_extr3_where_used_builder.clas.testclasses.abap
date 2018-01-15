CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_extr3_where_used_builder DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO z2mse_extr3_where_used_builder.
    METHODS:
      set_dynamic_read FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD set_dynamic_read.
    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.
    CREATE OBJECT model_builder.


    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    CREATE OBJECT element_manager
      EXPORTING
        i_model_builder          = model_builder
        i_exclude_found_sap_intf = abap_true.
    f_cut = NEW #( i_element_manager = element_manager ).

    f_cut->set_dynamic_read(
        i_dynamic_read = 'Z2MSE_TEST_DYNAMIC_USAGE'
    ).

    DATA: exp TYPE z2mse_extr3_where_used_builder=>wbcrossgts_type,
          act TYPE z2mse_extr3_where_used_builder=>wbcrossgts_type.

    exp = VALUE #( ( otype = 'ME'
                     name = 'Z2MSE_TEST_CL_A\ME:METHOD_A'
                     include = 'Z2MSE_TEST_CL_B1==============CM002' ) ).

    act = f_cut->g_dynamic_usage.

    cl_abap_unit_assert=>assert_equals( msg = 'Expect correct dynamic usage' exp = exp act = act ).

  ENDMETHOD.



ENDCLASS.
