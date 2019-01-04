CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extract3 DEFINITION LOCAL FRIENDS ltcl_main.
"! Contains the main integration test of SAP2Moose
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      "! New class to test initial selections
      z2mse_test2_init FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD setup.
    z2mse_extr3=>clear_all( ).
  ENDMETHOD.

  METHOD z2mse_test2_init.

    z2mse_extr3=>clear_all( ).

    DATA: f_cut TYPE REF TO z2mse_extract3.
    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA: top_packages        TYPE z2mse_extr3_initial_elements=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extr3_initial_elements=>ty_s_pack.
    top_packages = VALUE #( ( sign = 'I' option = 'EQ' low = 'Z2MSE_TEST2_INITIAL' ) ).
    sub_packages_filter = VALUE #( ).

    DATA: initial_elements TYPE REF TO z2mse_extr3_initial_elements.
    initial_elements = NEW #( ).
    initial_elements->select_packages( EXPORTING top_packages           = top_packages
                                                 sub_packages_filter    = sub_packages_filter
                                                 including_sub_packages = abap_true ).


    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.
    CREATE OBJECT model_builder.


    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    CREATE OBJECT element_manager
      EXPORTING
        i_model_builder          = model_builder
        i_exclude_found_sap_intf = abap_true.

    model_builder->initialize( i_element_manager = element_manager
                               i_dynamic_read = || ).

    f_cut = NEW #( ).
    f_cut->extract( EXPORTING model_builder            = model_builder
                              element_manager          = element_manager
                              initial_elements         = initial_elements
                              i_search_up              = 1
                              i_search_down            = 1
                              i_exclude_found_sap_intf = abap_true
                    IMPORTING mse_model             = mse_model_act
                              nothing_done          = nothing_done_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(

    ).

    DATA maker TYPE REF TO z2mse_mse_harmonize_maker.

    maker = NEW #( ).

    maker->to_change = equalized_harmonized_mse_exp.

    maker->add_package( package = |Z2MSE_TEST2_INITIAL| ).
    maker->add_function_group( name = |Z2MSE_TEST2_I_FGR_A| parentpackage = |Z2MSE_TEST2_INITIAL| ). " TBD Add Parentpackage
    maker->add_function(              function = |Z2MSE_TEST2_I_FUNCTION_A| ).

    equalized_harmonized_mse_exp = maker->to_change.

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).


  ENDMETHOD.

ENDCLASS.
