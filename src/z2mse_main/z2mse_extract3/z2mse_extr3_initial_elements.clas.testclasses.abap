CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr3_initial_elements DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO z2mse_extr3_initial_elements.
    METHODS:
      simple_no_sub_packages FOR TESTING RAISING cx_static_check,
      simple_wth_sub_packages FOR TESTING RAISING cx_static_check,
      complex_wth_sub_packages FOR TESTING RAISING cx_static_check.
*      add_selected_packages_to_model FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD simple_no_sub_packages.

    DATA: tdevc_test TYPE z2mse_extr_packages=>ty_t_tdevc_test.

    tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' )
                          ( devclass = 'A_A' parentcl = 'A' )
                          ( devclass = 'B' parentcl = '' ) ).

    DATA top_packages_range TYPE z2mse_extr_packages=>ty_s_pack.

    top_packages_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' ) ).

    f_cut = NEW #( tdevc_test = tdevc_test ).

    f_cut->select_packages( EXPORTING top_packages = top_packages_range ).

    DATA: selected_packages_act TYPE z2mse_extr_packages=>ty_packages,
          selected_packages_exp TYPE z2mse_extr_packages=>ty_packages.

    selected_packages_act = f_cut->g_selected_packages.
    selected_packages_exp = VALUE #( ( package = 'A') ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_packages_act
        exp                  = selected_packages_exp
        msg                  = 'Only package A is selected' ).

  ENDMETHOD.

  METHOD simple_wth_sub_packages.

    DATA: tdevc_test TYPE z2mse_extr_packages=>ty_t_tdevc_test.

    tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' )
                          ( devclass = 'A_A' parentcl = 'A' )
                          ( devclass = 'B' parentcl = '' ) ).

    DATA top_packages_range TYPE z2mse_extr_packages=>ty_s_pack.

    top_packages_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' ) ).

    f_cut = NEW #( tdevc_test = tdevc_test ).

    f_cut->select_packages( EXPORTING top_packages = top_packages_range
                                      including_sub_packages = abap_true  ).

    DATA: selected_packages_act TYPE z2mse_extr_packages=>ty_packages,
          selected_packages_exp TYPE z2mse_extr_packages=>ty_packages.

    selected_packages_act = f_cut->g_selected_packages.
    selected_packages_exp = VALUE #( ( package = 'A' parentpackage = '')
                                     ( package = 'A_A' parentpackage = 'A') ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_packages_act
        exp                  = selected_packages_exp
        msg                  = 'Only package A and subpackage A_A is selected' ).

  ENDMETHOD.

  METHOD complex_wth_sub_packages.

    DATA: tdevc_test TYPE z2mse_extr_packages=>ty_t_tdevc_test.

    tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' )
                          ( devclass = 'A_A' parentcl = 'A' )
                          ( devclass = 'A_A_A' parentcl = 'A_A' )
                          ( devclass = 'B' parentcl = '' ) ).

    DATA: top_packages_range  TYPE z2mse_extr_packages=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extr_packages=>ty_s_pack.

    top_packages_range = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' ) ).
    sub_packages_filter = VALUE #( ( sign = 'E' option = 'EQ' low = 'A_A' ) ).

    f_cut = NEW #( tdevc_test = tdevc_test ).

    f_cut->select_packages( EXPORTING top_packages = top_packages_range
                                      sub_packages_filter = sub_packages_filter
                                      including_sub_packages = abap_true  ).

    DATA: selected_packages_act TYPE z2mse_extr_packages=>ty_packages,
          selected_packages_exp TYPE z2mse_extr_packages=>ty_packages.

    selected_packages_act = f_cut->g_selected_packages.
    selected_packages_exp = VALUE #( ( package = 'A' parentpackage = '')
                                     ( package = 'A_A_A' parentpackage = 'A_A') ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_packages_act
        exp                  = selected_packages_exp
        msg                  = 'Only package A and A_A_A is selected. The package A_A from which A_A_A inherits is explicitely excluded.' ).

  ENDMETHOD.



*  METHOD add_selected_packages_to_model.
*
*
*    DATA model            TYPE REF TO z2mse_model.
*    DATA famix_package     TYPE REF TO z2mse_famix_package.
*
*    DATA: tdevc_test TYPE z2mse_extr_packages=>ty_t_tdevc_test.
*
*    tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' )
*                          ( devclass = 'A_A' parentcl = 'A' )
*                          ( devclass = 'A_A_A' parentcl = 'A_A' )
*                          ( devclass = 'B' parentcl = '' ) ).
*
*    f_cut = NEW #( tdevc_test = tdevc_test ).
*
*    f_cut->g_selected_packages = VALUE #( ( package = 'A' parentpackage = '')
*                                          ( package = 'A_A_A' parentpackage = 'A_A') ).
*
*    CREATE OBJECT model.
*
*    CREATE OBJECT famix_package EXPORTING model = model.
*
*    DATA: mse_model_act TYPE z2mse_model=>lines_type,
*          mse_model_exp TYPE z2mse_model=>lines_type.
*
*    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
*                             ( line = |  (name 'A'))| )
*                             ( line = |(FAMIX.Package (id: 2 )| )
*                             ( line = |  (name 'A_A_A')| )
*                             ( line = |  (parentPackage (ref: 3)))| )
*                             ( line = |(FAMIX.Package (id: 3 )| )
*                             ( line = |  (name 'A_A')))| )
*                             ).
*
*    f_cut->add_selected_packages_to_model( famix_package = famix_package ).
*
*    model->make_mse( IMPORTING mse_model = mse_model_act ).
*
*    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
*          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
*
*    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).
*
*    equalized_harmonized_mse_exp = VALUE #(
*( |FAMIX.Package A| )
*( |FAMIX.Package A_A| )
*( |FAMIX.Package A_A_A parentPackage A_A| )
*
**( |FAMIX.Package A| )
**( |FAMIX.Class CLASS_A modifiers ABAPGlobalClass| )
**( |FAMIX.Attribute CLASS_A>>ATTRIBUTE_A| )
**( |FAMIX.Method CLASS_A>>METHOD_A signature METHOD_A| )
**( |FAMIX.Class INTERFACE_A modifiers ABAPGlobalInterface| )
**( |FAMIX.Class INTERFACE_A isInterface true  | )
*    ).
*
*    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = equalized_harmonized_mse_act
*        exp                  = equalized_harmonized_mse_exp
*        msg                  = 'Wrong mse file for new class' ).
*
*
*  ENDMETHOD.

ENDCLASS.
