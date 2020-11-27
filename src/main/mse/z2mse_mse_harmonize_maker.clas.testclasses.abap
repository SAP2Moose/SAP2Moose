CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO z2mse_mse_harmonize_maker.
    METHODS:
      add_package FOR TESTING RAISING cx_static_check,
      add_table FOR TESTING RAISING cx_static_check,
      add_class FOR TESTING RAISING cx_static_check,
      add_method FOR TESTING RAISING cx_static_check,
      add_attribute FOR TESTING RAISING cx_static_check,
      add_interface FOR TESTING RAISING cx_static_check,
      add_interface_method FOR TESTING RAISING cx_static_check,
      add_interface_attribute FOR TESTING RAISING cx_static_check,
      add_web_dynpro_component FOR TESTING RAISING cx_static_check,
      add_program FOR TESTING RAISING cx_static_check,
      usage FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add_package.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_package( package = |Z2MSE_TEST_INITIAL_SELECTION| ).

    t_exp = VALUE #( ( |FAMIX.Package Z2MSE_TEST_INITIAL_SELECTION| )  ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new package' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_table.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_db_table( name = |Z2MSE_TEST_A|
                         parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    t_exp = VALUE #( ( |FAMIX.Class Z2MSE_TEST_A modifiers DBTable| )
                     ( |FAMIX.Class Z2MSE_TEST_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
                     ( |FAMIX.Attribute Z2MSE_TEST_A>>Z2MSE_TEST_A| )  ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new table' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_class.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_class( name          = |Z2MSE_TEST_CL_A|
                      parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    t_exp = VALUE #( ( |FAMIX.Class Z2MSE_TEST_CL_A modifiers ABAPGlobalClass| )
                     ( |FAMIX.Class Z2MSE_TEST_CL_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
                     ( |FAMIX.FileAnchor Z2MSE_TEST_CL_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main| )
                     ( |FAMIX.Class Z2MSE_TEST_CL_A sourceAnchor| )  ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_method.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_method( class          = |Z2MSE_TEST_CL_A|
                       method = |EVENTHANDLER_A|
                       at_line = 13 ).

    t_exp = VALUE #( ( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A signature EVENTHANDLER_A| )
                     ( |FAMIX.FileAnchor EVENTHANDLER_A fileName adt://NPL/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=13,1| )
                     ( |FAMIX.Method Z2MSE_TEST_CL_A>>EVENTHANDLER_A sourceAnchor| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_attribute.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_attribute( class          = |Z2MSE_TEST_CL_A|
                       attribute = |ATTRIBUTE_A|
                       at_line = 13 ).

    t_exp = VALUE #( ( |FAMIX.Attribute Z2MSE_TEST_CL_A>>ATTRIBUTE_A sourceAnchor| )
                     ( |FAMIX.FileAnchor ATTRIBUTE_A fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/z2mse_test_cl_a/source/main#start=13,1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_interface.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_interface( name          = |Z2MSE_TEST_IF_A_00000000000000|
                          parentpackage = |Z2MSE_TEST_NO_INITIAL_SELECTN| ).

    t_exp = VALUE #( ( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 isInterface true| )
                     ( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 modifiers ABAPGlobalInterface| )
                     ( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 parentPackage Z2MSE_TEST_NO_INITIAL_SELECTN| )
                     ( |FAMIX.FileAnchor Z2MSE_TEST_IF_A_00000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main| )
                     ( |FAMIX.Class Z2MSE_TEST_IF_A_00000000000000 sourceAnchor| )  ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new interface' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_interface_method.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_interface_method( interface = |Z2MSE_TEST_IF_A_00000000000000|
                                    method = |METHOD_A_000000000000000000000|
                                   at_line = 9 ).

    t_exp = VALUE #( ( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 signature METHOD_A_000000000000000000000| )
                     ( |FAMIX.Method Z2MSE_TEST_IF_A_00000000000000>>METHOD_A_000000000000000000000 sourceAnchor| )
                     ( |FAMIX.FileAnchor METHOD_A_000000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=9,1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_interface_attribute.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_interface_attribute( interface = |Z2MSE_TEST_IF_A_00000000000000|
                                    attribute = |ATTRIBUTE_A_000000000000000000|
                                      at_line = 5 ).

    t_exp = VALUE #( ( |FAMIX.Attribute Z2MSE_TEST_IF_A_00000000000000>>ATTRIBUTE_A_000000000000000000 sourceAnchor| )
                     ( |FAMIX.FileAnchor ATTRIBUTE_A_000000000000000000 fileName adt://NPL/sap/bc/adt/oo/interfaces/z2mse_test_if_a_00000000000000/source/main#start=5,1| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_web_dynpro_component.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_web_dynpro_component( wda_name          = |Z2MSE_TEST_WDY_A|
                                     parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    t_exp = VALUE #( ( |FAMIX.Class Z2MSE_TEST_WDY_A modifiers ABAPWebDynproComponent| )
                     ( |FAMIX.Class Z2MSE_TEST_WDY_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD add_program.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->add_program( name          = |Z2MSE_TEST_PROGRAM_A|
                        parentpackage = |Z2MSE_TEST_INITIAL_SELECTION| ).

    t_exp = VALUE #( ( |FAMIX.Class Z2MSE_TEST_PROGRAM_A modifiers ABAPProgram| )
                     ( |FAMIX.Class Z2MSE_TEST_PROGRAM_A parentPackage Z2MSE_TEST_INITIAL_SELECTION| )
                     ( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A sourceAnchor| )
                     ( |FAMIX.FileAnchor Z2MSE_TEST_PROGRAM_A fileName adt://NPL/sap/bc/adt/programs/programs/z2mse_test_program_a| )
                     ( |FAMIX.Method Z2MSE_TEST_PROGRAM_A>>Z2MSE_TEST_PROGRAM_A signature Z2MSE_TEST_PROGRAM_A| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

  METHOD usage.
    DATA: t_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    f_cut = NEW #( ).
    f_cut->usage( using_group = |Z2MSE_TEST_CL_B1| using = |METHOD_A| used_group = |Z2MSE_TEST_CL_A| used = |METHOD_A| ).

    t_exp = VALUE #( ( |FAMIX.Invocation sender Z2MSE_TEST_CL_B1>>METHOD_A candidates Z2MSE_TEST_CL_A>>METHOD_A signature DUMMY| ) ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = f_cut->to_change ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = t_exp ).

    cl_abap_unit_assert=>assert_equals( msg = 'Wrong mse for new class' exp = t_exp act = f_cut->to_change ).

  ENDMETHOD.

ENDCLASS.
