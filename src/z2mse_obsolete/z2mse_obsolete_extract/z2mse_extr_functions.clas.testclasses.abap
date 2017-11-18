CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_extr_functions DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO z2mse_extr_functions.
    METHODS:
      _convert_fugr_progrname_2_fugr FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD _convert_fugr_progrname_2_fugr.

    DATA: fugr_progname TYPE progname,
          fugr_act      TYPE string,
          fugr_exp      TYPE string.
    fugr_progname = |SAPLFUGR|.

    fugr_exp = |FUGR|.

    f_cut = NEW #( ).

    fugr_act = f_cut->_convert_fugr_progname_2_fugr( fugr_progname = fugr_progname ).

    cl_abap_unit_assert=>assert_equals( msg = 'Expect correc function group name' exp = fugr_exp act = fugr_act ).

  ENDMETHOD.

ENDCLASS.
