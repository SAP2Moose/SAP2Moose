CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: my_lines_type TYPE z2mse_model=>lines_type,
           ty_exp_mse    TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY,
           ty_act_mse    TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check,
      second_test FOR TESTING RAISING cx_static_check,
      _handle_test
        IMPORTING
          i_this_test_case  TYPE z2mse_chartest-testcase
          i_write_reference TYPE abap_bool
          i_mse_model       TYPE ltcl_main=>my_lines_type
        EXPORTING
          e_exp_mse         TYPE ty_exp_mse
          e_act_mse         TYPE ty_act_mse.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD first_test.

    DATA: mse_model TYPE my_lines_type.
    DATA f_cut TYPE REF TO  z2mse_extract_sap.

*    TYPES:         ty_s_compsn                       TYPE RANGE OF tadir-obj_name.
*    TYPES:         ty_s_pack                         TYPE RANGE OF tadir-devclass.

    DATA components TYPE z2mse_extract_sap=>ty_s_compsn.
    DATA packages TYPE z2mse_extract_sap=>ty_s_pack.

    f_cut = NEW #(
        i_g_filter_using_package       = abap_true
        i_g_filter_using_name          = abap_false
        i_g_parameter_package_to_analz = 'YRW1_MOOSE_TEST'
        i_p_iprog                      = abap_false
        i_p_clas                       = abap_true
        i_p_wdyn                       = abap_true
        i_p_intf                       = abap_true
        i_p_prog                       = abap_true
        i_p_tables                     = abap_true
        i_s_compsn                     = components
        i_s_pack                       = packages
        i_g_param_usage_outpack_groupd = abap_false ).

    DATA nothing_done TYPE boolean.
    f_cut->extract( IMPORTING mse_model    = mse_model
                                      nothing_done = nothing_done ).
    DATA this_test_case TYPE z2mse_chartest-testcase VALUE 'FIRST'.
    DATA write_reference TYPE abap_bool VALUE abap_false.

    DATA exp_mse TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.
    DATA act_mse TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.

    _handle_test( EXPORTING i_mse_model = mse_model
                            i_this_test_case = this_test_case
                            i_write_reference = write_reference
                  IMPORTING e_exp_mse = exp_mse
                            e_act_mse = act_mse ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act                  = act_mse
                                                  exp                  = exp_mse
                                                  msg                  = 'Main package YRW1_MOOSE_TEST Objects outside of package not grouped' ).

  ENDMETHOD.

  METHOD second_test.

    DATA: mse_model TYPE my_lines_type.
    DATA f_cut TYPE REF TO  z2mse_extract_sap.

*    TYPES:         ty_s_compsn                       TYPE RANGE OF tadir-obj_name.
*    TYPES:         ty_s_pack                         TYPE RANGE OF tadir-devclass.

    DATA components TYPE z2mse_extract_sap=>ty_s_compsn.
    DATA packages TYPE z2mse_extract_sap=>ty_s_pack.

    f_cut = NEW #(
        i_g_filter_using_package       = abap_true
        i_g_filter_using_name          = abap_false
        i_g_parameter_package_to_analz = 'YRW1_MOOSE_TEST'
        i_p_iprog                      = abap_false
        i_p_clas                       = abap_true
        i_p_wdyn                       = abap_true
        i_p_intf                       = abap_true
        i_p_prog                       = abap_true
        i_p_tables                     = abap_true
        i_s_compsn                     = components
        i_s_pack                       = packages
        i_g_param_usage_outpack_groupd = abap_true ).

    DATA nothing_done TYPE boolean.
    f_cut->extract( IMPORTING mse_model    = mse_model
                                      nothing_done = nothing_done ).
    DATA this_test_case TYPE z2mse_chartest-testcase VALUE 'SECOND'.
    DATA write_reference TYPE abap_bool VALUE abap_false.

    DATA exp_mse TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.
    DATA act_mse TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.

    _handle_test( EXPORTING i_mse_model = mse_model
                            i_this_test_case = this_test_case
                            i_write_reference = write_reference
                  IMPORTING e_exp_mse = exp_mse
                            e_act_mse = act_mse ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act                  = act_mse
                                                  exp                  = exp_mse
                                                  msg                  = 'Main package YRW1_MOOSE_TEST Objects outside of package grouped' ).

  ENDMETHOD.

  METHOD _handle_test.

    IF i_write_reference EQ abap_true.
      DELETE FROM z2mse_chartest WHERE testcase = i_this_test_case.

      LOOP AT i_mse_model INTO DATA(line).

        e_exp_mse = VALUE #( BASE e_exp_mse ( mandt = sy-mandt testcase = i_this_test_case line = sy-tabix content = line-line ) ).

      ENDLOOP.

      INSERT z2mse_chartest FROM TABLE e_exp_mse.

      COMMIT WORK.

    ELSE.

      SELECT * FROM z2mse_chartest INTO TABLE e_exp_mse WHERE testcase = i_this_test_case ORDER BY line.

    ENDIF.

    LOOP AT i_mse_model INTO line.

      e_act_mse = VALUE #( BASE e_act_mse ( mandt = sy-mandt testcase = i_this_test_case line = sy-tabix content = line-line ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
