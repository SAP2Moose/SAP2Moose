CLASS ltcl_end_to_end_packages DEFINITION DEFERRED.
CLASS z2mse_extract_sap2 DEFINITION LOCAL FRIENDS ltcl_end_to_end_packages.
CLASS ltcl_end_to_end_packages DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO z2mse_extract_sap2.
    METHODS:
      simple FOR TESTING RAISING cx_static_check,
      simple_classes FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_end_to_end_packages IMPLEMENTATION.

  METHOD simple.

    TEST-INJECTION creator_packages.
      DATA: tdevc_test TYPE z2mse_extr_packages=>ty_t_tdevc_test.

      tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' )
                            ( devclass = 'A_A' parentcl = 'A' )
                            ( devclass = 'A_A_A' parentcl = 'A_A' ) ).
      extract_packages = NEW #( tdevc_test = tdevc_test ).
    END-TEST-INJECTION.

    TEST-INJECTION creator_classes.
      extract_classes = NEW #( tadir_test = VALUE #( ( ) ) ).
    END-TEST-INJECTION.

    TEST-INJECTION creator_tables.
      extract_tables = NEW #( tadir_test = VALUE #( ( ) ) ).
    END-TEST-INJECTION.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA: top_packages        TYPE z2mse_extract_sap2=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extract_sap2=>ty_s_pack.
    top_packages = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' ) ).
    sub_packages_filter = VALUE #( ( sign = 'E' option = 'EQ' low = 'A_A' ) ).
    f_cut = NEW #( ).
    f_cut->extract(
      EXPORTING
        i_top_packages        = top_packages
        i_sub_packages_filter = sub_packages_filter
        i_search_sub_packages = abap_true
        i_search_up           = 1
      IMPORTING
        mse_model             = mse_model_act
        nothing_done          = nothing_done_act ).

    DATA mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Package (id: 2 )| )
                             ( line = |  (name 'A_A_A')| )
                             ( line = |  (parentPackage (ref: 3)))| )
                             ( line = |(FAMIX.Package (id: 3 )| )
                             ( line = |  (name 'A_A')))| )
                             ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model_act
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

  METHOD simple_classes.

    TEST-INJECTION creator_packages.
      "DATA: tdevc_test TYPE yrw1_mcextr_packages=>ty_t_tdevc_test.

      tdevc_test = VALUE #( ( devclass = 'A' parentcl = '' ) ).
      extract_packages = NEW #( tdevc_test = tdevc_test ).
    END-TEST-INJECTION.

    TEST-INJECTION creator_classes.
      DATA: ttadir_test    TYPE z2mse_extr_classes=>ty_t_tadir_test,
            tseoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test,
            tseocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

      ttadir_test = VALUE #( ( object = 'CLAS' obj_name = 'CLASSA' devclass = 'A' )
                             ( object = 'CLAS' obj_name = 'CLASS_NOT_EXISTING' devclass = 'A' ) ).
      tseoclass_test = VALUE #( ( clsname = 'CLASSA') ).
      tseocompo_test = VALUE #( ( clsname = 'CLASSA' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).
      extract_classes = NEW #( tadir_test = ttadir_test
                               seoclass_test = tseoclass_test
                               seocompo_test = tseocompo_test ).
    END-TEST-INJECTION.

    TEST-INJECTION creator_tables.
      extract_tables = NEW #( tadir_test = VALUE #( ( ) ) ).
    END-TEST-INJECTION.

    DATA: mse_model_act TYPE z2mse_model=>lines_type.
    DATA: nothing_done_act TYPE abap_bool.
    DATA: top_packages        TYPE z2mse_extract_sap2=>ty_s_pack,
          sub_packages_filter TYPE z2mse_extract_sap2=>ty_s_pack.
    top_packages = VALUE #( ( sign = 'I' option = 'EQ' low = 'A' ) ).
    sub_packages_filter = VALUE #( ( sign = 'E' option = 'EQ' low = 'A_A' ) ).
    f_cut = NEW #( ).
    f_cut->extract(
      EXPORTING
        i_top_packages        = top_packages
        i_sub_packages_filter = sub_packages_filter
        i_search_sub_packages = abap_true
        i_search_up           = 1
      IMPORTING
        mse_model             = mse_model_act
        nothing_done          = nothing_done_act ).

    DATA mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'CLASSA')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Method (id: 3 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 2))))| )
                             ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model_act
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

ENDCLASS.
