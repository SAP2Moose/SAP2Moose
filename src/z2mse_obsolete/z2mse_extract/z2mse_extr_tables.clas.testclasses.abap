CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr_tables DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      simple_model_from_package FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD simple_model_from_package.

    DATA: model           TYPE REF TO z2mse_model,
          famix_package   TYPE REF TO z2mse_famix_package,
          famix_class     TYPE REF TO z2mse_famix_class,
          famix_attribute TYPE REF TO z2mse_famix_attribute.

    DATA: tadir_test TYPE z2mse_extr_tables=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'TABL' obj_name = 'TABLE_A' devclass = 'A' ) ).

    DATA: dd02l_test TYPE z2mse_extr_tables=>ty_t_dd02l_test.

    dd02l_test = VALUE #( ( tabname = 'TABLE_A' ) ).

    DATA f_cut TYPE REF TO z2mse_extr_tables.

    f_cut = NEW #( tadir_test = tadir_test dd02l_test = dd02l_test ).

    DATA packages  TYPE z2mse_extr_packages=>ty_packages.

    packages = VALUE #( ( package = 'A' ) ).

    f_cut->select_tables_by_packages( packages = packages ).

    data: tables_to_do_act type z2mse_extr_tables=>ty_tables_public,
          tables_to_do_exp type z2mse_extr_tables=>ty_tables_public.

    tables_to_do_act = f_cut->get_tables_to_do_where_used( ).

    tables_to_do_exp = value #( ( tabname = 'TABLE_A') ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = tables_to_do_act
        exp                  = tables_to_do_exp
        msg                  = 'Expect correct list of tables to do' ).

    tables_to_do_act = f_cut->get_tables_to_do_where_used( ).

    tables_to_do_exp = value #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = tables_to_do_act
        exp                  = tables_to_do_exp
        msg                  = 'Do not return tables to do a second tme' ).

    DATA: mse_model_act TYPE z2mse_model=>lines_type,
          mse_model_exp TYPE z2mse_model=>lines_type.

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'TABLE_A')| )
                             ( line = |  (modifiers 'DBTable')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Attribute (id: 3 )| )
                             ( line = |  (name 'TABLE_A')| )
                             ( line = |  (parentType (ref: 2))))| )
                             ).

    f_cut->add_to_model( EXPORTING famix_package   = famix_package
                                   famix_class     = famix_class
                                   famix_attribute = famix_attribute ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.
    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).
    equalized_harmonized_mse_exp = VALUE #(


( |FAMIX.Attribute TABLE_A>>TABLE_A| )
( |FAMIX.Class TABLE_A modifiers DBTable| )
( |FAMIX.Class TABLE_A parentPackage A| )
( |FAMIX.Package A| )

    ).
    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).


    cl_abap_unit_assert=>assert_equals( EXPORTING act                  = equalized_harmonized_mse_act
                                                  exp                  = equalized_harmonized_mse_exp
                                                  msg                  = 'Wrong mse file for new table' ).

  ENDMETHOD.

ENDCLASS.
