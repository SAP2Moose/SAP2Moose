CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_extr_classes DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO z2mse_extr_classes.
    METHODS:
      simple FOR TESTING RAISING cx_static_check,
      simple_model_from_package FOR TESTING RAISING cx_static_check,
      simple_model_from_component FOR TESTING RAISING cx_static_check,
      repeated_call FOR TESTING RAISING cx_static_check,
      simple_interface FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD simple.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'CLASS' obj_name = 'CLASS_D' devclass = 'OTHER' )
                          ( object = 'CLASS' obj_name = 'CLASS_A_NOT_EXISTING' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'ZINTERFACE_A' devclass = 'A' )
                          ( object = 'CLASS' obj_name = 'CLASS_B' devclass = 'B' )
                          ( object = 'INTF' obj_name = 'ZINTERFACE_B' devclass = 'B' ) ).

    DATA: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = VALUE #( ( clsname = 'CLASS_A')
                             ( clsname = 'CLASS_B')
                             ( clsname = 'CLASS_D')
                             ( clsname = 'ZINTERFACE_A')
                             ( clsname = 'ZINTERFACE_B') ).

    DATA: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = VALUE #( ( clsname = 'ZINTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type )
                             ( clsname = 'CLASS_D' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                              ).

    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test
                   i_exclude_found_sap_intf = abap_false ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = VALUE #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA: selected_classes_act TYPE z2mse_extr_classes=>ty_classes,
          selected_classes_exp TYPE z2mse_extr_classes=>ty_classes.

    selected_classes_act = f_cut->g_selected_classes.
    selected_classes_exp = VALUE #( ( clstype = z2mse_extr_classes=>class_type clsname = 'CLASS_A' devclass = 'A' exists = 'X' )
                                  ( clstype = z2mse_extr_classes=>interface_type clsname = 'ZINTERFACE_A' devclass = 'A' exists = 'X' ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_classes_act
        exp                  = selected_classes_exp
        msg                  = 'Select correct classes' ).

    DATA: selected_components_act TYPE z2mse_extr_classes=>ty_class_components,
          selected_components_exp TYPE z2mse_extr_classes=>ty_class_components.

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                                       ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type )
                                       ( clsname = 'ZINTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Nothing is selected if method get_comp_to_do_where_used is called a second time' ).




    " Add further classes and components due to a where used analysis



    DATA add_components TYPE z2mse_extr_classes=>ty_class_components_hashed.

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                              ( clsname = 'CLASS_D' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    f_cut->select_classes_by_components( components = add_components ).


*    DATA: add_classes_act TYPE z2mse_extr_classes=>ty_classes,
*          add_classes_exp TYPE z2mse_extr_classes=>ty_classes.
*
*    add_classes_act = f_cut->g_add_classes.
*    add_classes_exp = VALUE #( ( clstype = z2mse_extr_classes=>class_type clsname = 'CLASS_D' devclass = 'OTHER' exists = 'X' ) ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = add_classes_act
*        exp                  = add_classes_exp
**        msg                  = 'Add correct classes, but only if class is not already selected' ).
*
*    DATA: add_components_act TYPE z2mse_extr_classes=>ty_class_components,
*          add_components_exp TYPE z2mse_extr_classes=>ty_class_components.
*
*    add_components_act = f_cut->g_add_components.
*    add_components_exp = VALUE #( ( clsname = 'CLASS_D' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act                  = add_components_act
*        exp                  = add_components_exp
*        msg                  = 'Add correct class components, but only if class is not already selected' ).


  ENDMETHOD.

  METHOD simple_interface.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'B' ) ).

    DATA: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = VALUE #( ( clsname = 'CLASS_A')
                             ( clsname = 'INTERFACE_A') ).

    DATA: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'INTERFACE_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'INTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                              ).

    DATA: seometarel_test TYPE z2mse_extr_classes=>ty_t_seometarel_test.

    seometarel_test = VALUE #( ( clsname = 'CLASS_A' refclsname = 'INTERFACE_A'  version = 1 state = 1 reltype = 1 )
                              ).

    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test
                   seometarel_test = seometarel_test
                   i_exclude_found_sap_intf = abap_false ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = VALUE #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA: selected_classes_act TYPE z2mse_extr_classes=>ty_classes,
          selected_classes_exp TYPE z2mse_extr_classes=>ty_classes.

    selected_classes_act = f_cut->g_selected_classes.
    selected_classes_exp = VALUE #( ( clstype = z2mse_extr_classes=>class_type clsname = 'CLASS_A' devclass = 'A' exists = 'X' )
                                    ( clstype = z2mse_extr_classes=>interface_type clsname = 'INTERFACE_A' devclass = '' exists = 'X' ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_classes_act
        exp                  = selected_classes_exp
        msg                  = 'Select correct classes' ).

    DATA: selected_components_act TYPE z2mse_extr_classes=>ty_class_components,
          selected_components_exp TYPE z2mse_extr_classes=>ty_class_components.

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                                       ( clsname = 'CLASS_A' cmpname = 'INTERFACE_A~ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                                       ( clsname = 'CLASS_A' cmpname = 'INTERFACE_A~METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                                       ( clsname = 'INTERFACE_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                                       ( clsname = 'INTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).

  ENDMETHOD.



  METHOD simple_model_from_package.

    DATA: model            TYPE REF TO z2mse_model,
          famix_package    TYPE REF TO z2mse_famix_package,
          famix_class      TYPE REF TO z2mse_famix_class,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute  TYPE REF TO z2mse_famix_attribute,
          famix_invocation TYPE REF TO z2mse_famix_invocation,
          famix_access     TYPE REF TO z2mse_famix_access.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'A' ) ).

    DATA: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = VALUE #( ( clsname = 'CLASS_A')
                             ( clsname = 'INTERFACE_A') ).

    DATA: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = VALUE #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type ) ).


    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test
                   i_exclude_found_sap_intf = abap_false ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = VALUE #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

    DATA: mse_model_act TYPE z2mse_model=>lines_type,
          mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'CLASS_A')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Attribute (id: 3 )| )
                             ( line = |  (name 'ATTRIBUTE_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Method (id: 4 )| )
                             ( line = |  (name 'EVENT_A')| )
                             ( line = |  (signature 'EVENT_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Method (id: 5 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Class (id: 6 )| )
                             ( line = |  (name 'INTERFACE_A')| )
                             ( line = |  (modifiers 'ABAPGlobalInterface')| )
                             ( line = |  (parentPackage (ref: 1))| )
                             ( line = |  (isInterface true)))| )
                             ).

    f_cut->add_to_model( EXPORTING famix_package   = famix_package
                                                    famix_class     = famix_class
                                                    famix_method    = famix_method
                                                    famix_attribute = famix_attribute
                                                    famix_invocation = famix_invocation
                                                    famix_access = famix_access ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
( |FAMIX.Package A| )
( |FAMIX.Class CLASS_A modifiers ABAPGlobalClass| )
( |FAMIX.Class CLASS_A parentPackage A| )
( |FAMIX.Attribute CLASS_A>>ATTRIBUTE_A| )
( |FAMIX.Method CLASS_A>>EVENT_A signature EVENT_A| )
( |FAMIX.Method CLASS_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Class INTERFACE_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class INTERFACE_A parentPackage A| )
( |FAMIX.Class INTERFACE_A isInterface true| )
    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

  METHOD simple_model_from_component.

    DATA: model            TYPE REF TO z2mse_model,
          famix_package    TYPE REF TO z2mse_famix_package,
          famix_class      TYPE REF TO z2mse_famix_class,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute  TYPE REF TO z2mse_famix_attribute,
          famix_invocation TYPE REF TO z2mse_famix_invocation,
          famix_access     TYPE REF TO z2mse_famix_access.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'A' ) ).

    DATA: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = VALUE #( ( clsname = 'CLASS_A')
                             ( clsname = 'INTERFACE_A') ).

    DATA: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = VALUE #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type ) ).


    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test
                   i_exclude_found_sap_intf = abap_false ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = VALUE #( ( package = 'NOT_EXISTING' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA add_components TYPE z2mse_extr_classes=>ty_class_components_hashed.

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                              ( clsname = 'INTERFACE_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type ) ).

    f_cut->select_classes_by_components( components = add_components ).

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

    DATA: mse_model_act TYPE z2mse_model=>lines_type,
          mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'CLASS_A')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Attribute (id: 3 )| )
                             ( line = |  (name 'ATTRIBUTE_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Method (id: 4 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Class (id: 5 )| )
                             ( line = |  (name 'INTERFACE_A')| )
                             ( line = |  (modifiers 'ABAPGlobalInterface')| )
                             ( line = |  (parentPackage (ref: 1))| )
                             ( line = |  (isInterface true)))| )
                             ).

    f_cut->add_to_model( EXPORTING famix_package   = famix_package
                                                    famix_class     = famix_class
                                                    famix_method    = famix_method
                                                    famix_attribute = famix_attribute
                                                    famix_invocation = famix_invocation
                                                    famix_access = famix_access ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(
( |FAMIX.Package A| )
( |FAMIX.Class CLASS_A modifiers ABAPGlobalClass| )
( |FAMIX.Class CLASS_A parentPackage A| )
( |FAMIX.Attribute CLASS_A>>ATTRIBUTE_A| )
( |FAMIX.Method CLASS_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Class INTERFACE_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class INTERFACE_A isInterface true  | )
( |FAMIX.Class INTERFACE_A parentPackage A  | )
    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for classes added from components' ).

  ENDMETHOD.

  METHOD repeated_call.

    DATA: model            TYPE REF TO z2mse_model,
          famix_package    TYPE REF TO z2mse_famix_package,
          famix_class      TYPE REF TO z2mse_famix_class,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute  TYPE REF TO z2mse_famix_attribute,
          famix_invocation TYPE REF TO z2mse_famix_invocation,
          famix_access     TYPE REF TO z2mse_famix_access.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'CLASS' obj_name = 'CLASS_B' devclass = '' )
                          ( object = 'CLASS' obj_name = 'CLASS_C' devclass = '' )
                          ( object = 'INTF' obj_name = 'SAP_INTERFACE_A' devclass = '' ) ).

    DATA: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = VALUE #( ( clsname = 'CLASS_A')
                             ( clsname = 'CLASS_B')
                             ( clsname = 'CLASS_C')
                             ( clsname = 'SAP_INTERFACE_A') ).

    DATA: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = VALUE #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_C' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                             ( clsname = 'SAP_INTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).


    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test
                   i_exclude_found_sap_intf = abap_true ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = VALUE #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA: selected_components_act TYPE z2mse_extr_classes=>ty_class_components,
          selected_components_exp TYPE z2mse_extr_classes=>ty_class_components.

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    " SAP_2_FAMIX_65 : The SAP Interface will not be returned to do a where used analysis
    selected_components_exp = VALUE #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Nothing is selected if method get_comp_to_do_where_used is called a second time' ).

    DATA add_components TYPE z2mse_extr_classes=>ty_class_components_hashed.

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                              ( clsname = 'SAP_INTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    f_cut->select_classes_by_components( components = add_components ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Nothing is selected if method get_comp_to_do_where_used is called a second time' ).

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                              ( clsname = 'CLASS_C' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    f_cut->select_classes_by_components( components = add_components ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ( clsname = 'CLASS_C' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).

    selected_components_act = f_cut->get_comp_to_do_where_used( ).
    selected_components_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Nothing is selected if method get_comp_to_do_where_used is called a second time' ).

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

    DATA: mse_model_act TYPE z2mse_model=>lines_type,
          mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'A'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'CLASS_A')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Method (id: 3 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Package (id: 4 )| )
                             ( line = |  (name ''))| )
                             ( line = |(FAMIX.Class (id: 5 )| )
                             ( line = |  (name 'CLASS_B')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 6 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 5)))| )
                             ( line = |(FAMIX.Class (id: 7 )| )
                             ( line = |  (name 'SAP_INTERFACE_A')| )
                             ( line = |  (modifiers 'ABAPGlobalInterface')| )
                             ( line = |  (parentPackage (ref: 4))| )
                             ( line = |  (isInterface true))| )
                             ( line = |(FAMIX.Method (id: 8 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 7)))| )
                             ( line = |(FAMIX.Class (id: 9 )| )
                             ( line = |  (name 'CLASS_C')| )
                             ( line = |  (modifiers 'ABAPGlobalClass')| )
                             ( line = |  (parentPackage (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 10 )| )
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (signature 'METHOD_A')| )
                             ( line = |  (parentType (ref: 9))))| )
                             ).

    f_cut->add_to_model( EXPORTING famix_package   = famix_package
                                                    famix_class     = famix_class
                                                    famix_method    = famix_method
                                                    famix_attribute = famix_attribute
                                                    famix_invocation = famix_invocation
                                                    famix_access = famix_access ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    DATA: equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse_model_act ).

    equalized_harmonized_mse_exp = VALUE #(

( |FAMIX.Package A| )
( |FAMIX.Package| )
( |FAMIX.Class CLASS_A modifiers ABAPGlobalClass| )
( |FAMIX.Class CLASS_A parentPackage A| )
( |FAMIX.Method CLASS_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Class CLASS_B modifiers ABAPGlobalClass| )
( |FAMIX.Class CLASS_B parentPackage| )
( |FAMIX.Method CLASS_B>>METHOD_A signature METHOD_A| )
( |FAMIX.Class SAP_INTERFACE_A modifiers ABAPGlobalInterface| )
( |FAMIX.Class SAP_INTERFACE_A parentPackage| )
( |FAMIX.Class SAP_INTERFACE_A isInterface true| )
( |FAMIX.Method SAP_INTERFACE_A>>METHOD_A signature METHOD_A| )
( |FAMIX.Class CLASS_C modifiers ABAPGlobalClass| )
( |FAMIX.Class CLASS_C parentPackage| )
( |FAMIX.Method CLASS_C>>METHOD_A signature METHOD_A  | )

    ).

    z2mse_mse_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = equalized_harmonized_mse_act
        exp                  = equalized_harmonized_mse_exp
        msg                  = 'Wrong mse file for classes added from components' ).

  ENDMETHOD.

ENDCLASS.
