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
      simple_model_from_component FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD simple.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'CLASS' obj_name = 'CLASS_D' devclass = 'OTHER' )
                          ( object = 'CLASS' obj_name = 'CLASS_A_NOT_EXISTING' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'A' )
                          ( object = 'CLASS' obj_name = 'CLASS_B' devclass = 'B' )
                          ( object = 'INTF' obj_name = 'INTERFACE_B' devclass = 'B' ) ).

    data: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = value #( ( clsname = 'CLASS_A')
                             ( clsname = 'CLASS_B')
                             ( clsname = 'CLASS_D')
                             ( clsname = 'INTERFACE_A')
                             ( clsname = 'INTERFACE_B') ).

    data: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = value #( ( clsname = 'INTERFACE_A' cmpname = 'METHOD_A' CMPTYPE = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' CMPTYPE = z2mse_extr_classes=>event_type )
                             ( clsname = 'CLASS_D' cmpname = 'METHOD_A' CMPTYPE = z2mse_extr_classes=>method_type )
                              ).

    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = value #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA: selected_classes_act TYPE z2mse_extr_classes=>ty_classes,
          selected_classes_exp TYPE z2mse_extr_classes=>ty_classes.

    selected_classes_act = f_cut->g_selected_classes.
    selected_classes_exp = VALUE #( ( clstype = z2mse_extr_classes=>class_type clsname = 'CLASS_A' devclass = 'A' exists = 'X' )
                                  ( clstype = z2mse_extr_classes=>interface_type clsname = 'INTERFACE_A' devclass = 'A' exists = 'X' ) ).

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
                                       ( clsname = 'INTERFACE_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = selected_components_act
        exp                  = selected_components_exp
        msg                  = 'Select correct class components' ).



    " Add further classes and components due to a where used analysis



    data add_components TYPE z2mse_extr_classes=>ty_class_components_hashed.

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type )
                              ( clsname = 'CLASS_D' cmpname = 'METHOD_A' CMPTYPE = z2mse_extr_classes=>method_type ) ).

    f_cut->select_classes_by_components( components = add_components ).


        DATA: add_classes_act TYPE z2mse_extr_classes=>ty_classes,
          add_classes_exp TYPE z2mse_extr_classes=>ty_classes.

    add_classes_act = f_cut->g_add_classes.
    add_classes_exp = VALUE #( ( clstype = z2mse_extr_classes=>class_type clsname = 'CLASS_D' devclass = 'OTHER' exists = 'X' ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = add_classes_act
        exp                  = add_classes_exp
        msg                  = 'Add correct classes, but only if class is not already selected' ).

    DATA: add_components_act TYPE z2mse_extr_classes=>ty_class_components,
          add_components_exp TYPE z2mse_extr_classes=>ty_class_components.

    add_components_act = f_cut->g_add_components.
    add_components_exp = VALUE #( ( clsname = 'CLASS_D' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = add_components_act
        exp                  = add_components_exp
        msg                  = 'Add correct class components, but only if class is not already selected' ).


  ENDMETHOD.





  METHOD simple_model_from_package.

    DATA: model            TYPE REF TO z2mse_model,
          famix_package TYPE REF TO z2mse_famix_package,
          famix_class     TYPE REF TO Z2MSE_famix_class,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute     TYPE REF TO Z2MSE_famix_attribute.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'A' ) ).

    data: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = value #( ( clsname = 'CLASS_A')
                             ( clsname = 'INTERFACE_A') ).

    data: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = value #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' CMPTYPE = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type )
                             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' CMPTYPE = z2mse_extr_classes=>event_type ) ).


    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = value #( ( package = 'A' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    DATA: selected_tadir_act TYPE z2mse_extr_classes=>ty_t_tadir_test,
          selected_tadir_exp TYPE z2mse_extr_classes=>ty_t_tadir_test.

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.

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
                                                    famix_attribute = famix_attribute ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model_act
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

  METHOD simple_model_from_component.

    DATA: model            TYPE REF TO z2mse_model,
          famix_package TYPE REF TO z2mse_famix_package,
          famix_class     TYPE REF TO Z2MSE_famix_class,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute     TYPE REF TO Z2MSE_famix_attribute.

    DATA: tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.

    tadir_test = VALUE #( ( object = 'CLASS' obj_name = 'CLASS_A' devclass = 'A' )
                          ( object = 'INTF' obj_name = 'INTERFACE_A' devclass = 'A' ) ).

    data: seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.

    seoclass_test = value #( ( clsname = 'CLASS_A')
                             ( clsname = 'INTERFACE_A') ).

    data: seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.

    seocompo_test = value #( ( clsname = 'CLASS_A' cmpname = 'METHOD_A' CMPTYPE = z2mse_extr_classes=>method_type )
                             ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type ) ).


    f_cut = NEW #( tadir_test = tadir_test
                   seoclass_test = seoclass_test
                   seocompo_test = seocompo_test ).

    DATA: packages_to_select TYPE z2mse_extr_packages=>ty_packages.

    packages_to_select = value #( ( package = 'NOT_EXISTING' ) ).

    f_cut->select_classes_by_packages( packages = packages_to_select ).

    data add_components TYPE z2mse_extr_classes=>ty_class_components_hashed.

    " Add only a class if it not originally selected

    add_components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type )
                              ( clsname = 'INTERFACE_A' cmpname = 'ATTRIBUTE_A' CMPTYPE = z2mse_extr_classes=>attribute_type ) ).

    f_cut->select_classes_by_components( components = add_components ).

    DATA: selected_tadir_act TYPE z2mse_extr_classes=>ty_t_tadir_test,
          selected_tadir_exp TYPE z2mse_extr_classes=>ty_t_tadir_test.

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.

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
                                                    famix_attribute = famix_attribute ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model_act
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for classes added from components' ).

  ENDMETHOD.

ENDCLASS.
