CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      mainlogic FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD mainlogic.

    DATA model TYPE REF TO z2mse_model.
    DATA f_package TYPE REF TO z2mse_sap_package.
    DATA f_program TYPE REF TO z2mse_sap_program.
    DATA f_class TYPE REF TO z2mse_sap_class.
    DATA f_attributes TYPE REF TO Z2MSE_sap_attribute.
    DATA f_method TYPE REF TO z2mse_sap_method.
    DATA f_inheritance TYPE REF TO Z2MSE_sap_inheritance.
    DATA f_db_table TYPE REF TO Z2MSE_sap_db_table.
    data f_access TYPE REF TO Z2MSE_sap_access.
    data f_invocation TYPE REF TO z2mse_sap_invocation.

    model = NEW #( ).
    f_package = NEW #( model = model ).
    f_package->add( EXPORTING name = 'Package1' ).
    f_package->add( EXPORTING name = 'Package2' ).
    DATA(packages) = f_package->get_all_packages( ).
    DATA packages_exp TYPE z2mse_sap_package=>packages_type.
    packages_exp = VALUE #( ( CONV #('Package1') )
                            ( CONV #('Package2') ) ).

    f_package->set_parent_package( EXPORTING this_package   = 'Package1'
                                             parent_package = 'Package2' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = packages
        exp                  = packages_exp
        msg                  = 'Package List').

    f_program = NEW #( model = model ).

    DATA(id_program) = f_program->add( name = 'Program' ).

    f_program->set_parent_package( EXPORTING element_id     = id_program
                                             parent_package = 'Package1' ).

    f_class = NEW #( model = model ).

    f_class->add( EXPORTING name      = 'Class1'
                            modifiers = 'ClassModifier'
                  IMPORTING id        = DATA(id_class) ).


    f_class->add( EXPORTING name      = 'Class2'
                            modifiers = 'ClassModifier'
                  IMPORTING id        = DATA(id_class2) ).

    f_class->set_parent_package( EXPORTING element_id     = id_class
                                           parent_package = 'Package1' ).


    f_class->is_interface( element_id = id_class ).


    DATA(id_local_class) = f_class->add_local( program   = 'Program'
                                               name      = 'LocalClass'
                                               modifiers = 'modifier_local' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_local_class
                                                  exp = 6
                                                  msg = 'Check ID of local class' ).


    DATA(id_local_sub_class) = f_class->add_local( program   = 'Program'
                                               name      = 'LocalSubClass'
                                               modifiers = 'modifier_local' ).

    f_class->set_parent_program( EXPORTING element_id  = id_local_class
                                           sap_program = 'Program' ).

    f_attributes = NEW #( model = model ).

    f_attributes->add( EXPORTING class     = 'Class1'
                                 attribute = 'Attribute1' ).

    DATA(id_attribute) = f_attributes->get_id( EXPORTING class     = 'Class1'
                                                         attribute = 'Attribute1' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_attribute
                                                  exp = 8
                                                  msg = 'Check ID of attribute' ).

    f_method = NEW #( model = model ).

    DATA(id_method) = f_method->add( EXPORTING class  = 'Class1'
                                               method = 'Method1' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_method
                                                  exp = 9
                                                  msg = 'Check ID of method' ).

    DATA(id_method_read) = f_method->get_id( class  = 'Class1'
                                             method = 'Method1' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_method
                                                  exp = id_method_read
                                                  msg = 'ID of method has to be equal to the read ID' ).

    DATA(id_method_local) = f_method->add_local_method( class_name  = 'LocalClass'
                                                        class_id    = id_local_class
                                                        method_name = 'MethodOfLocalClass' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_method_local
                                                  exp = 10
                                                  msg = 'ID of local method' ).

    f_class->add( EXPORTING name      = 'ClassSub'
                            modifiers = 'ClassModifier'
                  IMPORTING id        = DATA(id_class_sub) ).


    f_class->add( EXPORTING name      = 'Interface'
                            modifiers = 'ClassModifier'
                  IMPORTING id        = DATA(id_interface) ).


    DATA(id_local_class_sub) = f_class->add_local( program   = 'Program'
                                                   name      = 'LocalClass'
                                                   modifiers = 'modifier_local' ).

    f_inheritance = NEW #( model = model ).

    DATA(id_inheritance_global) = f_inheritance->add( ).
    f_inheritance->set_sub_and_super_class( EXPORTING element_id      = id_inheritance_global
                                                      subclass_name   = 'Class2'
                                                      superclass_name = 'Class1' ).

    DATA(id_inheritance_interface_glob) = f_inheritance->add( ).
    f_inheritance->set_interface_for_class( EXPORTING element_id     = id_inheritance_interface_glob
                                                      interface_name = 'Interface'
                                                      class_name     = 'Class1' ).

    DATA(id_inheritance_local) = f_inheritance->add( ).
    f_inheritance->set_local_sub_and_super_class( EXPORTING element_id      = id_inheritance_local
                                                            program         = 'Program'
                                                            subclass_name   = 'LocalSubClass'
                                                            superclass_name =  'LocalClass' ).

    f_db_table = new #( model = model ).
    f_db_table->add( EXPORTING name                   = 'DBTable'
                     IMPORTING exists_already_with_id = data(id_exists_db_table)
                               id                     = data(id_db_table)
                               dummy_attribute_id     = data(id_dummy_attribute_db_table) ).

    f_db_table->set_parent_package( EXPORTING element_id     = id_db_table
                                              parent_package = 'Package1' ).

    f_access = new #( model = model ).

    f_access->add_access( EXPORTING used_attribute = id_dummy_attribute_db_table
                                    using_method   = id_method ).

    f_access->add_access( EXPORTING used_attribute = id_attribute
                                    using_method   = id_method ).


    DATA(id_method2) = f_method->add( EXPORTING class  = 'Class1'
                                               method = 'Method2' ).

    f_invocation = new #( model = model ).

    f_invocation->add_invocation( EXPORTING used_method  = id_method
                                            using_method = id_method2 ).

    model->make_mse( IMPORTING mse_model = DATA(mse_model) ).

    DATA mse_model_exp TYPE model->lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Package (id: 1 )| )
                             ( line = |  (name 'Package1')| )
                             ( line = |  (parentPackage (ref: 2)))| )
                             ( line = |(FAMIX.Package (id: 2 )| )
                             ( line = |  (name 'Package2'))| )
                             ( line = |(FAMIX.Module (id: 3 )| )
                             ( line = |  (name 'Program')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Class (id: 4 )| )
                             ( line = |  (name 'Class1')| )
                             ( line = |  (modifiers 'ClassModifier')| )
                             ( line = |  (parentPackage (ref: 1))| )
                             ( line = |  (isInterface true))| )
                             ( line = |(FAMIX.Class (id: 5 )| )
                             ( line = |  (name 'Class2')| )
                             ( line = |  (modifiers 'ClassModifier'))| )
                             ( line = |(FAMIX.Class (id: 6 )| )
                             ( line = |  (name 'LocalClass')| )
                             ( line = |  (modifiers 'modifier_local')| )
                             ( line = |  (container (ref: 3)))| )
                             ( line = |(FAMIX.Class (id: 7 )| )
                             ( line = |  (name 'LocalSubClass')| )
                             ( line = |  (modifiers 'modifier_local'))| )
                             ( line = |(FAMIX.Attribute (id: 8 )| )
                             ( line = |  (name 'Attribute1')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 9 )| )
                             ( line = |  (name 'Method1')| )
                             ( line = |  (signature 'Method1')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 10 )| )
                             ( line = |  (name 'MethodOfLocalClass')| )
                             ( line = |  (signature 'MethodOfLocalClass')| )
                             ( line = |  (parentType (ref: 6)))| )
                             ( line = |(FAMIX.Class (id: 11 )| )
                             ( line = |  (name 'ClassSub')| )
                             ( line = |  (modifiers 'ClassModifier'))| )
                             ( line = |(FAMIX.Class (id: 12 )| )
                             ( line = |  (name 'Interface')| )
                             ( line = |  (modifiers 'ClassModifier'))| )
                             ( line = |(FAMIX.Inheritance| )
                             ( line = |  (subclass (ref: 5))| )
                             ( line = |  (superclass (ref: 4)))| )
                             ( line = |(FAMIX.Inheritance| )
                             ( line = |  (subclass (ref: 12))| )
                             ( line = |  (superclass (ref: 4)))| )
                             ( line = |(FAMIX.Inheritance| )
                             ( line = |  (subclass (ref: 7))| )
                             ( line = |  (superclass (ref: 6)))| )
                             ( line = |(FAMIX.Class (id: 16 )| )
                             ( line = |  (name 'DBTable')| )
                             ( line = |  (modifiers 'DBTable')| )
                             ( line = |  (parentPackage (ref: 1)))| )
                             ( line = |(FAMIX.Attribute (id: 17 )| )
                             ( line = |  (name 'DBTable')| )
                             ( line = |  (parentType (ref: 16)))| )
                             ( line = |(FAMIX.Access| )
                             ( line = |  (accessor (ref: 9))| )
                             ( line = |  (variable (ref: 17)))| )
                             ( line = |(FAMIX.Access| )
                             ( line = |  (accessor (ref: 9))| )
                             ( line = |  (variable (ref: 8)))| )
                             ( line = |(FAMIX.Method (id: 20 )| )
                             ( line = |  (name 'Method2')| )
                             ( line = |  (signature 'Method2')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Invocation| )
                             ( line = |  (sender (ref: 20))| )
                             ( line = |  (candidates (ref: 9))| )
                             ( line = |  (signature 'DUMMY')))| )
                             ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).


  ENDMETHOD.

ENDCLASS.
