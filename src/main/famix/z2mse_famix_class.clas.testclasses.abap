CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_famix_class DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_class_end_to_end FOR TESTING RAISING cx_static_check,
      test_methods_attr_end_to_end FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test_class_end_to_end.
    DATA model TYPE REF TO z2mse_model.
    DATA f_custom_source_language TYPE REF TO Z2MSE_famix_custom_source_lng.
    DATA f_package TYPE REF TO z2mse_famix_package.
    DATA f_module TYPE REF TO z2mse_famix_module.
    DATA f_inheritance TYPE REF TO Z2MSE_famix_inheritance.
    DATA f_cut TYPE REF TO z2mse_famix_class.
    model = NEW #( ).
    f_custom_source_language = NEW #( model = model ).
    f_custom_source_language->add( name = 'ABAP' name_group = z2mse_extr3=>ng_source_language ).
    f_package = NEW #( model = model ).
    f_package->add(
      EXPORTING
        name_group             = z2mse_extr3=>ng_abap_package
        name                   = 'package1'
        modifiers              = 'pmodifier' ).
    f_module = NEW #( model = model ).
    f_module->add( EXPORTING name_group             = 'ModulNames'
                             name                   = 'AProgram'
                             modifiers              = 'AProgramModifier'
                   IMPORTING id = DATA(id_module) ).
    f_cut = NEW #( model = model ).
    f_cut->add( EXPORTING name_group             = 'Group1'
                          name                   = 'Class1'
                          modifiers              = 'modifier'
                IMPORTING exists_already_with_id = DATA(exist_with_id)
                          id                     = DATA(id_class1) ).

    f_cut->add( EXPORTING name_group             = 'Group1'
                          name                   = 'Class2'
                          modifiers              = 'modifier'
                IMPORTING id                     = DATA(id_class2) ).

    f_cut->is_interface( EXPORTING element_id = id_class1 ).
    f_cut->set_parent_package( EXPORTING element_id         = id_class1
                                         parent_package     = 'package1'
                                         parent_package_name_group = z2mse_extr3=>ng_abap_package ).
    f_cut->set_container( EXPORTING element_id         = id_class1
                                    container_element  = 'FAMIX.Module'
                                    parent_container   = 'AProgram'
                                    parent_container_name_group = 'ModulNames' ).
    f_cut->set_container_by_id( EXPORTING element_id          = id_class2
                                          container_element   = 'FAMIX.Module'
                                          parent_container_id = id_module ).

    f_inheritance = NEW #( model = model ).
    DATA(id_inheritance) = f_inheritance->add( ).
    f_inheritance->set_sub_and_super_class( EXPORTING element_id            = id_inheritance
                                                      subclass_element      = 'FAMIX.Class'
                                                      subclass_name_group   = 'Group1'
                                                      subclass_name         = 'Class2'
                                                      superclass_element    = 'FAMIX.Class'
                                                      superclass_name_group = 'Group1'
                                                      superclass_name       = 'Class1' ).

    model->make_mse( IMPORTING mse_model = DATA(mse_model) ).

    DATA mse_model_exp TYPE model->lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.CustomSourceLanguage (id: 1 )| )
                             ( line = |  (name 'ABAP'))| )
                             ( line = |(FAMIX.Package (id: 2 )| )
                             ( line = |  (name 'package1'))| )
                             ( line = |(FAMIX.Module (id: 3 )| )
                             ( line = |  (name 'AProgram'))| )
                             ( line = |(FAMIX.Class (id: 4 )| )
                             ( line = |  (name 'Class1')| )
                             ( line = |  (modifiers 'modifier')| )
                             ( line = |  (isInterface true)| )
                             ( line = |  (parentPackage (ref: 2))| )
                             ( line = |  (container (ref: 3)))| )
                             ( line = |(FAMIX.Class (id: 5 )| )
                             ( line = |  (name 'Class2')| )
                             ( line = |  (modifiers 'modifier')| )
                             ( line = |  (container (ref: 3)))| )
                             ( line = |(FAMIX.Inheritance| )
                             ( line = |  (subclass (ref: 5))| )
                             ( line = |  (superclass (ref: 4))))| ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

  METHOD test_methods_attr_end_to_end.
    DATA model TYPE REF TO z2mse_model.
    DATA f_cut TYPE REF TO z2mse_famix_class.
    DATA f_attribute TYPE REF TO Z2MSE_famix_attribute.
    DATA f_method TYPE REF TO z2mse_famix_method.
    DATA f_access TYPE REF TO Z2MSE_famix_access.
    DATA f_invocation TYPE REF TO z2mse_famix_invocation.
    model = NEW #( ).

    f_cut = NEW #( model = model ).
    f_cut->add( EXPORTING name_group             = 'Group1'
                          name                   = 'Class1'
                          modifiers              = 'modifier'
                IMPORTING exists_already_with_id = DATA(exist_with_id)
                          id                     = DATA(id_class1) ).

    f_cut->add( EXPORTING name_group             = 'Group1'
                          name                   = 'Class2'
                          modifiers              = 'modifier'
                IMPORTING id                     = DATA(id_class2) ).

    f_attribute = NEW #( model = model ).

    DATA(id_attribute_get) = f_attribute->get_id( EXPORTING name_group = z2mse_extr3=>ng_abap_class
                                                            class     = 'Class1'
                                                            attribute = 'ClassAttribute1' ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = id_attribute_get
       exp                  = 0
       msg                  = 'Wrong ID of not existing attribute read' ).

    f_attribute->add( EXPORTING name_group             = 'Group2'
                                name                   = 'ClassAttribute1'
                                modifiers              = 'AttrModifier'
                      IMPORTING id                     = DATA(id_attribute) ).

    f_attribute->set_parent_type( EXPORTING element_id        = id_attribute
                                            parent_element    = 'FAMIX.Class'
                                            parent_name_group = 'Group1'
                                            parent_name       = 'Class1' ).

    f_attribute->store_id( EXPORTING name_group = z2mse_extr3=>ng_abap_class
                                     class     = 'Class1'
                                     attribute = 'ClassAttribute1' ).

    id_attribute_get = f_attribute->get_id( EXPORTING name_group = z2mse_extr3=>ng_abap_class
                                                      class     = 'Class1'
                                                      attribute = 'ClassAttribute1' ).


    f_attribute->add( EXPORTING name_group             = 'Group2'
                                name                   = 'ClassAttribute2'
                                modifiers              = 'AttrModifier'
                      IMPORTING id                     = DATA(id_attribute2) ).

    f_attribute->set_parent_type( EXPORTING element_id = id_attribute2
                                            parent_id  = id_class2  ).

    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = id_attribute_get
       exp                  = id_attribute
       msg                  = 'Wrong ID of existing attribute read' ).

    f_method = NEW #( model = model ).

    f_method->add( EXPORTING name_group             = 'Group3'
                             name                   = 'method1'
                             modifiers              = 'methodModfier'
                   IMPORTING id                     = DATA(id_method1) ).

    f_method->set_signature( element_id = id_method1
                             signature  = 'method1' ).

    f_method->set_parent_type( EXPORTING element_id        = id_method1
                                         parent_element    = 'FAMIX.Class'
                                         parent_name_group = 'Group1'
                                         parent_name       = 'Class1' ).

    DATA(id_method1_read) = f_method->get_id( EXPORTING class_name_group  = 'Group1'
                                                        class             = 'Class1'
                                                        method_name_group = 'Group3'
                                                        method            = 'method1' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_method1_read
                                                  exp = 0
                                                  msg = 'ID of not yet stored method has to be zero' ).

    f_method->store_id( EXPORTING class_name_group  = 'Group1'
                                  class             = 'Class1'
                                  method_name_group = 'Group3'
                                  method            = 'method1' ).

    id_method1_read = f_method->get_id( EXPORTING class_name_group  = 'Group1'
                                                        class             = 'Class1'
                                                        method_name_group = 'Group3'
                                                        method            = 'method1' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = id_method1_read
                                                  exp = id_method1
                                                  msg = 'Received name id of method not as expected' ).

    f_method->add( EXPORTING name_group             = 'Group3'
                             name                   = 'method2'
                             modifiers              = 'methodModfier'
                   IMPORTING id                     = DATA(id_method2) ).

    f_method->set_parent_type( EXPORTING element_id         = id_method2
                                         parent_element     = 'FAMIX.Class'
                                         parent_id          = id_class2 ).

    f_access = NEW #( model = model ).

    DATA(attribute1_used_by_method1_new) = f_access->is_new_access( EXPORTING accessor_id = id_method1
                                                                          variable_id = id_attribute2 ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = attribute1_used_by_method1_new
                                                  exp = abap_true
                                                  msg = 'New access is not used' ).

    DATA(id_access) = f_access->add( ).

    f_access->set_accessor_variable_relation( EXPORTING element_id         = id_access
                                                        accessor_id        = id_method1
                                                        variable_id        = id_attribute2 ).

    attribute1_used_by_method1_new = f_access->is_new_access( EXPORTING accessor_id = id_method1
                                                                    variable_id = id_attribute2 ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = attribute1_used_by_method1_new
                                                  exp = abap_false
                                                  msg = 'Access is existing' ).

    f_invocation = NEW #( model = model ).

    DATA(invocation_is_new) = f_invocation->is_new_invocation_to_candidate( sender_id     = id_method1
                                                                            candidates_id = id_method2 ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = invocation_is_new
                                                  exp = abap_true
                                                  msg = 'Invocation is new' ).

    DATA(id_invocation) = f_invocation->add( ).

    f_invocation->set_invocation_by_reference( EXPORTING element_id           = id_invocation
                                                         sender_id            = id_method1
                                                         candidates_id        = id_method2
                                                         receiver_id          = id_method2
                                                         signature            = 'Dummy'
                                                         receiver_source_code = 'ReceiverSourceCode' ).

    model->make_mse( IMPORTING mse_model = DATA(mse_model) ).

    DATA mse_model_exp TYPE model->lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Class (id: 1 )| )
                             ( line = |  (name 'Class1')| )
                             ( line = |  (modifiers 'modifier'))| )
                             ( line = |(FAMIX.Class (id: 2 )| )
                             ( line = |  (name 'Class2')| )
                             ( line = |  (modifiers 'modifier'))| )
                             ( line = |(FAMIX.Attribute (id: 3 )| )
                             ( line = |  (name 'ClassAttribute1')| )
                             ( line = |  (parentType (ref: 1)))| )
                             ( line = |(FAMIX.Attribute (id: 4 )| )
                             ( line = |  (name 'ClassAttribute2')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Method (id: 5 )| )
                             ( line = |  (name 'method1')| )
                             ( line = |  (signature 'method1')| )
                             ( line = |  (parentType (ref: 1)))| )
                             ( line = |(FAMIX.Method (id: 6 )| )
                             ( line = |  (name 'method2')| )
                             ( line = |  (parentType (ref: 2)))| )
                             ( line = |(FAMIX.Access| )
                             ( line = |  (accessor (ref: 5))| )
                             ( line = |  (variable (ref: 4)))| )
                             ( line = |(FAMIX.Invocation| )
                             ( line = |  (sender (ref: 5))| )
                             ( line = |  (candidates (ref: 6))| )
                             ( line = |  (receiver (ref: 6))| )
                             ( line = |  (signature 'Dummy')| )
                             ( line = |  (receiverSourceCode 'ReceiverSourceCode')))| ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.

ENDCLASS.
