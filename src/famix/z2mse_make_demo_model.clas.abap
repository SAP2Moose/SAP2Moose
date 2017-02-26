

CLASS z2mse_make_demo_model DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS make
      EXPORTING
        mse_model TYPE z2mse_model=>lines_type.
ENDCLASS.

CLASS z2mse_make_demo_model IMPLEMENTATION.

  METHOD make.
    DATA model            TYPE REF TO z2mse_model.
    CREATE OBJECT model.

    DATA famix_namespace  TYPE REF TO z2mse_famix_namespace.
    CREATE OBJECT famix_namespace EXPORTING model = model.
    DATA famix_package      TYPE REF TO z2mse_famix_package.
    CREATE OBJECT famix_package EXPORTING model = model.
    DATA famix_class        TYPE REF TO Z2MSE_famix_class.
    CREATE OBJECT famix_class EXPORTING model = model.
    DATA famix_method         TYPE REF TO z2mse_famix_method.
    CREATE OBJECT famix_method EXPORTING model = model.
    DATA famix_attribute    TYPE REF TO Z2MSE_famix_attribute.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    DATA famix_inheritance  TYPE REF TO Z2MSE_famix_inheritance.
    CREATE OBJECT famix_inheritance EXPORTING model = model.
    DATA check_famix_model TYPE REF TO Z2MSE_check_famix_model.
    CREATE OBJECT check_famix_model.

    DATA last_id TYPE i.

    famix_namespace->add( name = 'aNamespace' ).
    famix_package->add( name = 'aPackage' ).
    " Add error to test issue22
    famix_package->add( name = 'bPackage' ).
    " End of adding error to test issue 22
    famix_package->add( EXPORTING name = 'anotherPackage' IMPORTING id = last_id ).
    famix_package->set_parent_package( element_id = last_id parent_package = 'aPackage' ).
    " Test SAP_2_FAMIX_52       Do not add attributes twice if they are added with identical attributes
    " Add the same attribute twice
    famix_package->set_parent_package( element_id = last_id parent_package = 'aPackage' ).
    " Add error to test issue22
    famix_package->set_parent_package( element_id = last_id parent_package = 'bPackage' ).
    " End of adding error to test issue 22
    " Add error to test issue24
    famix_class->add( name = '' ).
    " End of adding error to test issue 24
    famix_class->add( EXPORTING name = 'ClassA' IMPORTING id = last_id ).
    famix_class->set_container( EXPORTING element_id = last_id
                                          container_element = 'FAMIX.Namespace'
                                          parent_container  = 'aNamespace').
    famix_class->set_parent_package( element_id = last_id
                                     parent_package = 'aPackage' ).

    famix_method->add( EXPORTING name = 'methodA1' IMPORTING id = last_id ).
    famix_method->set_signature( element_id = last_id signature = 'methodA1()' ).
    famix_method->set_parent_type( element_id = last_id
                                   parent_element = 'FAMIX.Class'
                                   parent_name    = 'ClassA' ).
    famix_attribute->add( EXPORTING name = 'attributeA1' IMPORTING id = last_id ).
    famix_attribute->set_parent_type( element_id = last_id
                                      parent_element = 'FAMIX.Class'
                                      parent_name    = 'ClassA' ).
    famix_class->add( EXPORTING name = 'ClassB' IMPORTING id = last_id ).
    famix_class->set_container( element_id = last_id
                                container_element = 'FAMIX.Namespace'
                                parent_container  = 'aNamespace' ).
    famix_class->set_parent_package( element_id = last_id
                                     parent_package = 'anotherPackage' ).

    last_id = famix_inheritance->add( ).
    famix_inheritance->set_sub_and_super_class( EXPORTING element_id = last_id
                                                          subclass_element   = 'FAMIX.Class'
                                                          subclass_name_group = ''
                                                          subclass_name      = 'ClassB'
                                                          superclass_element = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name    = 'ClassA' ).
    TEST-SEAM check.
      check_famix_model->check( model = model ).
    END-TEST-SEAM.

    model->make_mse( IMPORTING mse_model = mse_model ).
  ENDMETHOD.

ENDCLASS.
