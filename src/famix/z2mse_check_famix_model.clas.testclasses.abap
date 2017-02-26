CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      number_of_parent_packages FOR TESTING RAISING cx_static_check,
      class_has_to_have_a_name FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD number_of_parent_packages.

    TEST-INJECTION parent_package.
      IF is_public_elements-element_id = 5.
        cl_abap_unit_assert=>fail( EXPORTING msg    = 'Class has a single package, do not report more than one' ).
      ENDIF.
    END-TEST-INJECTION.

    TEST-INJECTION parent_package2.
      IF is_public_elements-element_id = 5.
        cl_abap_unit_assert=>fail( EXPORTING msg    = 'Class has a single package, do not report none' ).
      ENDIF.
    END-TEST-INJECTION.

    TEST-INJECTION check_parent_package.
      IF is_public_elements-element_id = 3.
        IF count_parent_packages <> 2.
          cl_abap_unit_assert=>fail( EXPORTING msg    = 'there are two parent packages' ).
        ENDIF.
      ENDIF.
      IF is_public_elements-element_id = 4.
        IF count_parent_packages <> 0.
          cl_abap_unit_assert=>fail( EXPORTING msg    = 'there are no parent packages' ).
        ENDIF.
      ENDIF.
    END-TEST-INJECTION.

    DATA model TYPE REF TO z2mse_model.
    DATA f_package TYPE REF TO z2mse_famix_package.
    DATA f_class TYPE REF TO Z2MSE_famix_class.
    DATA f_cut TYPE REF TO z2mse_check_famix_model.
    model = NEW #( ).
    f_package = NEW #( model = model ).
    f_package->add(
      EXPORTING
        name_group             = 'GroupP'
        name                   = 'package1'
        modifiers              = 'pmodifier' ).
    f_package->add(
      EXPORTING
        name_group             = 'GroupP'
        name                   = 'package2'
        modifiers              = 'pmodifier' ).

    f_class = NEW #( model = model ).
    f_class->add( EXPORTING name_group             = 'Group1'
                            name                   = 'Class1'
                            modifiers              = 'modifier'
                  IMPORTING exists_already_with_id = DATA(exist_with_id)
                            id                     = DATA(id_class1) ).

    f_class->is_interface( EXPORTING element_id = id_class1 ).
    f_class->set_parent_package( EXPORTING element_id         = id_class1
                                         parent_package     = 'package1' ).
    f_class->set_parent_package( EXPORTING element_id         = id_class1
                                         parent_package     = 'package2' ).


    f_class->add( EXPORTING name_group             = 'Group1'
                            name                   = 'Class2'
                            modifiers              = 'modifier'
                  IMPORTING id                     = DATA(id_class2) ).

    f_class->add( EXPORTING name_group             = 'Group1'
                            name                   = 'Class3'
                            modifiers              = 'modifier'
                  IMPORTING id                     = DATA(id_class3) ).

    f_class->set_parent_package( EXPORTING element_id         = id_class3
                                         parent_package     = 'package1' ).

    f_cut = NEW #( ).
    f_cut->check( model = model ).

  ENDMETHOD.

  METHOD class_has_to_have_a_name.

    TEST-INJECTION parent_package.
    END-TEST-INJECTION.
    TEST-INJECTION parent_package2.
    END-TEST-INJECTION.

    TEST-INJECTION name1.
    END-TEST-INJECTION.

    TEST-INJECTION name2.
      cl_abap_unit_assert=>fail( EXPORTING msg    = 'Specifying a class with two attributes name should never be possible' ).
    END-TEST-INJECTION.

    TEST-INJECTION check_name.
      IF is_public_elements-element_id = 2.
        IF ls_public_attribute-string IS NOT INITIAL.
          cl_abap_unit_assert=>fail( EXPORTING msg    = 'name is initial' ).
        ENDIF.
      ENDIF.
    END-TEST-INJECTION.

    DATA model TYPE REF TO z2mse_model.
    DATA f_class TYPE REF TO Z2MSE_famix_class.
    DATA f_cut TYPE REF TO z2mse_check_famix_model.
    model = NEW #( ).

    f_class = NEW #( model = model ).
    f_class->add( EXPORTING name_group             = 'Group1'
                            name                   = 'Class1'
                            modifiers              = 'modifier'
                  IMPORTING exists_already_with_id = DATA(exist_with_id)
                            id                     = DATA(id_class1) ).

    f_class->add( EXPORTING name_group             = 'Group1'
                            name                   = ''
                            modifiers              = 'modifier'
                  IMPORTING id                     = DATA(id_class2) ).

    f_cut = NEW #( ).
    f_cut->check( model = model ).



  ENDMETHOD.

ENDCLASS.
