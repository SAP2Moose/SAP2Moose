CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      main_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD main_test.

    TEST-INJECTION check.
    END-TEST-INJECTION.


    DATA: f_cut         TYPE REF TO z2mse_make_demo_model,
          mse_model_exp TYPE z2mse_model=>lines_type.

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Namespace (id: 1 )| )
                             ( line = |  (name 'aNamespace'))| )
                             ( line = |(FAMIX.Package (id: 2 )| )
                             ( line = |  (name 'aPackage'))| )
                             ( line = |(FAMIX.Package (id: 3 )| )
                             ( line = |  (name 'bPackage'))| )
                             ( line = |(FAMIX.Package (id: 4 )| )
                             ( line = |  (name 'anotherPackage')| )
                             ( line = |  (parentPackage (ref: 2))| )
                             ( line = |  (parentPackage (ref: 3)))| )
                             ( line = |(FAMIX.Class (id: 5 )| )
                             ( line = |  (name ''))| )
                             ( line = |(FAMIX.Class (id: 6 )| )
                             ( line = |  (name 'ClassA')| )
                             ( line = |  (container (ref: 1))| )
                             ( line = |  (parentPackage (ref: 2)))| )
                             ( line = |(FAMIX.Method (id: 7 )| )
                             ( line = |  (name 'methodA1')| )
                             ( line = |  (signature 'methodA1()')| )
                             ( line = |  (parentType (ref: 6)))| )
                             ( line = |(FAMIX.Attribute (id: 8 )| )
                             ( line = |  (name 'attributeA1')| )
                             ( line = |  (parentType (ref: 6)))| )
                             ( line = |(FAMIX.Class (id: 9 )| )
                             ( line = |  (name 'ClassB')| )
                             ( line = |  (container (ref: 1))| )
                             ( line = |  (parentPackage (ref: 4)))| )
                             ( line = |(FAMIX.Inheritance| )
                             ( line = |  (subclass (ref: 9))| )
                             ( line = |  (superclass (ref: 6))))| ) ).

    f_cut = NEW #( ).

    f_cut->make( IMPORTING mse_model = DATA(mse_model) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model
        exp                  = mse_model_exp
        msg                  = 'Check Demo Model').

  ENDMETHOD.

ENDCLASS.
