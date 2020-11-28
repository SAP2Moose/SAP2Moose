CLASS ltcl_ DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_ IMPLEMENTATION.

  METHOD first_test.

    DATA: f_cut TYPE REF TO z2mse_famix_file_anchor.

    DATA: testmodel TYPE REF TO z2mse_model.

    testmodel = NEW #( ).

    f_cut = NEW #( model = testmodel ).

    f_cut->add( EXPORTING element_id = 1
                          file_name  = |adt://{ sy-sysid }/main#start)1,1|
                IMPORTING id         = DATA(id_element_act) ).

    DATA(model) = testmodel->get_model( ).

    DATA model_exp TYPE testmodel->public_elements_type.

    model_exp = VALUE #( ( element_id = 1 element_type = 'FAMIX.FileAnchor' is_id_required = abap_true
                           public_attributes = VALUE #( ( attribute_id = 1 attribute_type = 'element' reference = 1  )
                                                        ( attribute_id = 2 attribute_type = 'fileName' string = |adt://{ sy-sysid }/main#start)1,1| ) ) ) ).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = model
        exp                  = model_exp
        msg                  = 'Wrong model' ).
  ENDMETHOD.

ENDCLASS.
