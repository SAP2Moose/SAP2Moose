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
                          file_name  = |adt://NPL/main#start)1,1|
                IMPORTING id         = DATA(id_element_act) ).

*    testmodel->add_entity( EXPORTING elementname               = 'FAMIX.Type1'
*                                 name_group                = 'Group1'
*                                 is_named_entity           = abap_true
*                                 can_be_referenced_by_name = abap_true
*                                 name                      = 'Name1'
*                       IMPORTING exists_already_with_id    = DATA(id)
*                                 processed_id              = DATA(processed_id) ).
*
*    testmodel->add_entity( EXPORTING elementname               = 'FAMIX.Type1'
*                                 name_group                = 'Group1'
*                                 is_named_entity           = abap_true
*                                 can_be_referenced_by_name = abap_true
*                                 name                      = 'Name2'
*                       IMPORTING exists_already_with_id    = id
*                                 processed_id              = processed_id ).
*
*    testmodel->add_reference_by_name( EXPORTING element_id              = 1
**                                           element_type            =
**                                           element_name_group      =
**                                           element_name            =
*                                            attribute_name          = 'attribute1'
*                                            type_of_reference       = 'FAMIX.Type1'
*                                            name_group_of_reference = 'Group1'
*                                            name_of_reference       = 'Name2' ).
*
*    testmodel->add_boolean( EXPORTING element_id         = 0
*                                  element_type       = 'FAMIX.Type1'
*                                  element_name_group = 'Group1'
*                                  element_name       = 'Name1'
*                                  attribute_name     = 'attr'
*                                  is_true            = abap_true ).
*
*    testmodel->add_boolean( EXPORTING element_id         = 0
*                                  element_type       = 'FAMIX.Type1'
*                                  element_name_group = 'Group1'
*                                  element_name       = 'Name2'
*                                  attribute_name     = 'attr'
*                                  is_true            = abap_false ).
*
*    testmodel->add_entity( EXPORTING elementname               = 'FAMIX.Type2'
*                                 name_group                = 'Group2'
*                                 is_named_entity           = abap_false
*                                 is_id_required            = abap_true
*                                 can_be_referenced_by_name = abap_false
*                       IMPORTING exists_already_with_id    = id
*                                 processed_id              = processed_id ).
*
*    testmodel->add_reference_by_id( EXPORTING  element_id         = 3
*                                           attribute_name     = 'attribute2'
*                                           reference_id       = 2 ).
*
*    testmodel->add_string( EXPORTING element_id         = 3
*                                 attribute_name     = 'attribute3'
*                                 string             = 'A String' ).

    DATA(model) = testmodel->get_model( ).

    DATA model_exp TYPE testmodel->public_elements_type.

    model_exp = VALUE #( ( element_id = 1 element_type = 'FAMIX.FileAnchor' is_id_required = abap_true
                           public_attributes = VALUE #( ( attribute_id = 1 attribute_type = 'element' reference = 1  )
                                                        ( attribute_id = 2 attribute_type = 'fileName' string = |adt://NPL/main#start)1,1| ) ) )
).
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = model
        exp                  = model_exp
        msg                  = 'Wrong model' ).
  ENDMETHOD.

ENDCLASS.
