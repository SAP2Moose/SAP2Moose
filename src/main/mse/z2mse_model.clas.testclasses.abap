CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_model DEFINITION LOCAL FRIENDS  ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS: add_entity_referencable_named FOR TESTING RAISING cx_static_check.
    METHODS: add_entity_named FOR TESTING RAISING cx_static_check.
    METHODS: add_entity_not_named FOR TESTING RAISING cx_static_check,
      add_boolean FOR TESTING RAISING cx_static_check,
      add_boolean_by_name FOR TESTING RAISING cx_static_check,
      add_reference_by_id FOR TESTING RAISING cx_static_check,
      add_reference_by_name FOR TESTING RAISING cx_static_check,
      make_mse FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD add_entity_referencable_named.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    DATA g_named_entities_exp TYPE HASHED TABLE OF f_cut->named_entity_type WITH UNIQUE KEY element_type element_name_group element_name.

    g_named_entities_exp = VALUE #( ( element_type        = 'Type1'
                                       element_name_group  = 'Group1'
                                       element_name        = 'Name1'
                                       element_id          = 1 ) ).

    DATA g_elements_in_model_exp TYPE HASHED TABLE OF f_cut->element_in_model_type WITH UNIQUE KEY element_id.

    g_elements_in_model_exp = VALUE #( ( element_id      = 1
                                     is_named_entity = abap_true
                                      element_type   = 'Type1' ) ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 0
        msg                  = 'Not existing element has ID 0' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 1
        msg                  = 'First element has ID 1' ).

    " Add a second time with identical settings

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Second time identical value: Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Second time identical value: Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Second time identical value: Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 1
        msg                  = 'Second time identical value: Existing element has ID 1' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 1
        msg                  = 'Second time identical value: First element has ID 1' ).

  ENDMETHOD.

  METHOD add_entity_named.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    DATA g_named_entities_exp TYPE HASHED TABLE OF f_cut->named_entity_type WITH UNIQUE KEY element_type element_name_group element_name.

*    g_named_entities_exp = value #( ( element_type        = 'Type1'
*                                       element_name_group  = 'Group1'
*                                       element_name        = 'Name1'
*                                       element_id          = 1 ) ).

    DATA g_elements_in_model_exp TYPE HASHED TABLE OF f_cut->element_in_model_type WITH UNIQUE KEY element_id.

    g_elements_in_model_exp = VALUE #( ( element_id      = 1
                                     is_named_entity = abap_true
                                      element_type   = 'Type1' ) ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 0
        msg                  = 'Not existing element has ID 0' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 1
        msg                  = 'First element has ID 1' ).

    " Add a second time with identical settings

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).

    g_elements_in_model_exp = VALUE #( ( element_id      = 1
                                     is_named_entity = abap_true
                                      element_type   = 'Type1' )
                                      ( element_id      = 2
                                     is_named_entity = abap_true
                                      element_type   = 'Type1' ) ).

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 2
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )   ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Second time identical value: Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Second time identical value: Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Second time identical value: Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 0
        msg                  = 'Second time identical value: No check for existence possible, so: 0' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 2
        msg                  = 'Second time identical value: Second element has ID 2' ).

  ENDMETHOD.

  METHOD add_entity_not_named.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_false
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    DATA g_named_entities_exp TYPE HASHED TABLE OF f_cut->named_entity_type WITH UNIQUE KEY element_type element_name_group element_name.

    g_named_entities_exp = VALUE #( ).

    DATA g_elements_in_model_exp TYPE HASHED TABLE OF f_cut->element_in_model_type WITH UNIQUE KEY element_id.

    g_elements_in_model_exp = VALUE #( ( element_id      = 1
                                     is_named_entity = abap_false
                                      element_type   = 'Type1' ) ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 0
        msg                  = 'Not existing element has ID 0' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 1
        msg                  = 'First element has ID 1' ).

    " Add a second time with identical settings

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_false
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).


    g_elements_in_model_exp = VALUE #( ( element_id      = 1
                                         is_named_entity = abap_false
                                         element_type   = 'Type1' )
                                       ( element_id      = 2
                                         is_named_entity = abap_false
                                         element_type   = 'Type1' ) ).


    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_named_entities
        exp                  = g_named_entities_exp
        msg                  = 'Second time identical value: Wrong named entities' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_elements_in_model
        exp                  = g_elements_in_model_exp
        msg                  = 'Second time identical value: Wrong named elements in model' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Second time identical value: Wrong attributes table line' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = id
        exp                  = 0
        msg                  = 'Second time identical value: Not existing element has ID 0' ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = processed_id
        exp                  = 2
        msg                  = 'Second time identical value: Second element has ID 2' ).

  ENDMETHOD.

  METHOD add_boolean.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    f_cut->add_boolean( EXPORTING element_id         = 1
*                                 element_type       =
*                                  element_name_group = 'DUMMY'
*                                 element_name       =
                                  attribute_name     = 'attr'
                                  is_true            = abap_true ).


    f_cut->add_boolean( EXPORTING element_id         = 1
*                                 element_type       =
*                                  element_name_group = 'DUMMY'
*                                 element_name       =
                                  attribute_name     = 'attr'
                                  is_true            = abap_false ).

    f_cut->add_boolean( EXPORTING element_id         = 1
*                                 element_type       =
*                                  element_name_group = 'DUMMY'
*                                 element_name       =
                                  attribute_name     = 'attr'
                                  is_true            = abap_true ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 1
                                  attribute_id   = 2
                                  attribute_type = 'attr'
                                  value_type     = 'B'
                                  name           = ''
                                  reference      = 0
                                  boolean        = abap_true )
                                ( element_id     = 1
                                  attribute_id   = 3
                                  attribute_type = 'attr'
                                  value_type     = 'B'
                                  name           = ''
                                  reference      = 0
                                  boolean        = abap_false )   ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

  ENDMETHOD.

  METHOD add_boolean_by_name.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    f_cut->add_boolean( EXPORTING element_id         = 0
                                  element_type       = 'Type1'
                                  element_name_group = 'Group1'
                                  element_name       = 'Name1'
                                  attribute_name     = 'attr'
                                  is_true            = abap_true ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 1
                                  attribute_id   = 2
                                  attribute_type = 'attr'
                                  value_type     = 'B'
                                  name           = ''
                                  reference      = 0
                                  boolean        = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

  ENDMETHOD.

  METHOD add_reference_by_id.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).
    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_false
                                 name                      = 'Name2'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).
    f_cut->add_reference_by_id( EXPORTING element_id         = 1
*                                     element_type       =
*                                          element_name_group = 'DUMMY'
*                                     element_name       =
                                          attribute_name     = 'attribute1'
                                          reference_id       = 2 ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 2
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name2'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 1
                                  attribute_id   = 2
                                  attribute_type = 'attribute1'
                                  value_type     = 'R'
                                  name           = ''
                                  reference      = 2
                                  boolean        = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

  ENDMETHOD.

  METHOD add_reference_by_name.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).
    f_cut->add_entity( EXPORTING elementname               = 'Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name2'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).
    f_cut->add_reference_by_name( EXPORTING element_id              = 1
*                                           element_type            =
*                                            element_name_group      =
*                                           element_name            =
                                            attribute_name          = 'attribute1'
                                            type_of_reference       = 'Type1'
                                            name_group_of_reference = 'Group1'
                                            name_of_reference       = 'Name2' ).

*    "! A table with all the attributes of an entity
    DATA g_attributes_exp TYPE SORTED TABLE OF f_cut->attribute_type WITH UNIQUE KEY element_id attribute_id.

    g_attributes_exp = VALUE #( ( element_id     = 1
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name1'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 2
                                  attribute_id   = 1
                                  attribute_type = 'name'
                                  value_type     = 'S'
                                  name           = 'Name2'
                                  reference      = 0
                                  boolean        = abap_false )
                                ( element_id     = 1
                                  attribute_id   = 2
                                  attribute_type = 'attribute1'
                                  value_type     = 'R'
                                  name           = ''
                                  reference      = 2
                                  boolean        = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_attributes
        exp                  = g_attributes_exp
        msg                  = 'Wrong attributes table line' ).

  ENDMETHOD.

  METHOD make_mse.

    DATA: f_cut TYPE REF TO z2mse_model.

    f_cut = NEW #( ).

    f_cut->add_entity( EXPORTING elementname               = 'FAMIX.Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name1'
                       IMPORTING exists_already_with_id    = DATA(id)
                                 processed_id              = DATA(processed_id) ).

    f_cut->add_entity( EXPORTING elementname               = 'FAMIX.Type1'
                                 name_group                = 'Group1'
                                 is_named_entity           = abap_true
                                 can_be_referenced_by_name = abap_true
                                 name                      = 'Name2'
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).

    f_cut->add_reference_by_name( EXPORTING element_id              = 1
*                                           element_type            =
*                                           element_name_group      =
*                                           element_name            =
                                            attribute_name          = 'attribute1'
                                            type_of_reference       = 'FAMIX.Type1'
                                            name_group_of_reference = 'Group1'
                                            name_of_reference       = 'Name2' ).

    f_cut->add_boolean( EXPORTING element_id         = 0
                                  element_type       = 'FAMIX.Type1'
                                  element_name_group = 'Group1'
                                  element_name       = 'Name1'
                                  attribute_name     = 'attr'
                                  is_true            = abap_true ).

    f_cut->add_boolean( EXPORTING element_id         = 0
                                  element_type       = 'FAMIX.Type1'
                                  element_name_group = 'Group1'
                                  element_name       = 'Name2'
                                  attribute_name     = 'attr'
                                  is_true            = abap_false ).

    f_cut->add_entity( EXPORTING elementname               = 'FAMIX.Type2'
                                 name_group                = 'Group2'
                                 is_named_entity           = abap_false
                                 is_id_required            = abap_true
                                 can_be_referenced_by_name = abap_false
                       IMPORTING exists_already_with_id    = id
                                 processed_id              = processed_id ).

    f_cut->add_reference_by_id( EXPORTING  element_id         = 3
                                           attribute_name     = 'attribute2'
                                           reference_id       = 2 ).

    f_cut->add_string( EXPORTING element_id         = 3
                                 attribute_name     = 'attribute3'
                                 string             = 'A String' ).

    data(model) = f_cut->get_model( ).

    data model_exp TYPE f_cut->public_elements_type.

    model_exp = value #( ( element_id = 1 element_type = 'FAMIX.Type1' is_named_entity = abap_true
                           public_attributes = value #( ( attribute_id = 1 attribute_type = 'name' string = 'Name1' )
                                                        ( attribute_id = 2 attribute_type = 'attribute1' reference = 2  )
                                                        ( attribute_id = 3 attribute_type = 'attr' boolean = abap_true ) ) )
                         ( element_id = 2 element_type = 'FAMIX.Type1' is_named_entity = abap_true
                           public_attributes = value #( ( attribute_id = 1 attribute_type = 'name' string = 'Name2' )
                                                        ( attribute_id = 4 attribute_type = 'attr' ) ) )
                         ( element_id = 3 element_type = 'FAMIX.Type2' is_named_entity = abap_false is_id_required = abap_true
                           public_attributes = value #( "( attribute_id = 1 attribute_type = 'name' string = 'Name1' )
                                                        ( attribute_id = 1 attribute_type = 'attribute2' reference = 2  )
                                                        ( attribute_id = 2 attribute_type = 'attribute3' string = |A String| ) ) )
                                                        ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = model
        exp                  = model_exp
        msg                  = 'Wrong model' ).

    f_cut->make_mse( IMPORTING mse_model = DATA(lt_mse_model) ).

    DATA lt_mse_mode_exp TYPE f_cut->lines_type.

    lt_mse_mode_exp = VALUE #( ( line = |( (FAMIX.Type1 (id: 1 )| )
                               ( line = |  (name 'Name1')| )
                               ( line = |  (attribute1 (ref: 2))| )
                               ( line = |  (attr true))| )
                               ( line = |(FAMIX.Type1 (id: 2 )| )
                               ( line = |  (name 'Name2')| )
                               ( line = |  (attr false))| )
                               ( line = |(FAMIX.Type2 (id: 3 )| )
                               ( line = |  (attribute2 (ref: 2))| )
                               ( line = |  (attribute3 'A String')))| ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lt_mse_model
        exp                  = lt_mse_mode_exp
        msg                  = 'Wrong mse model' ).


  ENDMETHOD.

ENDCLASS.
