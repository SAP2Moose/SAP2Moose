CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr_where_used_sap DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO z2mse_extr_where_used_sap.
    METHODS:
      simple FOR TESTING RAISING cx_static_check,
      "! Add a new method for tests
      _add_new_method_for_test IMPORTING i_famix_method        TYPE REF TO z2mse_famix_method
                                         i_class_of_new_method TYPE string
                                         i_new_method          TYPE string,
      _add_new_attribute_for_test
        IMPORTING
          i_famix_attribute        TYPE REF TO z2mse_famix_attribute
          i_class_of_new_attribute TYPE string
          i_new_attribute          TYPE string.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD simple.

    DATA: wbcrossgt_test TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test.

    wbcrossgt_test = VALUE #( ( otype = 'ME' name = 'CLASS_A\ME:METHOD_A' include = 'INCLUDE_C' direct = 'X' indirect = '' )
                              ( otype = 'ME' name = 'CLASS_A\ME:METHOD_A' include = 'INCLUDE_A' direct = 'X' indirect = '' )
                              ( otype = 'DA' name = 'CLASS_A\DA:ATTRIBUTE_A' include = 'INCLUDE_A' direct = 'X' indirect = '' )
                              ( otype = 'EV' name = 'CLASS_A\EV:EVENT_A' include = 'INCLUDE_B' direct = 'X' indirect = '' )
                              ( otype = 'ME' name = 'INTF_A\ME:I_METHOD_A' include = 'INCLUDE_D' direct = 'X' indirect = '' ) ).

    DATA: includes_to_components TYPE z2mse_extr_where_used_sap=>ty_includes_to_components.

    includes_to_components =
      VALUE #( ( include = 'INCLUDE_A' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_B' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_B' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_C' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_C' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_D' is_class_component = abap_true clsname = 'CLASS_C' cmpname = 'INTF_A~I_METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    f_cut = NEW #( wbcrossgt_test = wbcrossgt_test
                   includes_to_components = includes_to_components ).

    DATA: components              TYPE z2mse_extr_classes=>ty_class_components,
          comps_used_by_comps_exp TYPE z2mse_extr_where_used_sap=>ty_comps_used_by_comps.


    components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                          ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type )
                          ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                          ( clsname = 'INTF_A' cmpname = 'I_METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                          ).

    comps_used_by_comps_exp =
    VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_A' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_B' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_A' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_C' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'INTF_A' cmpname = 'I_METHOD_A' cmptype = z2mse_extr_classes=>method_type
               used_by_clsname = 'CLASS_C' used_by_cmpname = 'INTF_A~I_METHOD_A' used_by_cmptype = z2mse_extr_classes=>method_type ) ).

    SORT comps_used_by_comps_exp.

    f_cut->used_by_class_component( EXPORTING class_components = components ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_comps_used_by_comps
        exp                  = comps_used_by_comps_exp
        msg                  = 'Expect correct using methods' ).


    DATA g_class_components_initial_exp TYPE z2mse_extr_classes=>ty_class_components_hashed.
    DATA: g_class_comp_where_used_act TYPE z2mse_extr_classes=>ty_class_components_hashed,
          g_class_comp_where_used_exp TYPE z2mse_extr_classes=>ty_class_components_hashed.

    g_class_components_initial_exp = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                          ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type )
                          ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                          ( clsname = 'INTF_A' cmpname = 'I_METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                          ).

    g_class_comp_where_used_exp =
        VALUE #( ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                 ( clsname = 'CLASS_B' cmpname = 'METHOD_B' cmptype = z2mse_extr_classes=>method_type )
                 ( clsname = 'CLASS_B' cmpname = 'METHOD_C' cmptype = z2mse_extr_classes=>method_type )
                 ( clsname = 'CLASS_C' cmpname = 'INTF_A~I_METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = f_cut->g_class_components_initial
        exp                  = g_class_components_initial_exp
        msg                  = 'Expect correct list of initial class components' ).

    g_class_comp_where_used_act = f_cut->get_components_where_used( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = g_class_comp_where_used_act
        exp                  = g_class_comp_where_used_exp
        msg                  = 'Expect correct list of class components found by where used analysis' ).



    g_class_comp_where_used_act = f_cut->get_components_where_used( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = g_class_comp_where_used_act
        exp                  = VALUE z2mse_extr_classes=>ty_class_components_hashed( )
        msg                  = 'Expect empty list of class components found by where used analysis if method is called a second time' ).

    DATA: model            TYPE REF TO z2mse_model,
          famix_method     TYPE REF TO z2mse_famix_method,
          famix_attribute  TYPE REF TO z2mse_famix_attribute,
          famix_invocation TYPE REF TO z2mse_famix_invocation,
          famix_access     TYPE REF TO z2mse_famix_access,
          famix_class      TYPE REF TO z2mse_famix_class,
          mse_model_act    TYPE z2mse_model=>lines_type,
          mse_model_exp    TYPE z2mse_model=>lines_type.

    model = NEW #( ).
    famix_method = NEW #( model = model ).
    famix_attribute = NEW #( model = model ).
    famix_invocation = NEW #( model = model ).
    famix_access = NEW #( model = model ).
    famix_class = NEW #( model = model ).

    famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                name       = 'CLASS_A' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'CLASS_A'
                              i_new_method          = 'METHOD_A' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'CLASS_A'
                              i_new_method          = 'EVENT_A' ).


    famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                name                   = 'CLASS_B' ).

    famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                name                   = 'CLASS_C' ).


    famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                name       = 'INTF_A' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'INTF_A'
                              i_new_method          = 'I_METHOD_A' ).



*    DATA: class_of_new_attribute TYPE string,
*          new_attribute          TYPE string.
*
*    class_of_new_attribute = 'CLASS_A'.
*    new_attribute = 'ATTRIBUTE_A'.

    _add_new_attribute_for_test( i_famix_attribute        = famix_attribute
                                 i_class_of_new_attribute = 'CLASS_A'
                                 i_new_attribute          = 'ATTRIBUTE_A' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'CLASS_B'
                              i_new_method          = 'METHOD_A' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'CLASS_B'
                              i_new_method          = 'METHOD_B' ).

    _add_new_method_for_test( i_famix_method        = famix_method
                              i_class_of_new_method = 'CLASS_B'
                              i_new_method          = 'METHOD_C' ).

*    _add_new_method_for_test( i_famix_method        = famix_method
*                              i_class_of_new_method = 'CLASS_C'
*                              i_new_method          = 'INTF_A~I_METHOD_A' ).

    f_cut->add_usage_to_model( EXPORTING famix_method     = famix_method
                                         famix_attribute  = famix_attribute
                                         famix_invocation = famix_invocation
                                         famix_access     = famix_access ).

    model->make_mse( IMPORTING mse_model = mse_model_act ).

    mse_model_exp = VALUE #( ( line = |( (FAMIX.Class (id: 1 )| ) " CLASS_A
                             ( line = |  (name 'CLASS_A'))| )
                             ( line = |(FAMIX.Method (id: 2 )| ) " CLASS_A>>METHOD_A
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (parentType (ref: 1)))| )
                             ( line = |(FAMIX.Method (id: 3 )| ) " CLASS_A>>EVENT_A
                             ( line = |  (name 'EVENT_A')| )
                             ( line = |  (parentType (ref: 1)))| )
                             ( line = |(FAMIX.Class (id: 4 )| ) " CLASS_B
                             ( line = |  (name 'CLASS_B'))| )
                             ( line = |(FAMIX.Class (id: 5 )| ) " CLASS_C
                             ( line = |  (name 'CLASS_C'))| )
                             ( line = |(FAMIX.Class (id: 6 )| ) " INTF_A
                             ( line = |  (name 'INTF_A'))| )
                             ( line = |(FAMIX.Method (id: 7 )| ) " INTF_A>>I_METHOD_A
                             ( line = |  (name 'I_METHOD_A')| )
                             ( line = |  (parentType (ref: 6)))| )
                             ( line = |(FAMIX.Attribute (id: 8 )| ) " CLASS_A-ATTRIBUTE_A
                             ( line = |  (name 'ATTRIBUTE_A')| )
                             ( line = |  (parentType (ref: 1)))| )
                             ( line = |(FAMIX.Method (id: 9 )| ) " CLASS_B>>METHOD_A
                             ( line = |  (name 'METHOD_A')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 10 )| ) " CLASS_B>>METHOD_B
                             ( line = |  (name 'METHOD_B')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Method (id: 11 )| ) " CLASS_B>>METHOD_C
                             ( line = |  (name 'METHOD_C')| )
                             ( line = |  (parentType (ref: 4)))| )
                             ( line = |(FAMIX.Access| )
                             ( line = |  (accessor (ref: 9))| ) " CLASS_B>>METHOD_A
                             ( line = |  (variable (ref: 8)))| ) " CLASS_A-ATTRIBUTE_A
                             ( line = |(FAMIX.Invocation| )
                             ( line = |  (sender (ref: 10))| ) " CLASS_B>>METHOD_B
                             ( line = |  (candidates (ref: 3))| ) " CLASS_A>>EVENT_A
                             ( line = |  (signature 'DUMMY'))| )
                             ( line = |(FAMIX.Invocation| )
                             ( line = |  (sender (ref: 9))| ) " CLASS_B>>METHOD_A
                             ( line = |  (candidates (ref: 2))| ) " CLASS_A>>METHOD_A
                             ( line = |  (signature 'DUMMY'))| )
                             ( line = |(FAMIX.Invocation| )
                             ( line = |  (sender (ref: 11))| ) " CLASS_B>>METHOD_C
                             ( line = |  (candidates (ref: 2))| ) " CLASS_A>>METHOD_A
                             ( line = |  (signature 'DUMMY'))| )
                             ( line = |(FAMIX.Method (id: 16 )| ) " CLASS_C>>INTF_A~I_METHOD_A
                             ( line = |  (name 'INTF_A~I_METHOD_A')| )
                             ( line = |  (signature 'INTF_A~I_METHOD_A')| )
                             ( line = |  (parentType (ref: 5))))| )
                             ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = mse_model_act
        exp                  = mse_model_exp
        msg                  = 'Wrong mse file for new class' ).

  ENDMETHOD.


  METHOD _add_new_method_for_test.

    " Add new method for test

    DATA: last_id          TYPE i.

    i_famix_method->add( EXPORTING name = i_new_method
                         IMPORTING id   = last_id ).

    i_famix_method->set_parent_type( EXPORTING element_id = last_id
                                             parent_element = 'FAMIX.Class'
                                             parent_name_group = 'ABAP_CLASS'
                                             parent_name    = i_class_of_new_method ).


    i_famix_method->store_id( EXPORTING class  = i_class_of_new_method
                                        method = i_new_method ).

  ENDMETHOD.


  METHOD _add_new_attribute_for_test.

    " Add new attribute for test

    DATA last_id TYPE i.

    i_famix_attribute->add( EXPORTING name = i_new_attribute IMPORTING id = last_id ).
    i_famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                  parent_element = 'FAMIX.Class'
                                                  parent_name_group = 'ABAP_CLASS'
                                                  parent_name    = i_class_of_new_attribute ).

    i_famix_attribute->store_id( EXPORTING class     = i_class_of_new_attribute
                                         attribute = i_new_attribute ).

  ENDMETHOD.

ENDCLASS.
