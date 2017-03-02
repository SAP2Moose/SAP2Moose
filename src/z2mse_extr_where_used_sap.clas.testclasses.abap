CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_extr_where_used_sap DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA f_cut TYPE REF TO z2mse_extr_where_used_sap.
    METHODS:
      simple FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD simple.

    DATA: wbcrossgt_test TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test.

    wbcrossgt_test = VALUE #( ( otype = 'ME' name = 'CLASS_A\ME:METHOD_A' include = 'INCLUDE_C' direct = 'X' indirect = '' )
                              ( otype = 'ME' name = 'CLASS_A\ME:METHOD_A' include = 'INCLUDE_D' direct = 'X' indirect = '' )
                              ( otype = 'DA' name = 'CLASS_A\DA:ATTRIBUTE_A' include = 'INCLUDE_A' direct = 'X' indirect = '' )
                              ( otype = 'EV' name = 'CLASS_A\EV:EVENT_A' include = 'INCLUDE_B' direct = 'X' indirect = '' ) ).

    DATA: includes_to_components TYPE z2mse_extr_where_used_sap=>ty_includes_to_components.

    includes_to_components =
      VALUE #( ( include = 'INCLUDE_A' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_B' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_B' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_C' is_class_component = abap_true clsname = 'CLASS_B' cmpname = 'METHOD_C' cmptype = z2mse_extr_classes=>method_type )
               ( include = 'INCLUDE_D' is_class_component = abap_true clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    f_cut = NEW #( wbcrossgt_test = wbcrossgt_test
                   includes_to_components = includes_to_components ).

    DATA: components              TYPE z2mse_extr_classes=>ty_class_components,
          comps_used_by_comps_exp TYPE z2mse_extr_where_used_sap=>ty_comps_used_by_comps.


    components = VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type )
                          ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type )
                          ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    comps_used_by_comps_exp =
    VALUE #( ( clsname = 'CLASS_A' cmpname = 'ATTRIBUTE_A' cmptype = z2mse_extr_classes=>attribute_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_A' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'EVENT_A' cmptype = z2mse_extr_classes=>event_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_B' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type
               used_by_clsname = 'CLASS_A' used_by_cmpname = 'METHOD_A' used_by_cmptype = z2mse_extr_classes=>method_type )
             ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type
               used_by_clsname = 'CLASS_B' used_by_cmpname = 'METHOD_C' used_by_cmptype = z2mse_extr_classes=>method_type ) ).

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
                          ( clsname = 'CLASS_A' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type ) ).

    g_class_comp_where_used_exp =
        VALUE #( ( clsname = 'CLASS_B' cmpname = 'METHOD_A' cmptype = z2mse_extr_classes=>method_type )
                 ( clsname = 'CLASS_B' cmpname = 'METHOD_B' cmptype = z2mse_extr_classes=>method_type )
                 ( clsname = 'CLASS_B' cmpname = 'METHOD_C' cmptype = z2mse_extr_classes=>method_type ) ).

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

  ENDMETHOD.

ENDCLASS.
