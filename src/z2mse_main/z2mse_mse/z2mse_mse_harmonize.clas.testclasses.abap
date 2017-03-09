"! Analyses an mse file
"! See https://hal.inria.fr/hal-00646884/fr/ 2.2.1 for the specification of the grammar. This class follows the names given there.
CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_mse_harmonize DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      equalize_harmonized FOR TESTING RAISING cx_static_check,
      mse_2_harmonized FOR TESTING RAISING cx_static_check,
      _make_list_of_element_nodes FOR TESTING RAISING cx_static_check,
      _add_element_node FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD _add_element_node.

    DATA: element_node      TYPE string,
          element_nodes_act TYPE z2mse_mse_harmonize=>string_table,
          element_nodes_exp TYPE z2mse_mse_harmonize=>string_table.

    element_node = | |.

    z2mse_mse_harmonize=>_add_element_node( CHANGING element_node  = element_node
                                                     element_nodes = element_nodes_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = element_nodes_act
                                                  exp = element_nodes_exp
                                                  msg = 'Do not add a single blank' ).

  ENDMETHOD.

  METHOD _make_list_of_element_nodes.

    DATA msestr TYPE string.
    DATA element_nodes_act TYPE z2mse_mse_harmonize=>string_table.
    DATA element_nodes_exp TYPE z2mse_mse_harmonize=>string_table.

    msestr = |( (e1) (e2 2) (e3 a ( ) ) )|.
    element_nodes_exp = VALUE #( ( |(e1)| )
                                 ( |(e2 2)| )
                                 ( |(e3 a ( ) )| ) ).

    element_nodes_act = z2mse_mse_harmonize=>_make_list_of_element_nodes( i_msestr = msestr ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = element_nodes_act
                                                  exp = element_nodes_exp
                                                  msg = 'Expect correct list of element nodes' ).

  ENDMETHOD.

  METHOD mse_2_harmonized.

    DATA: mse                          TYPE z2mse_mse_harmonize=>mse,
          equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    mse = VALUE #( ( |( (FAMIX.Package (id: 1 )| )
                   ( |  (name 'Z2MSE_TEST_INITIAL_SELECTION'))| )
                   ( |(FAMIX.Class (id: 2 )| )
                   ( |  (name 'Z2MSE_TEST_CL_A')| )
                   ( |  (modifiers 'ABAPGlobalClass')| )
                   ( |  (parentPackage (ref: 1)))| ) ).

    equalized_harmonized_mse_exp = VALUE #( ( |FAMIX.Package Z2MSE_TEST_INITIAL_SELECTION| ) ).

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse ).

*    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
*                                                  exp = equalized_harmonized_mse_exp
*                                                  msg = 'Harmonize simple Package specification correctly' ).


  ENDMETHOD.

  METHOD equalize_harmonized.
    DATA: harmonized_mse               TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    harmonized_mse = VALUE #( ( |B      B| ) "Many blanks between both B
                              ( |A| ) ).

    equalized_harmonized_mse_exp = VALUE #( ( |A| )
                                            ( |B B| ) ). "Only a single blank between both B

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>equalize_harmonized( harmonized_mse = harmonized_mse ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonized mse has to be strictly sorted alphabetically by method equalize_harmonized. Multiple blanks are removed' ).

  ENDMETHOD.

ENDCLASS.
