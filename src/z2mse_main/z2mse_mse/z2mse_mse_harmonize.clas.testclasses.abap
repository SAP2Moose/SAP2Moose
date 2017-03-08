CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_mse_harmonize DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      equalize_harmonized FOR TESTING RAISING cx_static_check,
      mse_2_harmonized FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD mse_2_harmonized.

    DATA: mse                          TYPE z2mse_mse_harmonize=>mse,
          equalized_harmonized_mse_act TYPE z2mse_mse_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_mse_harmonize=>harmonized_mse.

    mse = VALUE #( ( |( (FAMIX.Package (id: 1 )| )
                   ( |  (name 'Z2MSE_TEST_INITIAL_SELECTION')))| ) ).

    equalized_harmonized_mse_exp = VALUE #( ( |FAMIX.Package Z2MSE_TEST_INITIAL_SELECTION| ) ).

    equalized_harmonized_mse_act = z2mse_mse_harmonize=>mse_2_harmonized( mse = mse ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonize simple Package specification correctly' ).


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
