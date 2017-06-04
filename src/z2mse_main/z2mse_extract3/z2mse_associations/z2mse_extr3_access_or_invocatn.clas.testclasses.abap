CLASS ltcl_test DEFINITION DEFERRED.
CLASS z2mse_extr3_access_or_invocatn DEFINITION LOCAL FRIENDS ltcl_test.
CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: f_cut TYPE REF TO z2mse_extr3_access_or_invocatn.
    METHODS:
      _get_famix_id_used_and_using FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD _get_famix_id_used_and_using.

    DATA(model_builder) = NEW z2mse_extr3_model_builder( ).

    DATA(element_manager) = NEW z2mse_extr3_element_manager( i_model_builder = model_builder
                                                             i_exclude_found_sap_intf = abap_true ).

    f_cut = NEW #( i_element_manager = element_manager ).

    DATA association TYPE z2mse_extr3_element_manager=>association_type.
    DATA: using_method_id_act TYPE i,
          used_id_act         TYPE i.

    association = VALUE #( element_id1 = 1
                           element_id2 = 2
                           ass_type = |A|
                             ).

    TEST-INJECTION get_element.

      invoced_element = z2mse_extr3_classes=>get_instance( element_manager ).

      invocing_element = z2mse_extr3_classes=>get_instance( element_manager ).

    END-TEST-INJECTION.

    f_cut->_get_famix_id_used_and_using(
      EXPORTING
        i_association     = association
      IMPORTING
        e_using_method_id = using_method_id_act
        e_used_id         = used_id_act ).

        cl_abap_unit_assert=>assert_equals( msg = 'using_method_id_act' exp = 0 act = using_method_id_act ).

        cl_abap_unit_assert=>assert_equals( msg = 'used_id_act' exp = 0 act = used_id_act ).

  ENDMETHOD.

ENDCLASS.
