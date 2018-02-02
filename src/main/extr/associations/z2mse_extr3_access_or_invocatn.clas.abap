CLASS z2mse_extr3_access_or_invocatn DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS _get_famix_id_used_and_using
      IMPORTING
        i_association     TYPE z2mse_extr3_element_manager=>association_type
      EXPORTING
        e_using_method_id TYPE i
        e_used_id         TYPE i.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3_access_or_invocatn IMPLEMENTATION.

  METHOD _get_famix_id_used_and_using.

    DATA: invoced_element  TYPE REF TO z2mse_extr3_elements,
          invocing_element TYPE REF TO z2mse_extr3_elements.

    DATA used_id TYPE i.

    invoced_element = element_manager->get_element( i_element_id = i_association-element_id1 ).

    invocing_element = element_manager->get_element( i_element_id = i_association-element_id2 ).

    CASE invoced_element->type.
      WHEN invoced_element->class_type.
        DATA classes TYPE REF TO z2mse_extr3_classes.
        DATA: invoced_class_name TYPE string,
              invoced_cmpname    TYPE string,
              invoced_cmptype    TYPE seocmptype.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = i_association-element_id1
                             IMPORTING class_name = invoced_class_name
                                       cmpname    = invoced_cmpname
                                       cmptype    = invoced_cmptype ).
        CASE invoced_cmptype.
          WHEN classes->attribute_type.

            e_used_id = element_manager->famix_attribute->get_id(  name_group = ng_abap_class
                                                                   class      = invoced_class_name
                                                                   attribute  = invoced_cmpname ).
          WHEN classes->method_type OR classes->event_type.
            e_used_id = element_manager->famix_method->get_id( class_name_group = ng_abap_class
                                                             class            = invoced_class_name
                                                             method_name_group = ng_abap_method
                                                             method           = invoced_cmpname ).
        ENDCASE.
      WHEN invoced_element->table_type.
        DATA tables TYPE REF TO z2mse_extr3_tables.
        DATA tabname TYPE tabname.
        tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
        tabname = tables->table_name( i_element_id = i_association-element_id1 ).

        e_used_id = element_manager->famix_attribute->get_id(  name_group = ng_sap_table
                                                               class      = tabname
                                                               attribute  = tabname ).
      WHEN invoced_element->program_type.
        DATA programs2 TYPE REF TO z2mse_extr3_programs.
        DATA: invoced_ext_progr_name_class  TYPE string,
              invoced_ext_progr_name_method TYPE string.

        programs2 = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).
        programs2->program_name( EXPORTING i_element_id = i_association-element_id1
                                 IMPORTING external_program_name_class = invoced_ext_progr_name_class
                                           external_program_name_method = invoced_ext_progr_name_method ).

        e_used_id = element_manager->famix_method->get_id( class_name_group = ng_abap_program
                                                           class             = invoced_ext_progr_name_class
                                                           method_name_group = ng_abap_program
                                                           method            = invoced_ext_progr_name_method ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    DATA: invoicing_famix_class       TYPE string,
          invoicing_famix_method      TYPE string,
          invoicing_class_name_group  TYPE string,
          invoicing_method_name_group TYPE string.

    CASE invocing_element->type.
      WHEN invocing_element->class_type.

        invoicing_class_name_group = ng_abap_class.
        invoicing_method_name_group = ng_abap_method.

        DATA: invocing_class_name TYPE string,
              invocing_cmpname    TYPE string.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).

        classes->comp_name( EXPORTING element_id  = i_association-element_id2
                             IMPORTING class_name = invocing_class_name
                                       cmpname    = invocing_cmpname ).

        invoicing_famix_class = invocing_class_name.
        invoicing_famix_method = invocing_cmpname.

      WHEN invocing_element->web_dynpro_comps_type.

        invoicing_class_name_group = ng_abap_webdynpro.
        invoicing_method_name_group = ng_abap_webdynpro.

        DATA web_dynpro_component TYPE REF TO z2mse_extr3_web_dynpro_comp.

        DATA: invocing_wdy_component_name  TYPE wdy_component_name,
              invocing_wdy_controller_name TYPE wdy_controller_name.


        web_dynpro_component = z2mse_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

        web_dynpro_component->wdy_controller_name( EXPORTING element_id          = i_association-element_id2
                                                   IMPORTING wdy_component_name  = invocing_wdy_component_name
                                                             wdy_controller_name = invocing_wdy_controller_name ).

        invoicing_famix_class = invocing_wdy_component_name.
        invoicing_famix_method = invocing_wdy_controller_name.

      WHEN invocing_element->program_type.

        invoicing_class_name_group = ng_abap_program.
        invoicing_method_name_group = ng_abap_program.

        DATA programs TYPE REF TO z2mse_extr3_programs.

        programs = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).

        programs->program_name( EXPORTING i_element_id          = i_association-element_id2
                                IMPORTING external_program_name_class = invoicing_famix_class
                                          external_program_name_method = invoicing_famix_method ).

      WHEN OTHERS.
        ASSERT 1 = 2.

    ENDCASE.

    DATA using_method_id TYPE i.
    ASSERT invoicing_class_name_group IS NOT INITIAL.
    ASSERT invoicing_method_name_group IS NOT INITIAL.
    e_using_method_id = element_manager->famix_method->get_id( class_name_group = invoicing_class_name_group
                                                               class  = invoicing_famix_class
                                                               method_name_group = invoicing_method_name_group
                                                               method = invoicing_famix_method ).

  ENDMETHOD.
ENDCLASS.
