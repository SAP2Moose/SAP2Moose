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
        DATA: invoced_class_name TYPE seoclsname,
              invoced_cmpname    TYPE seocmpname,
              invoced_cmptype    TYPE seocmptype.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = i_association-element_id1
                             IMPORTING class_name = invoced_class_name
                                       cmpname    = invoced_cmpname
                                       cmptype    = invoced_cmptype ).
        CASE invoced_cmptype.
          WHEN classes->attribute_type.

            e_used_id = element_manager->famix_attribute->get_id(  class            = invoced_class_name
                                                                   attribute           = invoced_cmpname ).
          WHEN classes->method_type OR classes->event_type.
            e_used_id = element_manager->famix_method->get_id( class_name_group = ''
                                                             class            = invoced_class_name
                                                             method           = invoced_cmpname ).
        ENDCASE.
      WHEN invoced_element->table_type.
        DATA tables TYPE REF TO z2mse_extr3_tables.
        DATA tabname TYPE tabname.
        tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
        tabname = tables->table_name( i_element_id = i_association-element_id1 ).

        e_used_id = element_manager->famix_attribute->get_id(  class            = tabname
                                                               attribute           = tabname ).
    ENDCASE.

    CASE invocing_element->type.
      WHEN invocing_element->class_type.

        DATA: invocing_class_name TYPE seoclsname,
              invocing_cmpname    TYPE seocmpname.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = i_association-element_id2
                             IMPORTING class_name = invocing_class_name
                                       cmpname    = invocing_cmpname ).
    ENDCASE.

    DATA using_method_id TYPE i.

    e_using_method_id = element_manager->famix_method->get_id( class  = invocing_class_name
                                                             method = invocing_cmpname ).

  ENDMETHOD.
ENDCLASS.
