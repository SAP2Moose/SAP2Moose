CLASS z2mse_extr3_where_used_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS search_up REDEFINITION.
    METHODS search_down REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3_where_used_builder IMPLEMENTATION.

  METHOD search_up.

    DATA: element TYPE REF TO z2mse_extr3_elements.

    element = element_manager->get_element( element_id ).

    CASE element->type.
      WHEN element->class_type.

        DATA classes TYPE REF TO z2mse_extr3_classes.
        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        DATA class_name TYPE seoclsname.
        DATA cmpname TYPE seocmpname.
        DATA cmptype TYPE seocmptype.

        classes->comp_name( EXPORTING element_id = element_id
                            IMPORTING class_name = class_name
                                      cmpname    = cmpname
                                      cmptype = cmptype ).

      WHEN element->table_type.

        DATA tables TYPE REF TO z2mse_extr3_tables.
        tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).

        DATA table TYPE tabname.

        table = tables->table_name( i_element_id = element_id ).

    ENDCASE.

  ENDMETHOD.

  METHOD search_down.

    DATA: element TYPE REF TO z2mse_extr3_elements.

    element = element_manager->get_element( element_id ).

  ENDMETHOD.

ENDCLASS.
