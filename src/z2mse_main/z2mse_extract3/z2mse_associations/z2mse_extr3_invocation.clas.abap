CLASS z2mse_extr3_invocation DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO z2mse_extr3_invocation.
    METHODS add
      IMPORTING
        invoced_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type
        invocing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_invocation.

    TYPES: BEGIN OF association_type,
             invoced_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type,
             invocing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY invoced_element_id1 invocing_element_id2.
ENDCLASS.



CLASS z2mse_extr3_invocation IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = invocation_ass.
    r_instance = instance.
  ENDMETHOD.

  METHOD add.

    DATA association TYPE association_type.

    association-invoced_element_id1 = invoced_element_id1.
    association-invocing_element_id2 = invocing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-invoced_element_id1
                                                element_2   = association-invocing_element_id2
                                                association = me ).

  ENDMETHOD.

  METHOD make_model.

    DATA: invoced_element  TYPE REF TO z2mse_extr3_elements,
          invocing_element TYPE REF TO z2mse_extr3_elements.

    invoced_element = element_manager->get_element( i_element_id = association-element_id1 ).

    invocing_element = element_manager->get_element( i_element_id = association-element_id2 ).

    CASE invoced_element->type.
      WHEN invoced_element->class_type.
        DATA classes TYPE REF TO z2mse_extr3_classes.
        DATA: invoced_class_name TYPE seoclsname,
              invoced_cmpname    TYPE seocmpname.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = association-element_id1
                             IMPORTING class_name = invoced_class_name
                                       cmpname    = invoced_cmpname ).
    ENDCASE.

    CASE invocing_element->type.
      WHEN invocing_element->class_type.

        DATA: invocing_class_name TYPE seoclsname,
              invocing_cmpname    TYPE seocmpname.

        classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = association-element_id1
                             IMPORTING class_name = invocing_class_name
                                       cmpname    = invocing_cmpname ).
    ENDCASE.

    DATA using_method_id TYPE i.

    using_method_id = element_manager->famix_method->get_id( class  = invocing_class_name
                                                             method = invocing_cmpname ).

    DATA used_id TYPE i.

    used_id = element_manager->famix_method->get_id( class_name_group = ''
                                                     class            = invoced_class_name
                                                     method           = invoced_cmpname ).

    DATA invocation_id TYPE i.
    invocation_id = element_manager->famix_invocation->add( ).
    element_manager->famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                               sender_id     = using_method_id
                                                               candidates_id = used_id
                                                               signature     = 'DUMMY' ).



  ENDMETHOD.

ENDCLASS.
