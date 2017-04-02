CLASS z2mse_extr3_access DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO z2mse_extr3_access.
    METHODS add
      IMPORTING
        accessed_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type
        accessing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_access.

    TYPES: BEGIN OF association_type,
             accessed_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type,
             accessing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY accessed_element_id1 accessing_element_id2.
ENDCLASS.



CLASS z2mse_extr3_access IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = access_ass..
    r_instance = instance.
  ENDMETHOD.

  METHOD add.

    DATA association TYPE association_type.

    association-accessed_element_id1 = accessed_element_id1.
    association-accessing_element_id2 = accessing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-accessed_element_id1
                                                element_2   = association-accessing_element_id2
                                                association = me ).

  ENDMETHOD.

  METHOD make_model.

  ENDMETHOD.

ENDCLASS.
