CLASS z2mse_extr3_parent_package DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_parent_package.
    METHODS add
      IMPORTING
        element_id        TYPE z2mse_extr3_element_manager=>element_id_type
        parent_element_id TYPE z2mse_extr3_element_manager=>element_id_type
          PREFERRED PARAMETER element_id.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_parent_package.

    TYPES: BEGIN OF association_type,
             element_id1 TYPE z2mse_extr3_element_manager=>element_id_type,
             element_id2 TYPE z2mse_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY element_id1 element_id2.

ENDCLASS.



CLASS z2mse_extr3_parent_package IMPLEMENTATION.


  METHOD add.

    DATA association TYPE association_type.

    association-element_id1 = element_id.
    association-element_id2 = parent_element_id.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-element_id1
                                                element_2   = association-element_id2
                                                association = me ).

  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance.
    instance->type = parent_package_ass.
  ENDMETHOD.
ENDCLASS.
