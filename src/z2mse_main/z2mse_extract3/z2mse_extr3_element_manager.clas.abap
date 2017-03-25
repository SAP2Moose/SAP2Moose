"! I know all elements and associations between elements that are currently known.
"! I provide general methods to add new elements and associations between elements.
CLASS z2mse_extr3_element_manager DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    "! A unique identifier for each object extracted
    TYPES element_id_type TYPE i.
    METHODS constructor.
    "! Call if an element might be added.
    "! Add the element if it is not already part of the model.
    METHODS add_element
      IMPORTING
                element           TYPE REF TO z2mse_extr3_elements
      RETURNING VALUE(element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS add_association
      IMPORTING
        element_specification_1   TYPE REF TO z2mse_extr3_element_specifictn
        element_specification_2   TYPE REF TO z2mse_extr3_element_specifictn
        association_specification TYPE REF TO z2mse_extr3_association_spec.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF object_type,
             obj_id TYPE element_id_type,
             "! A reference to the instance that handles this object
             object TYPE REF TO z2mse_extr3_elements,
           END OF object_type.
    TYPES objects_type TYPE HASHED TABLE OF object_type WITH UNIQUE KEY obj_id.
    DATA objects TYPE objects_type.
    TYPES: BEGIN OF association_type,
             obj_id1     TYPE element_id_type,
             obj_id2     TYPE element_id_type,
             association TYPE REF TO z2mse_extr3_association,
           END OF association_type.
    TYPES associations1_type TYPE SORTED TABLE OF association_type WITH NON-UNIQUE KEY obj_id1.
    TYPES associations2_type TYPE SORTED TABLE OF association_type WITH NON-UNIQUE KEY obj_id2.
    DATA associations1 TYPE associations1_type.
    DATA associations2 TYPE associations2_type.
    DATA next_object_id TYPE i.
ENDCLASS.



CLASS z2mse_extr3_element_manager IMPLEMENTATION.


  METHOD add_association.

  ENDMETHOD.


  METHOD add_element.

    DATA element_line TYPE object_type.
    element_line-obj_id = next_object_id.
    element_line-object =  element.
    INSERT element_line INTO TABLE objects.
    ADD 1 TO next_object_id.

  ENDMETHOD.


  METHOD constructor.
    next_object_id = 1.
  ENDMETHOD.
ENDCLASS.
