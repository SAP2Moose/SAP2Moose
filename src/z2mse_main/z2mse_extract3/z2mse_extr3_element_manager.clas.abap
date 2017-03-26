"! I know all elements and associations between elements that are currently known.
"! I provide general methods to add new elements and associations between elements.
CLASS z2mse_extr3_element_manager DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    DATA model            TYPE REF TO z2mse_model.
    DATA famix_package     TYPE REF TO z2mse_famix_package.
    DATA famix_class     TYPE REF TO z2mse_famix_class.
    DATA famix_method     TYPE REF TO z2mse_famix_method.
    DATA famix_attribute     TYPE REF TO z2mse_famix_attribute.
    DATA famix_invocation     TYPE REF TO z2mse_famix_invocation.
    DATA famix_access     TYPE REF TO z2mse_famix_access.
    "! A unique identifier for each object extracted
    TYPES element_id_type TYPE i.

    TYPES: BEGIN OF association_type,
             element_id1 TYPE element_id_type,
             element_id2 TYPE element_id_type,
             association TYPE REF TO z2mse_extr3_association,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.

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
    METHODS make_model
      RETURNING
        VALUE(r_result) TYPE z2mse_model=>lines_type.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id TYPE element_id_type,
             "! A reference to the instance that handles this object
             element     TYPE REF TO z2mse_extr3_elements,
           END OF element_type.
    TYPES elements_type TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements TYPE elements_type.
    TYPES associations1_type TYPE SORTED TABLE OF association_type WITH NON-UNIQUE KEY element_id1.
    TYPES associations2_type TYPE SORTED TABLE OF association_type WITH NON-UNIQUE KEY element_id2.
    DATA associations1 TYPE associations1_type.
    DATA associations2 TYPE associations2_type.
    DATA next_element_id TYPE i.
ENDCLASS.



CLASS Z2MSE_EXTR3_ELEMENT_MANAGER IMPLEMENTATION.


  METHOD add_association.

  ENDMETHOD.


  METHOD add_element.

    DATA element_line TYPE element_type.
    element_line-element_id = next_element_id.
    element_id = next_element_id.
    element_line-element =  element.
    INSERT element_line INTO TABLE elements.
    ADD 1 TO next_element_id.

  ENDMETHOD.


  METHOD constructor.
    next_element_id = 1.



    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

  ENDMETHOD.


  METHOD make_model.

    DATA: element      TYPE element_type,
          associations TYPE associations_type,
          association  TYPE association_type.

    LOOP AT elements INTO element.

      LOOP AT associations1 INTO association WHERE element_id1 = element-element_id.
        INSERT association INTO TABLE associations.
      ENDLOOP.

      LOOP AT associations2 INTO association WHERE element_id2 = element-element_id.
        INSERT association INTO TABLE associations.
      ENDLOOP.

      element-element->make_model( element_id = element-element_id
                                   associations = associations ).

    ENDLOOP.


    model->make_mse( IMPORTING mse_model = r_result ).

  ENDMETHOD.
ENDCLASS.
