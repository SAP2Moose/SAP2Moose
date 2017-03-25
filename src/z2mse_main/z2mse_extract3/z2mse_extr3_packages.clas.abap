CLASS z2mse_extr3_packages DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_packages.
    METHODS add
      EXPORTING package TYPE devclass.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             obj_id   TYPE z2mse_extr3_element_manager=>element_id_type,
             devclass TYPE devclass,
           END OF element_type.
    DATA instance TYPE REF TO z2mse_extr3_packages.
    DATA elements_obj_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY obj_id.
    DATA elements_devclass TYPE HASHED TABLE OF element_type WITH UNIQUE KEY devclass.
ENDCLASS.

CLASS z2mse_extr3_packages IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance.
  ENDMETHOD.

  METHOD add.

    READ TABLE elements_devclass TRANSPORTING NO FIELDS WITH KEY devclass = package.
    IF sy-subrc <> 0.
      DATA new_element_id TYPE z2mse_extr3_element_manager=>element_id_type.
      new_element_id = element_manager->add_element( element = me ).

      DATA element TYPE element_type.
      element-obj_id = new_element_id.
      element-devclass = package.
      INSERT element INTO TABLE elements_obj_id.
      INSERT element INTO TABLE elements_devclass.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
