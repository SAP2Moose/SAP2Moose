"! I am the abstract super class of all elements.
"! My subclasses know the details of elements.
CLASS z2mse_extr3_elements DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3.

  PUBLIC SECTION.

    DATA type TYPE c LENGTH 30.

    CONSTANTS: package_type LIKE type VALUE 'package',
               table_type like type VALUE 'table',
               class_type like type value 'class',
               class_components_type like TYPE value 'class_components'.

    METHODS make_model
      IMPORTING
        element_id   TYPE z2mse_extr3_element_manager=>element_id_type
        associations TYPE z2mse_extr3_element_manager=>associations_type.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS Z2MSE_EXTR3_ELEMENTS IMPLEMENTATION.


  METHOD make_model.

  ENDMETHOD.
ENDCLASS.
