"! I am the abstract super class of all elements.
"! My subclasses know the details of elements.
CLASS z2mse_extr3_elements DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3.

  PUBLIC SECTION.

    METHODS make_model
      IMPORTING
        associations TYPE z2mse_extr3_element_manager=>associations_type.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS z2mse_extr3_elements IMPLEMENTATION.


  METHOD make_model.

  ENDMETHOD.

ENDCLASS.
