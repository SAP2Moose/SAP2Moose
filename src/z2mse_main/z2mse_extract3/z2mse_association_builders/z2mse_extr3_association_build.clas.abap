"! I know how to build required associations
"! I have subclasses with concrete specifications that are used to find or build concrete associations.
CLASS z2mse_extr3_association_build DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3.

  PUBLIC SECTION.

    METHODS search_down
      IMPORTING
        element_id TYPE z2mse_extr3_element_manager=>element_id_type.

    METHODS search_up
      IMPORTING
        element_id TYPE z2mse_extr3_element_manager=>element_id_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_EXTR3_ASSOCIATION_BUILD IMPLEMENTATION.

  METHOD search_down.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.

  METHOD search_up.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.

ENDCLASS.
