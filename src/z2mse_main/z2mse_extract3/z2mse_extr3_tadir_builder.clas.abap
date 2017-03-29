CLASS z2mse_extr3_tadir_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS search_down REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_EXTR3_TADIR_BUILDER IMPLEMENTATION.


  METHOD search_down.

    data: element TYPE REF TO z2mse_extr3_elements,
          package TYPE REF TO z2mse_extr3_packages.

    element = element_manager->get_element( element_id ).

    if element->type eq element->package_type.

    package ?= element.

      data devclass TYPE devclass.

      devclass = package->devclass( element_id ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
