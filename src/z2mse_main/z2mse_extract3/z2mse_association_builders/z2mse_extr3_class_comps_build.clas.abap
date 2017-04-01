CLASS z2mse_extr3_class_comps_build DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_class_comps_build.
    METHODS search_down REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_class_comps_build.
ENDCLASS.



CLASS z2mse_extr3_class_comps_build IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance.

  ENDMETHOD.

  METHOD search_down.

    DATA: element TYPE REF TO z2mse_extr3_elements,
          classes TYPE REF TO z2mse_extr3_classes,
          class_component_elements TYPE REF TO z2mse_extr3_class_comp_ass.

    "! use as replacement for ty_component_long
    "! Required because in case of interfaces the name of the interface is together with a ~ part of the name
    TYPES ty_component_long TYPE c LENGTH 61.

    TYPES: BEGIN OF ty_class_component,
             clsname TYPE seoclsname,
             cmpname TYPE ty_component_long,
             cmptype TYPE seocmptype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE STANDARD TABLE OF ty_class_component WITH KEY clsname cmpname cmptype.
    DATA: class_components TYPE ty_class_components,
          class_component  TYPE ty_class_component.

    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).

    class_component_elements = z2mse_extr3_class_comp_ass=>get_instance( i_element_manager = element_manager ).

    element = element_manager->get_element( element_id ).

    IF element->type EQ element->class_type.

      DATA class_name TYPE seoclsname.

      class_name = classes->class_name( element_id = element_id ).

      TEST-SEAM seocompo.

        SELECT clsname cmpname cmptype
          FROM seocompo
          INTO CORRESPONDING FIELDS OF TABLE class_components
          WHERE cmptype <> 3 " A type
            AND clsname = class_name.

      END-TEST-SEAM.

      LOOP AT class_components INTO class_component.

      " TBD
*      class_component_elements->add(
*        EXPORTING
*          component_element_id =
*          class_element_id     =
*      ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
