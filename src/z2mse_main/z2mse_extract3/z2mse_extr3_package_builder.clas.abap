"! I build elements of type package
CLASS z2mse_extr3_package_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_element_builder
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_package_builder.
    METHODS add
      EXPORTING package TYPE devclass.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_package_builder.
    DATA packages TYPE REF TO z2mse_extr3_packages.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO z2mse_extr3_element_manager.
ENDCLASS.



CLASS z2mse_extr3_package_builder IMPLEMENTATION.

  METHOD get_instance.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance.

  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_element_manager = i_element_manager ).
    packages = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).
  ENDMETHOD.

  METHOD add.

    packages->add( IMPORTING package = package ).

  ENDMETHOD.
ENDCLASS.
