CLASS z2mse_extr3_packages_mock DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_packages
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear_mock.
    CLASS-METHODS get_mock_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_packages_mock.
  PROTECTED SECTION.
    METHODS _does_package_exists REDEFINITION.
  PRIVATE SECTION.
    CLASS-DATA instance_mock TYPE REF TO z2mse_extr3_packages_mock.
ENDCLASS.



CLASS Z2MSE_EXTR3_PACKAGES_MOCK IMPLEMENTATION.


  METHOD clear_mock.
    CLEAR instance_mock.
  ENDMETHOD.


  METHOD get_mock_instance.

    IF instance_mock IS NOT BOUND.
      CREATE OBJECT instance_mock
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance_mock.
    instance->type = package_type.

  ENDMETHOD.


  METHOD _does_package_exists.
    exists = abap_true.
  ENDMETHOD.
ENDCLASS.
