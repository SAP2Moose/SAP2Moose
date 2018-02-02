"! I am the top superclass for all classes that require the element manager.
CLASS z2mse_extr3 DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    "! Call once to clear all global variables. This is required before an extraction is repeated
    CLASS-METHODS clear_all.
    CONSTANTS: ng_source_language TYPE string VALUE 'SOURCE_LANGUAGE',
               ng_abap_package TYPE string VALUE 'ABAP_PACKAGE',
               ng_abap_class TYPE string VALUE 'ABAP_CLASS',
               ng_abap_method TYPE string VALUE 'ABAP_METHOD',
               ng_abap_program TYPE string VALUE 'ABAP_PROGRAM',
               ng_abap_webdynpro TYPE string VALUE 'ABAP_WEBDYNPRO',
               ng_sap_table TYPE string VALUE 'SAP_TABLE'.
  PROTECTED SECTION.
    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO z2mse_extr3_element_manager.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_EXTR3 IMPLEMENTATION.


  METHOD clear_all.

    z2mse_extr3_access=>clear( ).
    z2mse_extr3_invocation=>clear( ).
    z2mse_extr3_parent_package=>clear( ).
    z2mse_extr3_classes=>clear( ).
    z2mse_extr3_packages=>clear( ).
    z2mse_extr3_programs=>clear( ).
    z2mse_extr3_tables=>clear( ).
    z2mse_extr3_web_dynpro_comp=>clear( ).

  ENDMETHOD.


  METHOD constructor.
    element_manager = i_element_manager.
  ENDMETHOD.
ENDCLASS.
