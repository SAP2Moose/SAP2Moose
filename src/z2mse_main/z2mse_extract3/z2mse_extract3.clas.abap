"! I am the starting point for an extraction. I am called from the main report.
CLASS z2mse_extract3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    "! Main start to do the extraction
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    "! @parameter i_exclude_found_sap_intf | exclude found interfaces in SAP namespace in the where-used analysis
    METHODS extract
      IMPORTING
        !initial_elements        TYPE REF TO z2mse_extr3_initial_elements
        i_search_up              TYPE i
        i_exclude_found_sap_intf TYPE abap_bool
      EXPORTING
        !mse_model               TYPE z2mse_model=>lines_type
        VALUE(nothing_done)      TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extract3 IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD extract.

    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.
    CREATE OBJECT model_builder.

    model_builder->initial_selection_started( ).

    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    CREATE OBJECT element_manager EXPORTING i_model_builder = model_builder.

    model_builder->initialize( element_manager = element_manager ).

    DATA packages_elements TYPE REF TO z2mse_extr3_packages.

    packages_elements = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).

    DATA: packages TYPE z2mse_extr3_initial_elements=>ty_packages,
          package  TYPE z2mse_extr3_initial_elements=>ty_package.

    packages = initial_elements->get_selected( ).

    LOOP AT packages INTO package.

      packages_elements->add( EXPORTING package = package-package ).

    ENDLOOP.

    model_builder->search( ).

    mse_model = element_manager->make_model( ).

  ENDMETHOD.
ENDCLASS.
