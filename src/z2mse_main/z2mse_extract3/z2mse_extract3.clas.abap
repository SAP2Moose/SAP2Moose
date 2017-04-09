"! I am the starting point for an extraction. I am called from the main report.
CLASS z2mse_extract3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_s_pack TYPE RANGE OF tadir-devclass .
    TYPES: ty_string_range TYPE RANGE OF char45.
    CONSTANTS modifier_abapglobalclass TYPE string VALUE 'ABAPGlobalClass' ##NO_TEXT.
    CONSTANTS modifier_abapglobalinterface TYPE string VALUE 'ABAPGlobalInterface' ##NO_TEXT.
    CONSTANTS modifier_webdynpro_component TYPE string VALUE 'ABAPWebDynproComponent'.
    CONSTANTS modifier_dbtable TYPE string VALUE 'DBTable' ##NO_TEXT.
    CONSTANTS modifier_program TYPE string VALUE 'ABAPProgram' ##NO_TEXT.

    METHODS constructor.

    "! Main start to do the extraction
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    "! @parameter i_exclude_found_sap_intf | exclude found interfaces in SAP namespace in the where-used analysis
    METHODS extract
      IMPORTING
        model_builder TYPE REF TO z2mse_extr3_model_builder
        element_manager TYPE REF TO z2mse_extr3_element_manager
        !initial_elements        TYPE REF TO z2mse_extr3_initial_elements
        i_search_up              TYPE i
        i_search_down            TYPE i
        i_exclude_found_sap_intf TYPE abap_bool
      EXPORTING
        !mse_model               TYPE z2mse_model=>lines_type
        VALUE(nothing_done)      TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_EXTRACT3 IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD extract.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Collect initial elements|.

    model_builder->initial_selection_started( ).

*    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
*    CREATE OBJECT element_manager
*      EXPORTING
*        i_model_builder          = model_builder
*        i_exclude_found_sap_intf = i_exclude_found_sap_intf.

*    model_builder->initialize( i_element_manager = element_manager ).

    DATA packages_elements TYPE REF TO z2mse_extr3_packages.

    packages_elements = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).

    DATA: packages TYPE z2mse_extr3_initial_elements=>ty_packages,
          package  TYPE z2mse_extr3_initial_elements=>ty_package.

    packages = initial_elements->get_selected( ).

    LOOP AT packages INTO package.

      packages_elements->add( EXPORTING package = package-package ).

    ENDLOOP.

    model_builder->search( i_search_up           = i_search_up
                           i_search_down         = i_search_down ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Write found elements|.

    TEST-SEAM write_found_elements.

      model_builder->write_found_elements( ).

    END-TEST-SEAM.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Make model file|.

    mse_model = element_manager->make_model( ).

  ENDMETHOD.
ENDCLASS.
