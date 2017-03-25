CLASS z2mse_extract3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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


  METHOD extract.

  ENDMETHOD.

ENDCLASS.
