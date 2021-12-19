"! I am the starting point for an extraction. I am called from the main report.
class Z2MSE_EXTRACT3 definition
  public
  final
  create public .

public section.

  types:
    ty_s_pack TYPE RANGE OF tadir-devclass .
  types:
    ty_string_range TYPE RANGE OF char45 .

  constants TECHTYPE_ABAPPACKAGE type STRING value 'ABAPPackage' ##NO_TEXT.
  constants TECHTYPE_ABAPMETHOD type STRING value 'ABAPMethod' ##NO_TEXT.
  constants TECHTYPE_ABAPCLASSATTRIBUTE type STRING value 'ABAPClassAttribute' ##NO_TEXT.
  constants TECHTYPE_ABAP_FUNCTION type STRING value 'ABAPFunktion' ##NO_TEXT.
  constants TECHTYPE_WEBDYNPRO_CONTROLLER type STRING value 'ABAPWebDynproController' ##NO_TEXT.
  constants MODIFIER_ABAPGLOBALCLASS type STRING value 'ABAPGlobalClass' ##NO_TEXT.
  constants MODIFIER_ABAPGLOBALINTERFACE type STRING value 'ABAPGlobalInterface' ##NO_TEXT.
  constants MODIFIER_WEBDYNPRO_COMPONENT type STRING value 'ABAPWebDynproComponent' ##NO_TEXT.
  constants MODIFIER_DBTABLE type STRING value 'DBTable' ##NO_TEXT.
  constants MODIFIER_PROGRAM type STRING value 'ABAPProgram' ##NO_TEXT.
  constants MODIFIER_FUNCTION_GROUP type STRING value 'ABAPFunktionGroup' ##NO_TEXT.
  constants MODIFIER_BW_TRANSFORMATION type STRING value 'BWTransformation' ##NO_TEXT.
  constants MODIFIER_UNKNOWN type STRING value 'UNKNOWN' ##NO_TEXT.

  class-methods CHECK_IF_TESTED
    returning
      value(IS_TESTED) type ABAP_BOOL .
  methods CONSTRUCTOR .
    "! Main start to do the extraction
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    "! @parameter i_exclude_found_sap_intf | exclude found interfaces in SAP namespace in the where-used analysis
  methods EXTRACT
    importing
      !MODEL_BUILDER type ref to Z2MSE_EXTR3_MODEL_BUILDER
      !ELEMENT_MANAGER type ref to Z2MSE_EXTR3_ELEMENT_MANAGER
      !INITIAL_ELEMENTS type ref to Z2MSE_EXTR3_INITIAL_ELEMENTS
      !I_SEARCH_UP type I
      !I_SEARCH_DOWN type I
      !I_EXCLUDE_FOUND_SAP_INTF type ABAP_BOOL
    exporting
      !MSE_MODEL type Z2MSE_MODEL=>LINES_TYPE
      value(NOTHING_DONE) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: g_check_for_test_done TYPE abap_bool,
                g_is_tested           TYPE abap_bool.
ENDCLASS.



CLASS Z2MSE_EXTRACT3 IMPLEMENTATION.


  METHOD check_if_tested.

    IF g_check_for_test_done EQ 'X'.
      " Buffer result of call to call stack
      is_tested = g_is_tested.
    ELSE.

      DATA et_callstack  TYPE sys_callst  .
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          et_callstack = et_callstack.

      READ TABLE et_callstack TRANSPORTING NO FIELDS WITH KEY eventname = 'INVOKE_TEST_METHOD'.

      IF sy-subrc EQ 0.
        g_is_tested = 'X'.
        is_tested = 'X'.
        g_check_for_test_done = 'X'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

  ENDMETHOD.


  METHOD extract.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Collect initial elements'.

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

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Make model file'.

    DATA sysid TYPE string.

    sysid = sy-sysid.

    element_manager->collect_infos( sysid ).

    mse_model = element_manager->make_model( ).

  ENDMETHOD.
ENDCLASS.
