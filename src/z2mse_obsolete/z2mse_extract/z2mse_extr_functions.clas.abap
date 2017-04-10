CLASS z2mse_extr_functions DEFINITION
  PUBLIC

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_function_group_component,
             "! program of a function group
             fugr_program TYPE progname,
             function     TYPE rs38l_fnam,
             includenr    TYPE includenr,
             "! use include to determine the connection between where used and the FAMIX model of a function
             include      TYPE programm,
           END OF ty_function_group_component.
    TYPES: ty_function_groups_compts_hash TYPE HASHED TABLE OF ty_function_group_component WITH UNIQUE KEY fugr_program function.

    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.
    "! Call once to select all functions that are in a list of packages
    METHODS select_by_packages.
    METHODS select_by_includes.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: tadir_function_group TYPE tadir-object VALUE 'FUGR' ##NO_TEXT.

    TYPES: BEGIN OF ty_function_group,
             "! program of a function group
             fugr_program TYPE progname,
             devclass     TYPE tadir-devclass,
             exists       TYPE abap_bool,
           END OF ty_function_group.
    TYPES: ty_function_groups TYPE HASHED TABLE OF ty_function_group WITH UNIQUE KEY fugr_program.
    "! A list of all primarily selected and existing function groups
    DATA g_selected_function_groups TYPE ty_function_groups.
    "! A list of all components of primarily selected and existing components. Only if not yet transfered to where used analysis
    DATA g_selected_fugr_comps_new TYPE ty_function_groups_compts_hash.
    "! A list of all components of primarily selected and existing function groups
    DATA g_selected_fugr_comps TYPE ty_function_groups_compts_hash.


    METHODS _convert_fugr_2_progname
      IMPORTING
        tadirline       TYPE ty_tadir_test
      RETURNING
        VALUE(progname) TYPE progname.
    METHODS _convert_fugr_progname_2_fugr
      IMPORTING
                fugr_progname TYPE progname
      RETURNING VALUE(fugr)   TYPE string.
    METHODS _check_existence
      CHANGING
        function_groups TYPE z2mse_extr_functions=>ty_function_groups.
    METHODS _read_function_group_comps
      IMPORTING
        function_groups             TYPE ty_function_groups
      RETURNING
        VALUE(function_group_comps) TYPE ty_function_groups_compts_hash.
ENDCLASS.



CLASS Z2MSE_EXTR_FUNCTIONS IMPLEMENTATION.


  METHOD select_by_includes.

  ENDMETHOD.


  METHOD select_by_packages.


    " Determine the name of the include for the function

    FIELD-SYMBOLS <fugr_comp> TYPE ty_function_group_component.

    LOOP AT g_selected_fugr_comps_new ASSIGNING <fugr_comp>.

      DATA fugr TYPE string.

      " TBD check for partner namespace (Program FUNC_GET_OBJECT)

      fugr = _convert_fugr_progname_2_fugr( fugr_progname = <fugr_comp>-fugr_program ).

      CONCATENATE 'L' fugr 'U' <fugr_comp>-includenr INTO <fugr_comp>-include.
    ENDLOOP.

  ENDMETHOD.


  METHOD _check_existence.

    TYPES: BEGIN OF ty_existing,
             name TYPE progname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <function_group> LIKE LINE OF function_groups.

    IF function_groups IS NOT INITIAL.
      SELECT name FROM progdir INTO TABLE table FOR ALL ENTRIES IN function_groups WHERE name = function_groups-fugr_program.
    ENDIF.

    LOOP AT table INTO line.

      READ TABLE function_groups ASSIGNING <function_group> WITH KEY fugr_program = line-name.
      IF sy-subrc EQ 0.
        <function_group>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE function_groups WHERE exists = abap_false.

  ENDMETHOD.


  METHOD _convert_fugr_2_progname.

    " convert_fugr_2_progname
    progname = |SAPL| && |{ tadirline-obj_name }|.

  ENDMETHOD.


  METHOD _convert_fugr_progname_2_fugr.

    fugr = fugr_progname+4.

  ENDMETHOD.


  METHOD _read_function_group_comps.
    " TBD 17.03.2017
    IF function_groups IS NOT INITIAL.
      SELECT funcname AS function
             pname    AS fugr_program
             include AS includenr FROM tfdir INTO CORRESPONDING FIELDS OF TABLE function_group_comps
             FOR ALL ENTRIES IN function_groups
             WHERE pname = function_groups-fugr_program.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
