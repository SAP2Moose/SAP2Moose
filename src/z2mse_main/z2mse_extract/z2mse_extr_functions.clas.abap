CLASS z2mse_extr_functions DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_function_group,
             "! program of a function group
             fugr_program TYPE progname,
             devclass     TYPE tadir-devclass,
             exists       TYPE abap_bool,
           END OF ty_function_group.
    TYPES: ty_function_groups TYPE HASHED TABLE OF ty_function_group WITH UNIQUE KEY fugr_program.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.
    "! Call once to select all functions that are in a list of packages
    METHODS select_by_packages
      IMPORTING
        packages TYPE z2mse_extr_packages=>ty_packages.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: tadir_function_group TYPE tadir-object VALUE 'FUGR' ##NO_TEXT.
    DATA g_selected_function_groups TYPE z2mse_extr_functions=>ty_function_groups.
    METHODS _select_from_tadir
      IMPORTING
        i_packages                      TYPE z2mse_extr_packages=>ty_packages
      RETURNING
        VALUE(selected_function_groups) TYPE ty_function_groups.
    METHODS add_and_sort
      IMPORTING
        i_tadirvalues     TYPE ty_t_tadir_test
      CHANGING
        c_selected_tables TYPE ty_function_groups.
    METHODS _convert_fugr_2_progname
      IMPORTING
        tadirline       TYPE ty_tadir_test
      RETURNING
        VALUE(progname) TYPE progname.
    METHODS _check_existence
      CHANGING
        function_groups TYPE z2mse_extr_functions=>ty_function_groups.
ENDCLASS.



CLASS z2mse_extr_functions IMPLEMENTATION.
  METHOD select_by_packages.

    g_selected_function_groups = _select_from_tadir( packages ).
    _check_existence( CHANGING function_groups = g_selected_function_groups ).
    " TBD 17.03.2017

  ENDMETHOD.


  METHOD _select_from_tadir.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF i_packages IS NOT INITIAL.
      SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
        pgmid = 'R3TR' AND
        devclass = i_packages-package AND
        object = tadir_function_group.

    ENDIF.
  ENDMETHOD.

  METHOD add_and_sort.

    DATA tadirline TYPE ty_tadir_test.

    DATA function_group TYPE ty_function_group.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR function_group.

      function_group-fugr_program = _convert_fugr_2_progname( tadirline ).
      function_group-devclass = tadirline-devclass.

      INSERT function_group INTO TABLE c_selected_tables .

    ENDLOOP.

    SORT c_selected_tables BY fugr_program.

  ENDMETHOD.


  METHOD _convert_fugr_2_progname.

    " convert_fugr_2_progname
    progname = |SAPL| && |{ tadirline-obj_name }|.

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

ENDCLASS.
