CLASS z2mse_extr_programs DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr_include
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_program_public,
             program TYPE progname,
           END OF ty_program_public.
    TYPES: ty_programs_public TYPE HASHED TABLE OF ty_program_public WITH UNIQUE KEY program.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.

    "! Call once to select all programs that are in a list of packages
    METHODS select_by_packages
      IMPORTING
        packages TYPE z2mse_extr_packages=>ty_packages.
    METHODS select_by_includes
      IMPORTING
        includes TYPE ty_includes_hashed.
    METHODS get_to_do_where_used
      RETURNING VALUE(programs) TYPE ty_programs_public.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_program,
             program  TYPE progname,
             devclass TYPE tadir-devclass,
             exists   TYPE abap_bool,
           END OF ty_program.
    TYPES: ty_programs TYPE HASHED TABLE OF ty_program WITH UNIQUE KEY program.
    CONSTANTS: tadir_program TYPE tadir-object VALUE 'PROG' ##NO_TEXT.
    DATA g_selected_programs TYPE ty_programs.
    DATA g_selected_programs_new TYPE ty_programs.
    METHODS _select_from_tadir
      IMPORTING
        i_packages               TYPE z2mse_extr_packages=>ty_packages
      RETURNING
        VALUE(selected_programs) TYPE ty_programs.
    METHODS add_and_sort
      IMPORTING
        i_tadirvalues     TYPE ty_t_tadir_test
      CHANGING
        c_selected_tables TYPE ty_programs.
    METHODS _check_existence
      CHANGING
        programs TYPE z2mse_extr_programs=>ty_programs.
ENDCLASS.



CLASS z2mse_extr_programs IMPLEMENTATION.


  METHOD add_and_sort.

    DATA tadirline TYPE ty_tadir_test.

    DATA program TYPE ty_program.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR program.

      program-program = tadirline-obj_name.
      program-devclass = tadirline-devclass.

      INSERT program INTO TABLE c_selected_tables .

    ENDLOOP.

    SORT c_selected_tables BY program.

  ENDMETHOD.


  METHOD select_by_packages.

    g_selected_programs = _select_from_tadir( packages ).
    _check_existence( CHANGING programs = g_selected_programs ).
    g_selected_programs_new = g_selected_programs.

  ENDMETHOD.

  METHOD _select_from_tadir.
    DATA: tadirline        TYPE ty_tadir_test,
          tadirvalues      TYPE ty_t_tadir_test,
          selected_program TYPE ty_program.

    IF i_packages IS NOT INITIAL.
      SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
        pgmid = 'R3TR' AND
        devclass = i_packages-package AND
        object = tadir_program.

      LOOP AT tadirvalues INTO tadirline.
        selected_program-program = tadirline-obj_name.
        selected_program-devclass = tadirline-devclass.
        INSERT selected_program INTO TABLE selected_programs.
        ASSERT sy-subrc EQ 0.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD _check_existence.

    TYPES: BEGIN OF ty_existing,
             name TYPE progname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <program> LIKE LINE OF programs.

    IF programs IS NOT INITIAL.
      SELECT name FROM progdir INTO TABLE table FOR ALL ENTRIES IN programs WHERE name = programs-program.
    ENDIF.

    LOOP AT table INTO line.

      READ TABLE programs ASSIGNING <program> WITH KEY program = line-name.
      IF sy-subrc EQ 0.
        <program>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE programs WHERE exists = abap_false.

  ENDMETHOD.
  METHOD get_to_do_where_used.
    DATA: line        LIKE LINE OF g_selected_programs_new,
          line_public TYPE ty_program_public.
    LOOP AT g_selected_programs_new INTO line.
      CLEAR line_public.
      line_public-program = line-program.
      INSERT line_public INTO TABLE programs.
    ENDLOOP.

    CLEAR g_selected_programs_new.
*    DATA: line        LIKE LINE OF g_selected_tables_new,
*          line_public TYPE ty_table_public.
*    LOOP AT g_selected_tables_new INTO line.
*      CLEAR line_public.
*      line_public-tabname = line-tabname.
*      INSERT line_public INTO TABLE tables.
*    ENDLOOP.
*
*    CLEAR g_selected_tables_new.
  ENDMETHOD.

  METHOD select_by_includes.

    DATA: programs TYPE ty_programs,
          program  TYPE ty_program,
          include  TYPE ty_include.

    LOOP AT includes INTO include.

      program-program = include-include.
      INSERT program INTO TABLE programs.

      _check_existence( CHANGING programs = programs ).

      LOOP AT programs INTO program.
        READ TABLE g_selected_programs TRANSPORTING NO FIELDS WITH KEY program = program-program.
        IF sy-subrc <> 0.
          INSERT program INTO TABLE g_selected_programs.
          INSERT program INTO TABLE g_selected_programs_new.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
