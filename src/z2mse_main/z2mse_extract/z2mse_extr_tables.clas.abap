"! Extract informations on SAP database tables
CLASS z2mse_extr_tables DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test .
    TYPES:
      ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name .
    TYPES:
      BEGIN OF ty_dd02l_test,
        tabname TYPE tabname,
      END OF ty_dd02l_test .
    TYPES:
      ty_t_dd02l_test TYPE HASHED TABLE OF ty_dd02l_test WITH UNIQUE KEY tabname .
    TYPES:
      BEGIN OF ty_table_public,
        tabname TYPE tabname,
      END OF ty_table_public .
    TYPES:
      ty_tables_public TYPE HASHED TABLE OF ty_table_public WITH UNIQUE KEY tabname .

    METHODS constructor
      IMPORTING
        !tadir_test TYPE ty_t_tadir_test OPTIONAL
        !dd02l_test TYPE ty_t_dd02l_test OPTIONAL .
    "! Call once to select all tables that are in a list of packages
    METHODS select_tables_by_packages
      IMPORTING
        !packages TYPE z2mse_extr_packages=>ty_packages .
    "! Returns tables. Returns these tables only once
    METHODS get_tables_to_do_where_used
      RETURNING
        VALUE(tables) TYPE ty_tables_public .
    METHODS add_to_model
      IMPORTING
        !famix_package   TYPE REF TO z2mse_famix_package
        !famix_class     TYPE REF TO z2mse_famix_class
        !famix_attribute TYPE REF TO z2mse_famix_attribute .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_table,
             tabname  TYPE tabname,
             devclass TYPE tadir-devclass,
             exists   TYPE abap_bool,
           END OF ty_table.
    TYPES: ty_tables TYPE HASHED TABLE OF ty_table WITH UNIQUE KEY tabname.
    CONSTANTS: tadir_table TYPE tadir-object VALUE 'TABL' ##NO_TEXT.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_tadir_test TYPE ty_t_tadir_test.
    "! Filled during tests
    DATA g_dd02l_test TYPE ty_t_dd02l_test.
    "! A list of all primarily selected and existing tables. Only if not yet transfered to where used analysis
    DATA g_selected_tables_new TYPE z2mse_extr_tables=>ty_tables.
    "! A list of all primarily selected and existing tables
    DATA g_selected_tables TYPE z2mse_extr_tables=>ty_tables.
    METHODS _select_from_tadir
      IMPORTING
        i_packages             TYPE z2mse_extr_packages=>ty_packages
      RETURNING
        VALUE(selected_tables) TYPE z2mse_extr_tables=>ty_tables.
    METHODS add_and_sort_to_tables_table
      IMPORTING
        i_tadirvalues     TYPE z2mse_extr_tables=>ty_t_tadir_test
      CHANGING
        c_selected_tables TYPE z2mse_extr_tables=>ty_tables.
    "! Checks whether a class exists. There can be TADIR entries for not existing classes.
    METHODS _check_existence
      CHANGING db_tables TYPE z2mse_extr_tables=>ty_tables.

ENDCLASS.



CLASS Z2MSE_EXTR_TABLES IMPLEMENTATION.


  METHOD add_and_sort_to_tables_table.

    DATA tadirline TYPE z2mse_extr_classes=>ty_tadir_test.

    "Add and sort to classes table
    DATA table_line TYPE ty_table.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR table_line.

      table_line-tabname = tadirline-obj_name.
      table_line-devclass = tadirline-devclass.

      INSERT table_line INTO TABLE c_selected_tables .

    ENDLOOP.

    SORT c_selected_tables BY tabname.

  ENDMETHOD.


  METHOD add_to_model.

    DATA: table LIKE LINE OF g_selected_tables.
    DATA last_id TYPE i.
    LOOP AT g_selected_tables INTO table.
      famix_package->add( name = table-devclass ).

      " SAP_2_FAMIX_54        Map database tables to FAMIX Class
      " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
      famix_class->add( EXPORTING name_group             = 'ABAP_TABLE'
                                  name                   = table-tabname
                                  modifiers              = z2mse_extract_sap2=>modifier_dbtable
                          IMPORTING id         = last_id ).
      famix_class->set_parent_package( EXPORTING element_id         = last_id
                                                 parent_package     = table-devclass ).
      DATA dummy_attribute_id TYPE i.
      " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
      famix_attribute->add( EXPORTING name                   = table-tabname
                            IMPORTING id                     = dummy_attribute_id ).

      famix_attribute->set_parent_type( EXPORTING element_id         = dummy_attribute_id
                                                  parent_id          = last_id ).

      famix_attribute->store_id( EXPORTING class     = table-tabname
                                           attribute = table-tabname ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    IF tadir_test IS SUPPLIED OR
       dd02l_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_dd02l_test = dd02l_test.
      g_is_test = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_tables_to_do_where_used.
    DATA: line        LIKE LINE OF g_selected_tables_new,
          line_public TYPE ty_table_public.
    LOOP AT g_selected_tables_new INTO line.
      CLEAR line_public.
      line_public-tabname = line-tabname.
      INSERT line_public INTO TABLE tables.
    ENDLOOP.

    CLEAR g_selected_tables_new.

  ENDMETHOD.


  METHOD select_tables_by_packages.

    g_selected_tables = _select_from_tadir( packages ).
    _check_existence( CHANGING db_tables = g_selected_tables ).
    g_selected_tables_new = g_selected_tables.

  ENDMETHOD.





  METHOD _check_existence.

    TYPES: BEGIN OF ty_existing,
             tabname TYPE tabname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <db_table> LIKE LINE OF db_tables.

*    " Build helper table with correct type for select
*    LOOP AT g_selected_tadir INTO selected_tadir.
*      line-clsname = selected_tadir-obj_name.
*      INSERT line INTO TABLE check_table.
*    ENDLOOP.

    " Select from database
    IF g_is_test EQ abap_false.

      IF db_tables IS NOT INITIAL.
        SELECT tabname FROM dd02l INTO TABLE table FOR ALL ENTRIES IN db_tables WHERE tabname = db_tables-tabname.
      ENDIF.

    ELSE.

      DATA dd02l_line TYPE ty_dd02l_test.

      LOOP AT g_dd02l_test INTO dd02l_line.
        READ TABLE db_tables TRANSPORTING NO FIELDS WITH KEY tabname = dd02l_line-tabname.
        IF sy-subrc EQ 0.
          line-tabname = dd02l_line-tabname.
          INSERT line INTO TABLE table.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " Mark as existing
    LOOP AT table INTO line.

      READ TABLE db_tables ASSIGNING <db_table> WITH KEY tabname = line-tabname.
      IF sy-subrc EQ 0.
        <db_table>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE db_tables WHERE exists = abap_false.

  ENDMETHOD.


  METHOD _select_from_tadir.

    " Select from TADIR
    CLEAR selected_tables.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF i_packages IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
          pgmid = 'R3TR' AND
          devclass = i_packages-package AND
          object = tadir_table.

      ENDIF.
    ELSE.
      DATA package LIKE LINE OF i_packages.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT i_packages INTO package.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          object = tadir_table AND
          devclass = package-package.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_tables_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_tables = selected_tables ).

  ENDMETHOD.
ENDCLASS.
