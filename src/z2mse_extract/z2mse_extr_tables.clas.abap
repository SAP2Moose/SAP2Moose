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
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.
    TYPES: BEGIN OF ty_dd02l_test,
             tabname TYPE tabname,
           END OF TY_DD02L_TEST.
    TYPES ty_t_dd02l_test TYPE HASHED TABLE OF ty_dd02l_test WITH UNIQUE KEY tabname.
    METHODS constructor
      IMPORTING
        !tadir_test TYPE ty_t_tadir_test OPTIONAL
         dd02l_test TYPE ty_t_dd02l_test optional.
    METHODS select_tables_by_packages
      IMPORTING
        packages TYPE z2mse_extr_packages=>ty_packages.
    METHODS add_to_model
      IMPORTING
        sap_package   TYPE REF TO z2mse_sap_package
        sap_class     TYPE REF TO Z2MSE_sap_class
        sap_attribute TYPE REF TO Z2MSE_sap_attribute.
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
    METHODS _add_tables_to_model
      IMPORTING
        sap_package   TYPE REF TO z2mse_sap_package
        sap_class     TYPE REF TO Z2MSE_sap_class
        sap_attribute TYPE REF TO Z2MSE_sap_attribute
        tables        TYPE ty_tables.
ENDCLASS.



CLASS z2mse_extr_tables IMPLEMENTATION.
  METHOD constructor.
    IF tadir_test IS SUPPLIED or
       dd02l_test is SUPPLIED.
      g_tadir_test = tadir_test.
      g_dd02l_test = dd02l_test.
      g_is_test = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD select_tables_by_packages.

    g_selected_tables = _select_from_tadir( packages ).
    _check_existence( CHANGING db_tables = g_selected_tables ).

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

  METHOD add_to_model.

  ENDMETHOD.

  METHOD _add_tables_to_model.

*    DATA: table LIKE LINE OF g_selected_tables.
*    DATA last_id TYPE i.
*    LOOP AT tables INTO table.
*      sap_package->add( name = table-devclass ).
*
**      IF class-clstype EQ class_type.
**        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
**        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
**                                  name       = class-clsname
**                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalclass
**                        IMPORTING id         = last_id ).
**        sap_class->set_parent_package( element_id     = last_id
**                                       parent_package = class-devclass ).
**      ELSEIF class-clstype EQ interface_type.
**        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
**        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
**                                  name       = class-clsname
**                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalinterface
**                        IMPORTING id         = last_id ).
**        sap_class->set_parent_package( element_id     = last_id
**                                       parent_package = class-devclass ).
**        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
**        sap_class->is_interface( element_id = last_id ).
**      ELSE.
**        CONTINUE.
**      ENDIF.
*
*    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
*    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
*    sap_class->add( EXPORTING name_group             = 'ABAP_TABLE'
*                                  name                   = table-tabname
*                                  modifiers              = yrw1_mcextract_sap2=>modifier_dbtable
*                        IMPORTING id         = last_id ).
*    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
*    sap_attribute->add(
*      EXPORTING
*        class     =
*        attribute =
*    ).
*    g_famix_attribute->add( EXPORTING name = name IMPORTING id = dummy_attribute_id ).
*    g_famix_attribute->set_parent_type( EXPORTING element_id = dummy_attribute_id
**                                                  parent_id  = id ).
*  ENDMETHOD.
*  METHOD set_parent_package.
*    g_famix_class->set_parent_package( element_id = element_id parent_package = parent_package ).
*
*    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
