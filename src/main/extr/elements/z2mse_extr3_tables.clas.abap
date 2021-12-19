"! I describe elements of type table
CLASS z2mse_extr3_tables DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA database_schema  TYPE db_schema.
    METHODS constructor
      IMPORTING
        !i_element_manager TYPE REF TO z2mse_extr3_element_manager .
    CLASS-METHODS clear .
    CLASS-METHODS get_instance
      IMPORTING
        !i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance)  TYPE REF TO z2mse_extr3_tables .
    METHODS add
      IMPORTING
        !table                TYPE string
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type .
    METHODS table_name
      IMPORTING
        !i_element_id   TYPE i
      RETURNING
        VALUE(r_result) TYPE tabname .

    METHODS collect_infos
        REDEFINITION .
    METHODS make_model
        REDEFINITION .
    METHODS name
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_tables.
    TYPES: BEGIN OF element_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             tabname    TYPE tabname,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_tabname TYPE HASHED TABLE OF element_type WITH UNIQUE KEY tabname.
ENDCLASS.



CLASS z2mse_extr3_tables IMPLEMENTATION.


  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_tabname INTO element WITH KEY tabname = table.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does table exists?
      DATA found_tabname TYPE tabname.
      DATA found_tabclass TYPE tabclass.
      TEST-SEAM dd02l.
        " No blank between ( and found... to be 7.02 compatible
        SELECT tabname tabclass FROM dd02l INTO (found_tabname, found_tabclass ) WHERE tabname = table.

        ENDSELECT.
      END-TEST-SEAM.
      IF     found_tabname IS NOT INITIAL
         AND found_tabclass <> 'INTTAB'  " Not structures
         AND found_tabclass <> 'APPEND'. " Not append structures
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_true ).
        element-element_id = new_element_id.
        element-tabname = table.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_tabname.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD clear.
    CLEAR instance.
  ENDMETHOD.


  METHOD collect_infos.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( i_element_manager = i_element_manager ).

    " Determine database schema to group database tables accordingly.
    DATA: dbinfo TYPE dbrelinfo.
    CALL FUNCTION 'DB_DBRELINFO' IMPORTING dbinfo = dbinfo.
    database_schema = dbinfo-dbschema.

  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = table_type.
    r_instance = instance.
  ENDMETHOD.


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA last_id TYPE i.
*      famix_package->add( name = table-devclass ).

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'

    DATA database_table_id TYPE i.

    IF element_manager->use_somix EQ 'X'.

      element_manager->somix_data->add( EXPORTING grouping_name_group = ng_database_schema
                                                  grouping            = database_schema
                                                  data_name_group     = ng_sap_table
                                                  data           = element-tabname
                                                  technical_type = z2mse_extract3=>modifier_dbtable
                                                  link_to_editor = ''
                                        IMPORTING id                     = database_table_id ).

      DATA association TYPE z2mse_extr3_element_manager=>association_type.
      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = z2mse_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO z2mse_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        DATA: package_id TYPE i.

        element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                        grouping               = package->devclass( i_element_id = association-element_id2 )
                                                        technical_type         = z2mse_extract3=>techtype_abappackage
                                                        link_to_editor         = ''
                                              IMPORTING id                     = package_id ).

        element_manager->somix_parentchild->add( EXPORTING parent_id = package_id
                                                           child_id  = database_table_id ).

      ENDLOOP.

    ELSE. " SOMIX

      element_manager->famix_class->add( EXPORTING name_group             = 'ABAP_TABLE'
                                                   name                   = element-tabname
                                                   modifiers              = z2mse_extract3=>modifier_dbtable
                                         IMPORTING id         = last_id ).

      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = z2mse_extr3_association=>parent_package_ass.

        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id ##TODO " Determine the package for a new table
                                                                    parent_package     = package->devclass( i_element_id = association-element_id2 )
                                                                    parent_package_name_group = ng_abap_package ).

      ENDLOOP.

    ENDIF. " SOMIX
    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table

    IF element_manager->use_somix EQ 'X'.

    ELSE. "SOMIX

      element_manager->famix_attribute->add( EXPORTING name                   = element-tabname
                                             IMPORTING id                     = database_table_id ).

      element_manager->famix_attribute->set_parent_type( EXPORTING element_id         = database_table_id
                                                  parent_id          = last_id ).

      element_manager->famix_attribute->store_id( EXPORTING name_group = ng_sap_table
                                                            class     = element-tabname
                                                            attribute = element-tabname ).

    ENDIF. "SOMIX

  ENDMETHOD.


  METHOD name.

    DATA: table TYPE tabname.

    table = table_name( i_element_id = element_id ).

    element_type = |ABAPDatabaseTable|.
    parent_name = ||.
    name = table.

  ENDMETHOD.


  METHOD table_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-tabname.

  ENDMETHOD.
ENDCLASS.
