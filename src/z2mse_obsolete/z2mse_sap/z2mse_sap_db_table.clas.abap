

CLASS z2mse_sap_db_table DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    "! Add global Database table
    METHODS add
      IMPORTING name                          TYPE clike
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i
                VALUE(dummy_attribute_id)     TYPE i.
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.
  PRIVATE SECTION.
    CONSTANTS modifier_dbtable TYPE string VALUE 'DBTable' ##NO_TEXT.
    DATA: g_famix_class     TYPE REF TO Z2MSE_famix_class,
          g_famix_attribute TYPE REF TO Z2MSE_famix_attribute.
ENDCLASS.

CLASS z2mse_sap_db_table IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_class EXPORTING model = model.
    CREATE OBJECT g_famix_attribute EXPORTING model = model.
  ENDMETHOD.
  METHOD add.
    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
    g_famix_class->add( EXPORTING name_group             = ''
                                  name                   = name
                                  modifiers              = modifier_dbtable
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id = id ).
    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
    g_famix_attribute->add( EXPORTING name = name IMPORTING id = dummy_attribute_id ).
    g_famix_attribute->set_parent_type( EXPORTING element_id = dummy_attribute_id
                                                  parent_id  = id ).
  ENDMETHOD.
  METHOD set_parent_package.
    g_famix_class->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.
ENDCLASS.
