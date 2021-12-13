CLASS z2mse_somix_access DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_coupling
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS set_accessor_accessed_relation
      IMPORTING
        element_id   TYPE i
        accessor_id  TYPE i
        accessed_id  TYPE i
        is_write     TYPE abap_bool
        is_read      TYPE abap_bool
        is_dependent TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_accessor_accessed_id,
             accessor_id TYPE i,
             accessed_id TYPE i,
           END OF  ty_accessor_accessed_id.
    DATA: g_accessor_accessed_ids TYPE HASHED TABLE OF ty_accessor_accessed_id WITH UNIQUE KEY accessor_id accessed_id.
ENDCLASS.



CLASS z2mse_somix_access IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Access'.
  ENDMETHOD.
  METHOD set_accessor_accessed_relation.

    DATA ls_accessor_id LIKE LINE OF g_accessor_accessed_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_accessor_id.
    ls_accessor_id-accessor_id = accessor_id.
    ls_accessor_id-accessed_id = accessed_id.
    INSERT ls_accessor_id INTO TABLE g_accessor_accessed_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'accessed'
                                            reference_id   = accessed_id ).
    IF is_write EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isWrite'
                                      is_true            = 'X' ).
    ENDIF.
    IF is_read EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isRead'
                                      is_true            = 'X' ). " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
    ENDIF.
    IF is_dependent EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isDependent'
                                      is_true            = 'X' ). " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
