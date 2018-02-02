

CLASS z2mse_famix_custom_source_lng DEFINITION INHERITING FROM z2mse_famix_entity
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING name                          TYPE clike
                          name_group                    TYPE clike
                EXPORTING VALUE(exists_already_with_id) TYPE i
                          VALUE(id)                     TYPE i.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
ENDCLASS.

CLASS z2mse_famix_custom_source_lng IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.

  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name = name
                                        name_group = name_group
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
