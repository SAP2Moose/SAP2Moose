

CLASS z2mse_famix_association DEFINITION INHERITING FROM z2mse_famix_sourced_entity ABSTRACT
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.
ENDCLASS.

CLASS z2mse_famix_association IMPLEMENTATION.

  METHOD add.
    g_model->add_entity( EXPORTING elementname               = g_elementname
                                        is_named_entity           = abap_false
                                        can_be_referenced_by_name = abap_false
                                        IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
