CLASS z2mse_somix_parentchild DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_entity
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add
      RETURNING VALUE(id) TYPE i.
    METHODS set_parent_child_relation
      IMPORTING
        element_id TYPE i
        parent_id  TYPE i
        child_id   TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_parent_child_id,
             parent_id TYPE i,
             child_id  TYPE i,
           END OF  ty_parent_child_id.
    DATA: g_parent_child_ids TYPE HASHED TABLE OF ty_parent_child_id WITH UNIQUE KEY parent_id child_id.
ENDCLASS.



CLASS z2mse_somix_parentchild IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.ParentChild'.
  ENDMETHOD.

  METHOD add.
    g_model->add_entity( EXPORTING elementname               = g_elementname
                                   is_named_entity           = abap_false
                                   can_be_referenced_by_name = abap_false
                         IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.

  METHOD set_parent_child_relation.

    DATA ls_parent_id LIKE LINE OF g_parent_child_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_parent_id.
    ls_parent_id-parent_id = parent_id.
    ls_parent_id-child_id = child_id.
    INSERT ls_parent_id INTO TABLE g_parent_child_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'parent'
                                            reference_id   = parent_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'child'
                                            reference_id   = child_id ).
  ENDMETHOD.

ENDCLASS.
