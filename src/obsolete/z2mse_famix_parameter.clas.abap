
CLASS z2mse_famix_parameter DEFINITION INHERITING FROM z2mse_famix_named_entity
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add REDEFINITION.
    "! Set the parent behavioural entity, either a method or a function
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_id | id of parent entity
    METHODS set_parent_behavioural_entity
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_id          TYPE i.
ENDCLASS.

CLASS z2mse_famix_parameter IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Parameter'.
  ENDMETHOD.

  METHOD set_parent_behavioural_entity.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'parentBehaviouralEntity'
                                            reference_id   = parent_id ).
  ENDMETHOD.

  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_false
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
