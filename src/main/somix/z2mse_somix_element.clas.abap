CLASS z2mse_somix_element DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_entity
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add
      IMPORTING
        !name_group                   TYPE clike OPTIONAL
        !name                         TYPE clike
        !technical_type               TYPE clike
        !link_to_editor               TYPE clike
      EXPORTING
        VALUE(exists_already_with_id) TYPE i
        VALUE(id)                     TYPE i .

  PROTECTED SECTION.

    DATA name TYPE string .
    DATA title TYPE string .
    DATA technical_type TYPE string .
    DATA link_to_editor TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_somix_element IMPLEMENTATION.


  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).

    g_model->add_string( EXPORTING element_id     = id
                                   attribute_name = 'technicalType'
                                   string         = technical_type ).

    IF link_to_editor IS NOT INITIAL.

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'technicalType'
                                     string         = link_to_editor ).
    ENDIF.

    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.
