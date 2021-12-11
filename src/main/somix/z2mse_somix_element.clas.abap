class Z2MSE_SOMIX_ELEMENT definition
  public
  inheriting from Z2MSE_SOMIX_ENTITY
  abstract
  create public .

public section.

  methods ADD
    importing
      !NAME_GROUP type CLIKE optional
      !NAME type CLIKE
      !TECHNICAL_TYPE type CLIKE
    exporting
      value(EXISTS_ALREADY_WITH_ID) type I
      value(ID) type I .
  PROTECTED SECTION.

    DATA name TYPE string .
    DATA title TYPE string .
    DATA technical_type TYPE string .
    DATA link_to_editor TYPE string .
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_SOMIX_ELEMENT IMPLEMENTATION.


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

    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.
