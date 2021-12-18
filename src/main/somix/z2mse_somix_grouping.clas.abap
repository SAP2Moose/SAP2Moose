CLASS z2mse_somix_grouping DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_element
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO z2mse_model .
    "! Call method to store ID before add is used the next time for the same type of element
    METHODS add
      IMPORTING
        !name_group                   TYPE clike
        !name                         TYPE clike
        !technical_type               TYPE clike
        !link_to_editor               TYPE clike
      EXPORTING
        VALUE(exists_already_with_id) TYPE i
        VALUE(id)                     TYPE i .
    "! Returns the ID for a given code. May use a grouping it is contained in.
    "! Returns 0 if the data is not known
    "! @parameter grouping_name_group | the name group of the grouping
    "! @parameter grouping | the grouping
    "! @parameter code_name_group | the name group of the code
    "! @parameter code | the ID of the element
    METHODS get_id
      IMPORTING grouping_name_group TYPE clike
                grouping            TYPE clike
      RETURNING VALUE(id)           TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_grouping_id,
             grouping_name_group TYPE string,
             grouping            TYPE string,
             id                  TYPE i,
           END OF ty_grouping_id.
    DATA: g_grouping_ids TYPE HASHED TABLE OF ty_grouping_id WITH UNIQUE KEY grouping_name_group grouping.
ENDCLASS.



CLASS z2mse_somix_grouping IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Grouping'.
  ENDMETHOD.
  METHOD get_id.
    FIELD-SYMBOLS <grouping_id> LIKE LINE OF g_grouping_ids.

    READ TABLE g_grouping_ids ASSIGNING <grouping_id> WITH TABLE KEY grouping_name_group = grouping_name_group
                                                                     grouping = grouping.
    IF sy-subrc EQ 0. "OK
      id = <grouping_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

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
                                     attribute_name = 'linkToEditor'
                                     string         = link_to_editor ).
    ENDIF.

    DATA ls_grouping_id LIKE LINE OF g_grouping_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_grouping_id.
    ls_grouping_id-id                  = id.
    ls_grouping_id-grouping_name_group = name_group.
    ls_grouping_id-grouping            = name.
    INSERT ls_grouping_id INTO TABLE g_grouping_ids.

    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
