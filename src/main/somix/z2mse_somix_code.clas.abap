CLASS z2mse_somix_code DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO z2mse_model .
    "! Call method to store ID before add is used the next time for the same type of element
    METHODS add
      IMPORTING
        grouping_name_group           TYPE clike
        grouping                      TYPE clike
        code_name_group               TYPE clike
        code                          TYPE clike
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
                code_name_group     TYPE clike
                code                TYPE clike
      RETURNING VALUE(id)           TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_code_id,
             grouping_name_group TYPE string,
             grouping            TYPE string,
             code_name_group     TYPE string,
             code                TYPE string,
             id                  TYPE i,
           END OF ty_code_id.
    DATA: g_code_ids TYPE HASHED TABLE OF ty_code_id WITH UNIQUE KEY grouping_name_group grouping code_name_group code.
ENDCLASS.



CLASS z2mse_somix_code IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Code'.
  ENDMETHOD.

  METHOD get_id.
    FIELD-SYMBOLS <code_id> LIKE LINE OF g_code_ids.

    READ TABLE g_code_ids ASSIGNING <code_id> WITH TABLE KEY grouping_name_group = grouping_name_group
                                                             grouping = grouping
                                                             code_name_group = code_name_group
                                                             code = code.
    IF sy-subrc EQ 0. "OK
      id = <code_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    FIELD-SYMBOLS <code_id> LIKE LINE OF g_code_ids.

    READ TABLE g_code_ids ASSIGNING <code_id> WITH TABLE KEY grouping_name_group = grouping_name_group
                                                             grouping = grouping
                                                             code_name_group = code_name_group
                                                             code = code.

    IF sy-subrc EQ 0.

      id = <code_id>-id.

    ELSE.

      g_model->add_entity(
                 EXPORTING elementname = g_elementname
                           is_named_entity = abap_true
                           can_be_referenced_by_name = abap_false
                           name = code
                 IMPORTING processed_id = id ).

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'technicalType'
                                     string         = technical_type ).

    ENDIF.

    IF link_to_editor IS NOT INITIAL.

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'linkToEditor'
                                     string         = link_to_editor ).
    ENDIF.

    DATA ls_code_id LIKE LINE OF g_code_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_code_id.
    ls_code_id-id = id.
    ls_code_id-grouping_name_group = grouping_name_group.
    ls_code_id-grouping            = grouping.
    ls_code_id-code_name_group     = code_name_group.
    ls_code_id-code                = code.
    INSERT ls_code_id INTO TABLE g_code_ids.
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
