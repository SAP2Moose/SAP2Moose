CLASS z2mse_somix_data DEFINITION
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
        data_name_group               TYPE clike
        data                          TYPE clike
        !technical_type               TYPE clike
        !link_to_editor               TYPE clike
      EXPORTING
        VALUE(exists_already_with_id) TYPE i
        VALUE(id)                     TYPE i
      CHANGING
        unique_name                   TYPE clike.
    "! Returns the ID for a given data. May use also a grouping the data is contained.
    "! Returns 0 if the attribute is not known
    "! @parameter grouping_name_group | the namegroup of the grouping the data is contained in
    "! @parameter grouping | a grouping the data is contained in
    "! @parameter data_name_group | the the namegroup of the data
    "! @parameter data | the data name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                grouping_name_group TYPE clike
                grouping            TYPE clike
                data_name_group     TYPE clike
                data                TYPE clike
      RETURNING VALUE(id)           TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF data_id_type,
             grouping_name_group TYPE string,
             grouping            TYPE string,
             data_name_group     TYPE string,
             data                TYPE string,
             id                  TYPE i,
           END OF data_id_type.
    DATA: g_data_ids TYPE HASHED TABLE OF data_id_type WITH UNIQUE KEY grouping_name_group data_name_group grouping data.

    DATA is_persistent TYPE abap_bool . ##TODO " Add attribute for this
ENDCLASS.



CLASS z2mse_somix_data IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Data'.
  ENDMETHOD.

  METHOD get_id.
    FIELD-SYMBOLS <data_id> LIKE LINE OF g_data_ids.

    READ TABLE g_data_ids ASSIGNING <data_id> WITH TABLE KEY grouping_name_group = grouping_name_group grouping = grouping data_name_group = data_name_group data = data.
    IF sy-subrc EQ 0. "OK
      id = <data_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    FIELD-SYMBOLS <data_id> LIKE LINE OF g_data_ids.

    READ TABLE g_data_ids ASSIGNING <data_id> WITH TABLE KEY grouping_name_group = grouping_name_group grouping = grouping data_name_group = data_name_group data = data.
    IF sy-subrc EQ 0. "OK
      id = <data_id>-id.

    ELSE.

      g_model->add_entity(
                 EXPORTING elementname = g_elementname
                           is_named_entity = abap_true
                           can_be_referenced_by_name = abap_false
                           name = data
                 IMPORTING processed_id = id ).

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'technicalType'
                                     string         = technical_type ).

    ENDIF.

    ASSERT unique_name IS NOT INITIAL.
    TRANSLATE unique_name TO LOWER CASE. " To be compatible with specification. Not case sensitive names are here in lower case.
    g_model->add_string( EXPORTING element_id     = id
                                   attribute_name = 'uniqueName'
                                   string         = unique_name ).

    IF link_to_editor IS NOT INITIAL.

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'linkToEditor'
                                     string         = link_to_editor ).
    ENDIF.

    DATA ls_data_id LIKE LINE OF g_data_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_data_id.
    ls_data_id-id = id.
    ls_data_id-grouping_name_group = grouping_name_group.
    ls_data_id-grouping = grouping.
    ls_data_id-data_name_group = data_name_group.
    ls_data_id-data = data.
    INSERT ls_data_id INTO TABLE g_data_ids.
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.
