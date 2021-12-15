CLASS z2mse_somix_data DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO z2mse_model .
    "! Returns the ID for a given data. May use also a grouping the data is contained.
    "! Returns 0 if the attribute is not known
    "! @parameter grouping_name_group | the namegroup of the grouping the code is contained in
    "! @parameter grouping | a grouping the code is contained in
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

ENDCLASS.
