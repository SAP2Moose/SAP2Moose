CLASS z2mse_somix_code DEFINITION
  PUBLIC
  INHERITING FROM z2mse_somix_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO z2mse_model .
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
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter grouping_name_group | the name group of the grouping
    "! @parameter grouping | the grouping
    "! @parameter code_name_group | the name group of the code
    "! @parameter code | the ID of the element
    METHODS store_id
      IMPORTING grouping_name_group TYPE clike
                grouping            TYPE clike
                code_name_group     TYPE clike
                code                TYPE clike.
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

  METHOD store_id.
    DATA ls_code_id LIKE LINE OF g_code_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_code_id.
    ls_code_id-id = g_last_used_id.
    ls_code_id-grouping_name_group = grouping_name_group.
    ls_code_id-grouping            = grouping.
    ls_code_id-code_name_group     = code_name_group.
    ls_code_id-code                = code.
    INSERT ls_code_id INTO TABLE g_code_ids.
  ENDMETHOD.

ENDCLASS.
