******************************************** Begin Include Z_MSE_ABAP *****************************
*The MIT License (MIT)
*
*Copyright (c) 2016 Rainer Winkler, CubeServ
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.

SELECTION-SCREEN BEGIN OF BLOCK bl_model_settings WITH FRAME TITLE text-100.

PARAMETERS p_down AS CHECKBOX DEFAULT 'X'.
"! Download model to file
DATA g_parameter_download_file TYPE bool.
g_parameter_download_file = p_down.
SELECTION-SCREEN END OF BLOCK bl_model_settings.

" Begin Model

CLASS cl_model DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF line_type,
             line TYPE string,
           END OF line_type.
    TYPES: lines_type TYPE STANDARD TABLE OF line_type.

    METHODS constructor.

    "! Add a named entity
    "! @parameter elementname | The name of the FAMIX Element. Like FAMIX.NamedEntity
    "! @parameter name_group | optional to handle cases where names may be duplicates
    "! @parameter is_named_entity | True if the entity has a name
    "! @parameter can_be_referenced_by_name | True if referencing by name is possible (For this the name has to be unique)
    "! @parameter name | the name of a FAMIX Entity that inherits from FAMIX.NamedEntity leave empty is is_named_entity is false
    "! @parameter exists_already_with_id | only if can_be_referenced_by_name true. Zero if it does not yet exist, otherwise filled with id
    "! @parameter processedid | the id in model either if just created or already existing
    METHODS add_entity
      IMPORTING elementname                   TYPE string
                name_group                    TYPE string DEFAULT ''
                is_named_entity               TYPE bool
                can_be_referenced_by_name     TYPE bool
                name                          TYPE string OPTIONAL
      EXPORTING VALUE(exists_already_with_id) TYPE i
      RETURNING VALUE(processed_id)           TYPE i.

    "! Generates a string with a valid MSE file
    METHODS make_mse
      EXPORTING
        mse_model TYPE lines_type.

    "! Generates an attribute of type string
    "! @parameter attribute_name | the name of the attribute
    "! @parameter string | The value of the attribute
    METHODS add_string
      IMPORTING
        attribute_name TYPE string
        string         TYPE string.

    "! Generates an attribute of type reference using a name
    "! @parameter attribute_name | the name of the attribute
    "! @parameter elementname | the element type of the reference
    "! @parameter name_of_reference | the reference
    METHODS add_reference
      IMPORTING
        attribute_name          TYPE string
        elementname             TYPE string
        name_group_of_reference TYPE string OPTIONAL
        name_of_reference       TYPE string.

    "! Generates an attribute of type reference using an id
    "! @parameter attribute_name | the name of the attribute
    "! @parameter reference_id | the id of the reference
    METHODS add_reference_by_id
      IMPORTING
        attribute_name TYPE string
        reference_id   TYPE i.

    METHODS add_boolean
      IMPORTING
        attribute_name TYPE string
        is_true        TYPE bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF element_in_model_type,
             id              TYPE i,
             is_named_entity TYPE bool,
             elementname     TYPE string,
           END OF element_in_model_type.
    "! A table with all Elements in the model
    DATA g_elements_in_model TYPE HASHED TABLE OF element_in_model_type WITH UNIQUE KEY id.

    TYPES: BEGIN OF named_entity_type,
             elementname TYPE string,
             name_group  TYPE string,
             xname       TYPE string,
             id          TYPE i,
           END OF named_entity_type.

    "! A table to find IDs using the names
    DATA g_named_entities TYPE HASHED TABLE OF named_entity_type WITH UNIQUE KEY elementname name_group xname.

    TYPES value_type TYPE c LENGTH 1.

    "! An attribute where a name is specified
    CONSTANTS string_value TYPE value_type VALUE 'S'.

    "! An attribute where a reference is specified
    CONSTANTS reference_value TYPE value_type VALUE 'R'.

    CONSTANTS boolean_value TYPE value_type VALUE 'B'.

    TYPES: BEGIN OF attribute_type,
             id             TYPE i,
             attribute_id   TYPE i,
             attribute_name TYPE string,
             value_type     TYPE value_type,
             string         TYPE string,
             reference      TYPE i,
             boolean        TYPE bool,
           END OF attribute_type.

    "! A table with all the attributes of an entity
    DATA g_attributes TYPE SORTED TABLE OF attribute_type WITH UNIQUE KEY id attribute_id.

    "! The ID of processed entity in the model
    DATA g_processed_id TYPE i.
    "! The ID of any attribute. Unique together with mv_id
    DATA g_attribute_id TYPE i.


ENDCLASS.

CLASS cl_model IMPLEMENTATION.

  METHOD constructor.
    g_processed_id = 0.
  ENDMETHOD.

  METHOD add_entity.

    IF can_be_referenced_by_name EQ true.

      READ TABLE g_named_entities ASSIGNING FIELD-SYMBOL(<ls_name>) WITH TABLE KEY elementname = elementname name_group = name_group xname = name.
      IF sy-subrc EQ ok.
        exists_already_with_id = <ls_name>-id.
        processed_id = <ls_name>-id.
        RETURN.
      ENDIF.

    ENDIF.

    ADD 1 TO g_processed_id.
    g_attribute_id = 0.

    IF can_be_referenced_by_name EQ true.
      g_named_entities = VALUE #( BASE g_named_entities ( elementname = elementname name_group = name_group xname = name id = g_processed_id ) ).
    ENDIF.

    g_elements_in_model = VALUE #( BASE g_elements_in_model ( id = g_processed_id
                                                              is_named_entity = is_named_entity
                                                              elementname = elementname ) ).

    IF is_named_entity EQ true.
      me->add_string( EXPORTING attribute_name = 'name' string = name ).
    ENDIF.

    processed_id = g_processed_id.

  ENDMETHOD.

  METHOD make_mse.

    " SAP_2_FAMIX_34      Allow to export the model in the .mse Moose format

    DATA: mse_model_line TYPE line_type.

    mse_model_line-line = |( |.

    SORT g_elements_in_model BY id.

    DATA(is_first) = true.

    LOOP AT g_elements_in_model ASSIGNING FIELD-SYMBOL(<element_in_model>).
      IF is_first EQ false.

        mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).
        mse_model_line = VALUE #( ).
      ENDIF.

      mse_model_line-line = mse_model_line-line && |(| && <element_in_model>-elementname.
      IF <element_in_model>-is_named_entity EQ true.

        mse_model_line-line = mse_model_line-line && | (id: | && <element_in_model>-id && | )|.
      ENDIF.

      LOOP AT g_attributes ASSIGNING FIELD-SYMBOL(<attribute>) WHERE id = <element_in_model>-id.

        mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).
        mse_model_line-line = |  (| && <attribute>-attribute_name.
        CASE <attribute>-value_type.
          WHEN string_value.

            mse_model_line-line = mse_model_line-line && | '| && <attribute>-string && |')|.

          WHEN reference_value.

            mse_model_line-line = mse_model_line-line && | (ref: | && <attribute>-reference && |))|.

          WHEN boolean_value.

            CASE <attribute>-boolean.
              WHEN true.
                mse_model_line-line = mse_model_line-line && | true)|.
              WHEN false.
                mse_model_line-line = mse_model_line-line && | false)|.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.

          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

      ENDLOOP.

      mse_model_line-line = mse_model_line-line && |)|.

      is_first = false.
    ENDLOOP.

    mse_model_line-line = mse_model_line-line && |)|.
    mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).

  ENDMETHOD.

  METHOD add_reference.

    READ TABLE g_named_entities ASSIGNING FIELD-SYMBOL(<named_entity>) WITH TABLE KEY elementname = elementname
                                                                                      name_group = name_group_of_reference
                                                                                      xname = name_of_reference.
    ASSERT sy-subrc EQ ok.
    ADD 1 TO g_attribute_id.
    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
                                                attribute_id   = g_attribute_id
                                                attribute_name = attribute_name
                                                value_type     = reference_value
                                                reference      = <named_entity>-id ) ).

  ENDMETHOD.

  METHOD add_reference_by_id.

    ADD 1 TO g_attribute_id.
    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
                                                attribute_id   = g_attribute_id
                                                attribute_name = attribute_name
                                                value_type     = reference_value
                                                reference      = reference_id ) ).

  ENDMETHOD.

  METHOD add_string.

    ADD 1 TO g_attribute_id.
    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
                                                attribute_id   = g_attribute_id
                                                attribute_name = attribute_name
                                                value_type     = string_value
                                                string         = string ) ).

  ENDMETHOD.

  METHOD add_boolean.
    ADD 1 TO g_attribute_id.
    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
                                                attribute_id   = g_attribute_id
                                                attribute_name = attribute_name
                                                value_type     = boolean_value
                                                boolean        = is_true ) ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_output_model DEFINITION.
  PUBLIC SECTION.
    METHODS make
      IMPORTING
        mse_model TYPE cl_model=>lines_type.
ENDCLASS.

CLASS cl_output_model IMPLEMENTATION.

  METHOD make.
    " Download the file

    DATA: filename    TYPE string,
          pathname    TYPE string,
          fullpath    TYPE string,
          user_action TYPE i.

    IF g_parameter_download_file EQ true.

      cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension = 'mse'
                                                  CHANGING  filename    = filename       " File Name to Save
                                                            path        = pathname       " Path to File
                                                            fullpath    = fullpath       " Path + File Name
                                                            user_action = user_action ). " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)

      IF user_action = cl_gui_frontend_services=>action_cancel.
        WRITE: / 'Canceled by user'.
      ELSE.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = fullpath
          TABLES
            data_tab = mse_model.

      ENDIF.

    ENDIF.

    LOOP AT mse_model ASSIGNING FIELD-SYMBOL(<mse_model_line>).
      WRITE: / <mse_model_line>-line.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
******************************************** End Include Z_MSE_ABAP *******************************