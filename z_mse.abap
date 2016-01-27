******************************************** Begin Include Z_MSE_ABAP ******************************************************************************************
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

PARAMETERS p_down AS CHECKBOX DEFAULT ' '.
"! Download model to file
data l_parameter_download_file type abap_bool.
l_parameter_download_file = p_down.

" Begin Model
"! To not compare sy-subrc to zero, but more readable to ok
CONSTANTS ok TYPE i VALUE 0.
"! Redefines abap_bool to simplify coding (Not always reading abap_...)
TYPES bool TYPE abap_bool.
CONSTANTS:
  "! Redefines abap_true to simplify coding (Not always reading abap_...)
  true  TYPE bool VALUE abap_true,
  "! Redefines abap_false to simplify coding (Not always reading abap_...)
  false TYPE bool VALUE abap_false.



CLASS cl_model DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_line,
             line TYPE string,
           END OF ty_line.
    TYPES: ty_lines TYPE STANDARD TABLE OF ty_line.

    METHODS constructor.

    "! Add a named entity
    "! @parameter i_elementname | The name of the FAMIX Element. Like FAMIX.NamedEntity
    "! @parameter i_name_group | optional to handle cases where names may be duplicates
    "! @parameter i_is_named_entity | True if the entity has a name
    "! @parameter i_can_be_referenced_by_name | True if referencing by name is possible (For this the name has to be unique)
    "! @parameter i_name | the name of a FAMIX Entity that inherits from FAMIX.NamedEntity leave empty is i_is_named_entity is false
    "! @parameter e_exists_already_with_id | only if i_can_be_referenced_by_name true. Zero if it does not yet exist, otherwise filled with id
    "! @parameter r_id | the id in model either if just created or already existing
    METHODS add_entity
      IMPORTING i_elementname                   TYPE string
                i_name_group                    TYPE string DEFAULT ''
                i_is_named_entity               TYPE bool
                i_can_be_referenced_by_name     TYPE bool
                i_name                          TYPE string OPTIONAL
      EXPORTING VALUE(e_exists_already_with_id) TYPE i
      RETURNING VALUE(r_id)                     TYPE i.

    "! Generates a string with a valid MSE file
    METHODS make_mse
      EXPORTING
        et_mse TYPE ty_lines.

    "! Generates an attribute of type string
    "! @parameter i_attribute_name | the name of the attribute
    "! @parameter i_string | The value of the attribute
    METHODS add_string
      IMPORTING
        i_attribute_name TYPE string
        i_string         TYPE string.

    "! Generates an attribute of type reference using a name
    "! @parameter i_attribute_name | the name of the attribute
    "! @parameter i_elementname | the element type of the reference
    "! @parameter i_name_of_reference | the reference
    METHODS add_reference
      IMPORTING
        i_attribute_name          TYPE string
        i_elementname             TYPE string
        i_name_group_of_reference TYPE string OPTIONAL
        i_name_of_reference       TYPE string.

    "! Generates an attribute of type reference using an id
    "! @parameter i_attribute_name | the name of the attribute
    "! @parameter i_reference_id | the id of the reference
    METHODS add_reference_by_id
      IMPORTING
        i_attribute_name TYPE string
        i_reference_id   TYPE i.

    METHODS add_boolean
      IMPORTING
        i_attribute_name TYPE string
        i_is_true        TYPE bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_id,
             id              TYPE i,
             is_named_entity TYPE bool,
             elementname     TYPE string,
           END OF ty_id.
    "! A table with all Elements in the model
    DATA mt_ids TYPE HASHED TABLE OF ty_id WITH UNIQUE KEY id.

    TYPES: BEGIN OF ty_named_entities,
             elementname TYPE string,
             name_group  TYPE string,
             xname       TYPE string,
             id          TYPE i,
           END OF ty_named_entities.

    "! A table to find IDs using the names
    DATA mt_names TYPE HASHED TABLE OF ty_named_entities WITH UNIQUE KEY elementname name_group xname.

    TYPES ty_value_type TYPE c LENGTH 1.

    "! An attribute where a name is specified
    CONSTANTS c_string_type TYPE ty_value_type VALUE 'S'.

    "! An attribute where a reference is specified
    CONSTANTS c_reference_type TYPE ty_value_type VALUE 'R'.

    CONSTANTS c_boolean_type TYPE ty_value_type VALUE 'B'.

    TYPES: BEGIN OF ty_attributes,
             id             TYPE i,
             attribute_id   TYPE i,
             attribute_name TYPE string,
             value_type     TYPE ty_value_type,
             string         TYPE string,
             reference      TYPE i,
             boolean        TYPE bool,
           END OF ty_attributes.

    "! A table with all the attributes of an entity
    DATA mt_attributes TYPE SORTED TABLE OF ty_attributes WITH UNIQUE KEY id attribute_id.

    "! The ID of any entity in the model
    DATA mv_id TYPE i.
    "! The ID of any attribute. Unique together with mv_id
    DATA mv_attribute_id TYPE i.


ENDCLASS.

CLASS cl_model IMPLEMENTATION.

  METHOD constructor.
    mv_id = 0.
  ENDMETHOD.

  METHOD add_entity.

    IF i_can_be_referenced_by_name EQ true.

      READ TABLE mt_names ASSIGNING FIELD-SYMBOL(<ls_name>) WITH TABLE KEY elementname = i_elementname name_group = i_name_group xname = i_name.
      IF sy-subrc EQ ok.
        e_exists_already_with_id = <ls_name>-id.
        r_id = <ls_name>-id.
        RETURN.
      ENDIF.

    ENDIF.

    ADD 1 TO mv_id.
    mv_attribute_id = 0.

    IF i_can_be_referenced_by_name EQ true.
      mt_names = VALUE #( BASE mt_names ( elementname = i_elementname name_group = i_name_group xname = i_name id = mv_id ) ).
    ENDIF.

    mt_ids = VALUE #( BASE mt_ids ( id = mv_id is_named_entity = i_is_named_entity elementname = i_elementname ) ).

    IF i_is_named_entity EQ true.
      me->add_string( EXPORTING i_attribute_name = 'name' i_string = i_name ).
    ENDIF.

    r_id = mv_id.

  ENDMETHOD.

  METHOD make_mse.

    " SAP_2_FAMIX_34      Allow to export the model in the .mse Moose format

    DATA: ls_line TYPE ty_line.

    ls_line-line = |( |.

    SORT mt_ids BY id.

    DATA(lv_is_first) = true.

    LOOP AT mt_ids ASSIGNING FIELD-SYMBOL(<ls_id>).
      IF lv_is_first EQ false.

        et_mse = VALUE #( BASE et_mse ( ls_line ) ).
        ls_line = VALUE #( ).
      ENDIF.

      ls_line-line = ls_line-line && |(| && <ls_id>-elementname.
      IF <ls_id>-is_named_entity EQ true.

        ls_line-line = ls_line-line && | (id: | && <ls_id>-id && | )|.
      ENDIF.

      LOOP AT mt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>) WHERE id = <ls_id>-id.

        et_mse = VALUE #( BASE et_mse ( ls_line ) ).
        ls_line-line = |  (| && <ls_attribute>-attribute_name.
        CASE <ls_attribute>-value_type.
          WHEN c_string_type.

            ls_line-line = ls_line-line && | '| && <ls_attribute>-string && |')|.

          WHEN c_reference_type.

            ls_line-line = ls_line-line && | (ref: | && <ls_attribute>-reference && |))|.

          WHEN c_boolean_type.

            CASE <ls_attribute>-boolean.
              WHEN true.
                ls_line-line = ls_line-line && | true)|.
              WHEN false.
                ls_line-line = ls_line-line && | false)|.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.

          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

      ENDLOOP.

      ls_line-line = ls_line-line && |)|.

      lv_is_first = false.
    ENDLOOP.

    ls_line-line = ls_line-line && |)|.
    et_mse = VALUE #( BASE et_mse ( ls_line ) ).

  ENDMETHOD.

  METHOD add_reference.

    READ TABLE mt_names ASSIGNING FIELD-SYMBOL(<ls_name>) WITH TABLE KEY elementname = i_elementname
                                                                         name_group = i_name_group_of_reference
                                                                         xname = i_name_of_reference.
    ASSERT sy-subrc EQ ok.
    ADD 1 TO mv_attribute_id.
    mt_attributes = VALUE #( BASE mt_attributes ( id = mv_id
                                                  attribute_id = mv_attribute_id
                                                  attribute_name = i_attribute_name
                                                  value_type = c_reference_type
                                                  reference = <ls_name>-id ) ).

  ENDMETHOD.

  METHOD add_reference_by_id.

    ADD 1 TO mv_attribute_id.
    mt_attributes = VALUE #( BASE mt_attributes ( id = mv_id
                                                  attribute_id = mv_attribute_id
                                                  attribute_name = i_attribute_name
                                                  value_type = c_reference_type
                                                  reference = i_reference_id ) ).

  ENDMETHOD.

  METHOD add_string.

    ADD 1 TO mv_attribute_id.
    mt_attributes = VALUE #( BASE mt_attributes ( id = mv_id
                                                  attribute_id = mv_attribute_id
                                                  attribute_name = i_attribute_name
                                                  value_type = c_string_type
                                                  string = i_string ) ).

  ENDMETHOD.

  METHOD add_boolean.
    ADD 1 TO mv_attribute_id.
    mt_attributes = VALUE #( BASE mt_attributes ( id = mv_id
                                                  attribute_id = mv_attribute_id
                                                  attribute_name = i_attribute_name
                                                  value_type = c_boolean_type
                                                  boolean = i_is_true ) ).
  ENDMETHOD.



ENDCLASS.

CLASS cl_output_model DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS make
      IMPORTING
        it_model TYPE cl_model=>ty_lines.
ENDCLASS.

CLASS cl_output_model IMPLEMENTATION.

  METHOD make.
    " Download the file

    DATA: filename    TYPE string,
          pathname    TYPE string,
          fullpath    TYPE string,
          user_action TYPE i.

    IF l_parameter_download_file EQ true.

      cl_gui_frontend_services=>file_save_dialog( CHANGING filename    = filename       " File Name to Save
                                                           path        = pathname       " Path to File
                                                           fullpath    = fullpath       " Path + File Name
                                                           user_action = user_action ). " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)

      IF user_action = cl_gui_frontend_services=>action_cancel.
        WRITE: / 'Canceled by user'.
      ELSE.

        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename = fullpath  " Name of file
          TABLES
            data_tab = it_model.    " Transfer table

      ENDIF.

    ENDIF.

    LOOP AT it_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      WRITE: / <ls_model>-line.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
******************************************** End Include Z_MSE_ABAP ******************************************************************************************