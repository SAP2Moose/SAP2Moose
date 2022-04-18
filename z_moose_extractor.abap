* generated on system T80 at 02.01.2022 on 09:38:05

*
* This is version 1.3
*
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

"! The latest version is available on https://github.com/SAP2Moose/SAP2Moose
"!
"! Thanks to Enno Wulff for providing the initial ABAP 7.31 version
"!
REPORT z2mse_moose_extractor2.
TABLES tadir. "So that select-options work

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE TEXT-001.


SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS s_pack FOR tadir-devclass.
SELECT-OPTIONS s_spack FOR tadir-devclass.
DATA: element_filter TYPE string.
PARAMETERS p_eltyp TYPE text30.
PARAMETERS p_elpar TYPE c LENGTH 30.
PARAMETERS p_elnam TYPE c LENGTH 61.
PARAMETERS p_sub AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_nup TYPE i DEFAULT -1.
PARAMETERS p_ndown TYPE i DEFAULT 1.
"Exclude interfaces in sap name space when found via where used analysis
PARAMETERS p_ex AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_dyn TYPE string. "Classes to determine dynamic accesses

*SELECT-OPTIONS s_compsn FOR tadir-obj_name.

SELECTION-SCREEN END OF BLOCK block_selct_sap_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_using_comp WITH FRAME TITLE TEXT-003.

*PARAMETERS: p_dm AS CHECKBOX DEFAULT ' '.
*"! Usages outside package grouped
*"! If false, a recursive search for using components is performed until no further using components are found
*DATA g_param_usage_outpack_groupd TYPE abap_bool.
*g_param_usage_outpack_groupd = p_dm.

SELECTION-SCREEN END OF BLOCK block_using_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_infos WITH FRAME TITLE TEXT-004.

*PARAMETERS: p_list AS CHECKBOX DEFAULT ' '.
*"! List Tokens of selected programs
*DATA g_parameter_list_tokens TYPE abap_bool.
*g_parameter_list_tokens = p_list.

SELECTION-SCREEN END OF BLOCK block_infos.

" include z_mse.
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

SELECTION-SCREEN BEGIN OF BLOCK bl_model_settings WITH FRAME TITLE TEXT-100.

PARAMETERS: p_down  AS CHECKBOX DEFAULT 'X',
            p_somix AS CHECKBOX DEFAULT 'X',
            " Default filename
            p_df    TYPE string.
*"! Download model to file
*DATA g_parameter_download_file TYPE abap_bool.
*g_parameter_download_file = p_down.
SELECTION-SCREEN END OF BLOCK bl_model_settings.

SELECTION-SCREEN BEGIN OF BLOCK bl_customize WITH FRAME TITLE TEXT-200.

PARAMETERS: p_intrev AS CHECKBOX DEFAULT ''.

SELECTION-SCREEN END OF BLOCK bl_customize.



" Begin Model
"! Specifies a model.
"! Create an instance only once, otherwise there will be multiple models each containing only part of the informations.
CLASS cl_model DEFINITION
  CREATE PUBLIC .
  PUBLIC SECTION.

    TYPES: BEGIN OF line_type,
             line TYPE string,
           END OF line_type.
    TYPES: lines_type TYPE STANDARD TABLE OF line_type WITH DEFAULT KEY.

    METHODS constructor.

    "! Add a named entity
    "! @parameter elementname | The name of the FAMIX Element. Like FAMIX.NamedEntity
    "! @parameter name_group | optional to handle cases where names may be duplicates
    "! @parameter is_named_entity | True if the entity has a name
    "! @parameter is_id_required  | Set true if (id: ...) is always required
    "! @parameter can_be_referenced_by_name | True if referencing by name is possible (For this the name has to be unique)
    "! @parameter name | the name of a FAMIX Entity that inherits from FAMIX.NamedEntity leave empty is is_named_entity is false
    "! @parameter exists_already_with_id | only if can_be_referenced_by_name true. Zero if it does not yet exist, otherwise filled with id
    "! @parameter processedid | the id in model either if just created or already existing
    METHODS add_entity
      IMPORTING elementname                   TYPE clike
                name_group                    TYPE clike OPTIONAL
                is_named_entity               TYPE abap_bool
                is_id_required                TYPE abap_bool DEFAULT ''
                can_be_referenced_by_name     TYPE abap_bool
                name                          TYPE clike OPTIONAL
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(processed_id)           TYPE i.

    "! Generates a string with a valid MSE file
    METHODS make_mse
      EXPORTING
        mse_model TYPE lines_type.

    "! Generates an attribute of type string
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter attribute_name | the name of the attribute
    "! @parameter string | The value of the attribute
    METHODS add_string
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        attribute_name     TYPE clike
        string             TYPE clike.

    "! Generates an attribute of type reference using a name
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter attribute_name | the name of the attribute
    "! @parameter elementname | the element type of the reference
    "! @parameter name_of_reference | the reference
    METHODS add_reference_by_name
      IMPORTING
        element_id              TYPE i
        element_type            TYPE clike OPTIONAL
        element_name_group      TYPE clike OPTIONAL
        element_name            TYPE clike OPTIONAL
        attribute_name          TYPE clike
        type_of_reference       TYPE clike
        name_group_of_reference TYPE clike OPTIONAL
        name_of_reference       TYPE clike.

    "! Generates an attribute of type reference using an id
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter attribute_name | the name of the attribute
    "! @parameter reference_id | the id of the reference
    METHODS add_reference_by_id
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        attribute_name     TYPE clike
        reference_id       TYPE i.

    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    METHODS add_boolean
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        attribute_name     TYPE clike
        is_true            TYPE abap_bool.

    " Public type so that the caller is able to test the model

    TYPES:
      "! A public type that returns the attributes of the model
      "! Provide either ID or type and name of element
      "! @parameter element_id | the ID of the element where the ID shall be added
      "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
      "! @parameter element_name_group | the name group of the element where the ID shall be added
      "! @parameter element_name | the name of the element
      BEGIN OF public_attribute_type,
        attribute_id   TYPE i,
        attribute_type TYPE string,
        string         TYPE string,
        reference      TYPE i,
        boolean        TYPE abap_bool,
      END OF public_attribute_type.


    TYPES:
      "! A public table type to contain the attributes of an element
      public_attributes_type TYPE HASHED TABLE OF public_attribute_type WITH UNIQUE KEY attribute_id.


    TYPES:
      "! A type that contains informations on an element
      BEGIN OF public_element_type,
        element_id        TYPE i,
        element_type      TYPE string,
        is_named_entity   TYPE abap_bool,
        is_id_required    TYPE abap_bool,
        public_attributes TYPE public_attributes_type,
      END OF public_element_type.
    TYPES:
      "! A public table type to contain all elements of a model
      public_elements_type TYPE HASHED TABLE OF public_element_type WITH UNIQUE KEY element_id.

    "! Returns the current model
    "! Use for checks and to decide what further actions are done to build the model.
    "! Also to give a feedback what is extracted
    METHODS get_model
      RETURNING VALUE(public_elements) TYPE public_elements_type.


protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_in_model_type,
             element_id      TYPE i,
             is_named_entity TYPE abap_bool,
             is_id_required  TYPE abap_bool,
             element_type    TYPE string,
           END OF element_in_model_type.
    "! A table with all Elements in the model
    DATA g_elements_in_model TYPE HASHED TABLE OF element_in_model_type WITH UNIQUE KEY element_id.

    TYPES: BEGIN OF named_entity_type,
             element_type       TYPE string,
             element_name_group TYPE string,
             element_name       TYPE string,
             element_id         TYPE i,
           END OF named_entity_type.

    "! A table to find IDs using the names
    DATA g_named_entities TYPE HASHED TABLE OF named_entity_type WITH UNIQUE KEY element_type element_name_group element_name.

    TYPES value_type TYPE c LENGTH 1.

    "! An attribute where a name is specified
    CONSTANTS string_value TYPE value_type VALUE 'S'.

    "! An attribute where a reference is specified
    CONSTANTS reference_value TYPE value_type VALUE 'R'.

    CONSTANTS boolean_value TYPE value_type VALUE 'B'.

    TYPES: BEGIN OF attribute_type,
             element_id     TYPE i,
             attribute_id   TYPE i,
             attribute_type TYPE string,
             value_type     TYPE value_type,
             name           TYPE string,
             reference      TYPE i,
             boolean        TYPE abap_bool,
           END OF attribute_type.

    "! A table with all the attributes of an entity
    DATA g_attributes TYPE SORTED TABLE OF attribute_type WITH UNIQUE KEY element_id attribute_id.

    "! The ID of processed entity in the model
    DATA g_processed_id TYPE i.
    "! The ID of any attribute. Unique together with mv_id
    DATA g_attribute_id TYPE i.
    "! True if attribute is already identically assigned
    METHODS _check_if_attr_already_there
      IMPORTING
        attribute            TYPE cl_model=>attribute_type
      RETURNING
        VALUE(already_there) TYPE abap_bool.
    METHODS _get_element_id
      IMPORTING
        element_id           TYPE i
        element_type         TYPE clike
        element_name_group   TYPE clike
        element_name         TYPE clike
      RETURNING
        VALUE(my_element_id) TYPE i.


ENDCLASS.
CLASS CL_MODEL IMPLEMENTATION.
  METHOD add_boolean.

    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute.
    ls_attribute-element_id             =  _get_element_id( element_id         = element_id
                                                   element_type       = element_type
                                                   element_name_group = element_name_group
                                                   element_name       = element_name ).
    ls_attribute-attribute_type = attribute_name.
    ls_attribute-value_type     = boolean_value.
    ls_attribute-boolean        = is_true.

    " SAP_2_FAMIX_52        Do not attributes twice if they are added with identical attributes

    IF _check_if_attr_already_there( ls_attribute ) EQ abap_false.
      sy-subrc = 1.
      WHILE sy-subrc <> 0.
        ADD 1 TO g_attribute_id.
        ls_attribute-attribute_id   = g_attribute_id.
        INSERT ls_attribute INTO TABLE g_attributes.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.
  METHOD add_entity.

    IF can_be_referenced_by_name EQ abap_true.
      ASSERT name_group IS NOT INITIAL.
    ENDIF.

    FIELD-SYMBOLS <ls_name> LIKE LINE OF g_named_entities.

    IF can_be_referenced_by_name EQ abap_true.

      READ TABLE g_named_entities ASSIGNING <ls_name>
            WITH TABLE KEY element_type = elementname element_name_group = name_group element_name = name.
      IF sy-subrc EQ 0. "OK
        exists_already_with_id = <ls_name>-element_id.
        processed_id = <ls_name>-element_id.
        RETURN.
      ENDIF.

    ENDIF.

    ADD 1 TO g_processed_id.
    g_attribute_id = 0.

    IF can_be_referenced_by_name EQ abap_true.
      DATA ls_named_entity    LIKE LINE OF g_named_entities.  " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
      CLEAR ls_named_entity.
      ls_named_entity-element_type = elementname.
      ls_named_entity-element_name_group  = name_group.
      ls_named_entity-element_name       = name.
      ls_named_entity-element_id          = g_processed_id.
      INSERT ls_named_entity INTO TABLE g_named_entities.
    ENDIF.

    DATA ls_elements_in_model LIKE LINE OF g_elements_in_model. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_elements_in_model.
    ls_elements_in_model-element_id = g_processed_id.
    ls_elements_in_model-is_named_entity = is_named_entity.
    ls_elements_in_model-is_id_required = is_id_required.
    ls_elements_in_model-element_type = elementname.
    INSERT ls_elements_in_model INTO TABLE g_elements_in_model.

    IF is_named_entity EQ abap_true.
      me->add_string( EXPORTING element_id = g_processed_id attribute_name = 'name' string = name element_name_group = name_group ).
    ENDIF.

    processed_id = g_processed_id.

  ENDMETHOD.
  METHOD add_reference_by_id.

    DATA ls_attribute TYPE attribute_type. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute.
    ls_attribute-element_id     =  _get_element_id( element_id         = element_id
                                                   element_type       = element_type
                                                   element_name_group = element_name_group
                                                   element_name       = element_name ).
    ls_attribute-attribute_type = attribute_name.
    ls_attribute-value_type     = reference_value.
    ls_attribute-reference      = reference_id.

    " SAP_2_FAMIX_52        Do not attributes twice if they are added with identical attributes

    IF _check_if_attr_already_there( ls_attribute ) EQ abap_false.
      sy-subrc = 1.
      WHILE sy-subrc <> 0.
        ADD 1 TO g_attribute_id.
        ls_attribute-attribute_id   = g_attribute_id.
        INSERT ls_attribute INTO TABLE g_attributes.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.
  METHOD add_reference_by_name.

    ASSERT name_group_of_reference IS NOT INITIAL.

    FIELD-SYMBOLS <named_entity> LIKE LINE OF g_named_entities.

    READ TABLE g_named_entities ASSIGNING <named_entity> WITH TABLE KEY element_type = type_of_reference
                                                                        element_name_group = name_group_of_reference
                                                                        element_name = name_of_reference.
*    ASSERT sy-subrc EQ 0. "OK
    IF sy-subrc <> 0.
      ASSERT 1 = 2.
    ENDIF.

    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute.
    ls_attribute-element_id     = _get_element_id( element_id         = element_id
                                                   element_type       = element_type
                                                   element_name_group = element_name_group
                                                   element_name       = element_name ).
    ls_attribute-attribute_type = attribute_name.
    ls_attribute-value_type     = reference_value.
    ls_attribute-reference      = <named_entity>-element_id.

    " SAP_2_FAMIX_52        Do not attributes twice if they are added with identical attributes

    IF _check_if_attr_already_there( ls_attribute ) EQ abap_false.
      sy-subrc = 1.
      WHILE sy-subrc <> 0.
        ADD 1 TO g_attribute_id.
        ls_attribute-attribute_id   = g_attribute_id.
        INSERT ls_attribute INTO TABLE g_attributes.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.
  METHOD add_string.

    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute.
    ls_attribute-element_id             =  _get_element_id( element_id         = element_id
                                                            element_type       = element_type
                                                            element_name_group = element_name_group
                                                            element_name       = element_name ).
    ls_attribute-attribute_type = attribute_name.
    ls_attribute-value_type     = string_value.
    ls_attribute-name         = string.

    " SAP_2_FAMIX_52        Do not attributes twice if they are added with identical attributes

    IF _check_if_attr_already_there( ls_attribute ) EQ abap_false.
      sy-subrc = 1.
      WHILE sy-subrc <> 0.
        ADD 1 TO g_attribute_id.
        ls_attribute-attribute_id   = g_attribute_id.
        INSERT ls_attribute INTO TABLE g_attributes.
      ENDWHILE.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    g_processed_id = 0.
  ENDMETHOD.
  METHOD get_model.

    DATA ls_public_element TYPE public_element_type.
    DATA ls_public_attribute TYPE public_attribute_type.
    DATA lt_public_attributes TYPE public_attributes_type.

    CLEAR public_elements.

    DATA ls_elements_in_model LIKE LINE OF g_elements_in_model.
    LOOP AT g_elements_in_model INTO ls_elements_in_model.



      CLEAR lt_public_attributes.

      DATA ls_attributes LIKE LINE OF g_attributes.
      LOOP AT g_attributes INTO ls_attributes WHERE element_id = ls_elements_in_model-element_id.

        CLEAR ls_public_attribute.
        ls_public_attribute-attribute_id = ls_attributes-attribute_id.
        ls_public_attribute-attribute_type = ls_attributes-attribute_type.
        ls_public_attribute-boolean = ls_attributes-boolean.
        ls_public_attribute-reference = ls_attributes-reference.
        ls_public_attribute-string = ls_attributes-name.
        INSERT ls_public_attribute INTO TABLE lt_public_attributes.

      ENDLOOP.

      CLEAR ls_public_element.
      ls_public_element-element_type = ls_elements_in_model-element_type.
      ls_public_element-element_id = ls_elements_in_model-element_id.
      ls_public_element-is_named_entity = ls_elements_in_model-is_named_entity.
      ls_public_element-is_id_required = ls_elements_in_model-is_id_required.
      ls_public_element-public_attributes = lt_public_attributes.

      INSERT ls_public_element INTO TABLE public_elements.

    ENDLOOP.
  ENDMETHOD.
  METHOD make_mse.

    " SAP_2_FAMIX_34      Allow to export the model in the .mse Moose format

    DATA: mse_model_line TYPE line_type.

    mse_model_line-line = |( |.

    SORT g_elements_in_model BY element_id.

    DATA is_first TYPE boolean VALUE abap_true.

    FIELD-SYMBOLS <element_in_model> LIKE LINE OF g_elements_in_model.

    LOOP AT g_elements_in_model ASSIGNING <element_in_model>.
      IF is_first EQ abap_false.

        APPEND mse_model_line TO mse_model.
        CLEAR mse_model_line.
      ENDIF.

      mse_model_line-line = mse_model_line-line && |(| && <element_in_model>-element_type.
      IF    <element_in_model>-is_named_entity EQ abap_true
         OR <element_in_model>-is_id_required EQ abap_true.

        mse_model_line-line = mse_model_line-line && | (id: | && <element_in_model>-element_id && | )|.
      ENDIF.

      FIELD-SYMBOLS <attribute> LIKE LINE OF g_attributes.
      LOOP AT g_attributes ASSIGNING <attribute> WHERE element_id = <element_in_model>-element_id.

        APPEND mse_model_line TO mse_model.
        mse_model_line-line = |  (| && <attribute>-attribute_type.
        ASSERT ( <attribute>-value_type EQ string_value ) OR ( <attribute>-value_type EQ reference_value ) OR ( <attribute>-value_type EQ boolean_value ).
        CASE <attribute>-value_type.
          WHEN string_value.

            mse_model_line-line = mse_model_line-line && | '| && <attribute>-name && |')|.

          WHEN reference_value.

            mse_model_line-line = mse_model_line-line && | (ref: | && <attribute>-reference && |))|.

          WHEN boolean_value.
            ASSERT ( <attribute>-boolean EQ abap_true ) OR ( <attribute>-boolean EQ abap_false ).
            CASE <attribute>-boolean.
              WHEN abap_true.
                mse_model_line-line = mse_model_line-line && | true)|.
              WHEN abap_false.
                mse_model_line-line = mse_model_line-line && | false)|.
            ENDCASE.
        ENDCASE.

      ENDLOOP.

      mse_model_line-line = mse_model_line-line && |)|.

      is_first = abap_false.
    ENDLOOP.

    mse_model_line-line = mse_model_line-line && |)|.
    APPEND mse_model_line TO mse_model.

  ENDMETHOD.
  METHOD _check_if_attr_already_there.

    " Check if attribute is already there
    DATA ls_attribute_2 TYPE attribute_type.
    DATA ls_attribute_3 TYPE attribute_type.

    ls_attribute_3 = attribute.
    CLEAR ls_attribute_3-attribute_id.

    already_there = abap_false.

    LOOP AT g_attributes INTO ls_attribute_2 WHERE element_id = attribute-element_id.
      CLEAR ls_attribute_2-attribute_id.
      IF ls_attribute_2 EQ ls_attribute_3.
        already_there = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD _get_element_id.

    " Get element ID
    IF element_id <> 0.
      my_element_id = element_id.
    ELSE.
      ASSERT element_name_group IS NOT INITIAL.
      FIELD-SYMBOLS <element_named_entity> LIKE LINE OF g_named_entities.
      READ TABLE g_named_entities ASSIGNING <element_named_entity> WITH TABLE KEY element_type = element_type
                                                                          element_name_group = element_name_group
                                                                          element_name = element_name.
      ASSERT sy-subrc EQ 0. "OK
      my_element_id = <element_named_entity>-element_id.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS cl_output_model DEFINITION
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS make
      IMPORTING
        mse_model                 TYPE cl_model=>lines_type
        g_parameter_download_file TYPE abap_bool
        i_default_prefix          TYPE string.
protected section.
private section.
ENDCLASS.
CLASS CL_OUTPUT_MODEL IMPLEMENTATION.
  METHOD make.
    " Download the file

    DATA: filename          TYPE string,
          default_file_name TYPE string,
          pathname          TYPE string,
          fullpath          TYPE string,
          user_action       TYPE i.

    default_file_name = |{ i_default_prefix }_{ sy-sysid }_{ sy-datum }_{ sy-uzeit }|.

    IF g_parameter_download_file EQ abap_true.

      cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension = 'mse'
                                                            default_file_name = default_file_name
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

  ENDMETHOD.
ENDCLASS.

******************************************** End Include Z_MSE_ABAP *******************************

" include z_famix.
******************************************** Begin Include Z_FAMIX_ABAP ***************************
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



CLASS cl_famix_entity DEFINITION ABSTRACT
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
  PROTECTED SECTION.
    DATA g_elementname TYPE string.
    DATA g_model TYPE REF TO cl_model.
    DATA g_last_used_id TYPE i.
private section.
ENDCLASS.
CLASS CL_FAMIX_ENTITY IMPLEMENTATION.
  METHOD constructor.
    g_model = model.
  ENDMETHOD.
ENDCLASS.


CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM cl_famix_entity
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! Declare source language
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter source_language_element | the FAMIX element of the source language
    "! @parameter source_language_name | the name of the source language
    METHODS set_declared_source_language
      IMPORTING
        element_id              TYPE i
        element_type            TYPE clike OPTIONAL
        element_name_group      TYPE clike OPTIONAL
        element_name            TYPE clike OPTIONAL
        source_language_element TYPE clike
        source_language_name    TYPE clike.
protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_SOURCED_ENTITY IMPLEMENTATION.
  METHOD set_declared_source_language.
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              attribute_name    = 'declaredSourceLanguage'
                                              type_of_reference       = source_language_element
                                              name_of_reference = source_language_name ).
  ENDMETHOD.
ENDCLASS.

class CL_FAMIX_FILE_ANCHOR definition
  inheriting from CL_FAMIX_ENTITY
  create public .

public section.

    "! Call once to create a new file anchor entiry
    "! @parameter element_id | The ID of the element for which a source is specified
    "! @parameter file_name | The path or link to the source
  methods ADD
    importing
      !ELEMENT_ID type I
      !FILE_NAME type CLIKE
    exporting
      value(ID) type I .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS CL_FAMIX_FILE_ANCHOR IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = |FAMIX.FileAnchor|
                                   name_group = |FILE_ANCHOR|
                                   is_named_entity = abap_false
                                   is_id_required            = abap_true
                                   can_be_referenced_by_name = abap_false
                         IMPORTING processed_id = id ).

    g_model->add_reference_by_id( EXPORTING element_id         = id
                                            attribute_name     = 'element'
                                            reference_id       = element_id ).

    g_model->add_string( EXPORTING element_id     = id
                                   attribute_name = 'fileName'
                                   string         = file_name ).

    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_named_entity DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT
  CREATE PUBLIC.
  PUBLIC SECTION.

    "! Call once to create a new named entity
    "! @parameter exists_already_with_id | Contains the id if entry already existed.
    "! @parameter id | The id in model either if just created or already existing.
    "! @parameter modifiers | A list of modifiers separated by blank. This attribute is marked by an asterisk in the Moose Meta Browser, which may be the sign of this. Will be an Ordered Collection in Moose.
    METHODS add
      IMPORTING name_group                    TYPE clike OPTIONAL
                name                          TYPE clike
                modifiers                     TYPE clike OPTIONAL
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i.
    "! Call once to set the parent package
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_package | the name of an element of type FAMIX.Package
    METHODS set_parent_package IMPORTING element_id         TYPE i
                                         element_type       TYPE clike OPTIONAL
                                         element_name_group TYPE clike OPTIONAL
                                         element_name       TYPE clike OPTIONAL
                                         parent_package     TYPE clike
                                         parent_package_name_group TYPE clike.

    "! Set the container an element is in using the reference
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter source_anchor_id | the id of the SoureceAnchor
    METHODS set_source_anchor_by_id IMPORTING element_id         TYPE i
                                              element_type       TYPE clike OPTIONAL
                                              element_name_group TYPE clike OPTIONAL
                                              element_name       TYPE clike OPTIONAL
                                              source_anchor_id   TYPE i.

  PROTECTED SECTION.

private section.
ENDCLASS.
CLASS CL_FAMIX_NAMED_ENTITY IMPLEMENTATION.
  METHOD add.
"    ASSERT name_group IS NOT INITIAL.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    IF modifiers IS SUPPLIED.
      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'modifiers'
                                     string         = modifiers ).
    ENDIF.
    g_last_used_id = id.
  ENDMETHOD.
  METHOD set_parent_package.
    g_model->add_reference_by_name( element_id = element_id
                                    element_type = element_type
                                    element_name_group = element_name_group
                                    element_name = element_name type_of_reference       = 'FAMIX.Package'
                                    name_of_reference = parent_package
                                    name_group_of_reference = parent_package_name_group
                                    attribute_name    = 'parentPackage' ).
  ENDMETHOD.
  METHOD set_source_anchor_by_id.

    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'sourceAnchor'
                                            reference_id   = source_anchor_id  ).

  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_attribute DEFINITION INHERITING FROM cl_famix_named_entity
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    METHODS store_id
      IMPORTING
        name_group TYPE clike
        class     TYPE clike
        attribute TYPE clike.
    "! Returns the ID for a given attribute of a class
    "! Returns 0 if the attribute is not known
    "! @parameter class | the class of the attribute
    "! @parameter attribute | the attribute name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                name_group TYPE clike
                class     TYPE clike
                attribute TYPE clike
      RETURNING VALUE(id) TYPE i.
    METHODS add REDEFINITION.

    "! set the parent type, for instance the class the method is contained in
    "! Provide either ID or type and name of element
    "! For parent: provide either parent_element and parent_name or parent_id
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name_group | the name group of the parent element
    "! @parameter parent_name | the name of the parent element
    "! @parameter parent_id | the id of the parent element
    METHODS set_parent_type
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_element     TYPE clike OPTIONAL
        parent_name_group  TYPE clike OPTIONAL
        parent_name        TYPE clike OPTIONAL
        parent_id          TYPE i     OPTIONAL.
protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF attribute_id_type,
             name_group TYPE string,
             class     TYPE string,
             attribute TYPE string,
             id        TYPE i,
           END OF attribute_id_type.
    DATA: g_attribute_ids TYPE HASHED TABLE OF attribute_id_type WITH UNIQUE KEY name_group class attribute.
ENDCLASS.
CLASS CL_FAMIX_ATTRIBUTE IMPLEMENTATION.
  METHOD add.
    g_model->add_entity(
               EXPORTING elementname = g_elementname
                         is_named_entity = abap_true
                         can_be_referenced_by_name = abap_false
                         name = name
               IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Attribute'.
  ENDMETHOD.
  METHOD get_id.
    FIELD-SYMBOLS <attribute_id> LIKE LINE OF g_attribute_ids.

    READ TABLE g_attribute_ids ASSIGNING <attribute_id> WITH TABLE KEY name_group = name_group class = class attribute = attribute.
    IF sy-subrc EQ 0. "OK
      id = <attribute_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.
  METHOD set_parent_type.
    ASSERT ( parent_element IS SUPPLIED AND parent_name IS SUPPLIED )
        OR parent_id IS SUPPLIED.
    IF parent_element IS SUPPLIED AND parent_name IS SUPPLIED.
      g_model->add_reference_by_name( EXPORTING element_id        = element_id
                                                element_type       = element_type
                                                element_name_group = element_name_group
                                                element_name       = element_name
                                                attribute_name     = 'parentType'
                                                type_of_reference  = parent_element
                                                name_group_of_reference = parent_name_group
                                                name_of_reference  = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING element_id        = element_id
                                                element_type       = element_type
                                                element_name_group = element_name_group
                                                element_name       = element_name
                                                attribute_name     = 'parentType'
                                                reference_id       = parent_id ).
    ENDIF.
  ENDMETHOD.
  METHOD store_id.
    DATA ls_attribute_id LIKE LINE OF g_attribute_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute_id.
    ls_attribute_id-id = g_last_used_id.
    ls_attribute_id-name_group = name_group.
    ls_attribute_id-class = class.
    ls_attribute_id-attribute = attribute.
    INSERT ls_attribute_id INTO TABLE g_attribute_ids.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_container_entity DEFINITION INHERITING FROM cl_famix_named_entity ABSTRACT
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! Set the container an element is in
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container | the name of the Container
    METHODS set_container IMPORTING element_id         TYPE i
                                    element_type       TYPE clike OPTIONAL
                                    element_name_group TYPE clike OPTIONAL
                                    element_name       TYPE clike OPTIONAL container_element TYPE clike
                                    parent_container   TYPE clike
                                    parent_container_name_group TYPE clike.
    "! Set the container an element is in using the reference
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container_id | the id of the Container
    METHODS set_container_by_id IMPORTING element_id          TYPE i
                                          element_type        TYPE clike OPTIONAL
                                          element_name_group  TYPE clike OPTIONAL
                                          element_name        TYPE clike OPTIONAL container_element   TYPE clike
                                          parent_container_id TYPE i.
  PROTECTED SECTION.

private section.
ENDCLASS.
CLASS CL_FAMIX_CONTAINER_ENTITY IMPLEMENTATION.
  METHOD set_container.
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              type_of_reference       = container_element
                                              name_of_reference = parent_container
                                              name_group_of_reference = parent_container_name_group
                                              attribute_name    = 'container' ).
  ENDMETHOD.
  METHOD set_container_by_id.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'container'
                                            reference_id   = parent_container_id ).

  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_behavioural_entty DEFINITION INHERITING FROM CL_famix_container_entity ABSTRACT
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! Set the signature of a method
    "! This might not be relevant for ABAP, but is contained here for completeness
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter signature | The signature like myMethod( myParameters, ...)
    METHODS set_signature IMPORTING
                            element_id         TYPE i
                            element_type       TYPE clike OPTIONAL
                            element_name_group TYPE clike OPTIONAL
                            element_name       TYPE clike OPTIONAL
                            signature          TYPE clike.

protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_BEHAVIOURAL_ENTTY IMPLEMENTATION.
  METHOD set_signature.
    g_model->add_string( EXPORTING element_id = element_id
                                   element_type = element_type
                                   element_name_group = element_name_group
                                   element_name = element_name
                                   attribute_name = 'signature'
                                   string         = signature ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_package DEFINITION INHERITING FROM cl_famix_named_entity
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_PACKAGE IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Package'.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_method DEFINITION INHERITING FROM CL_famix_behavioural_entty
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! set the parent type, for instance the class the method is contained in
    "! Provide either parent_name or parent_id
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name_group | optional the name group of the parent element
    "! @parameter parent_name | optional the name of the parent element
    "! @parameter parent_id | optional the id of the parent element
    METHODS set_parent_type
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_element     TYPE clike
        parent_name_group  TYPE clike OPTIONAL
        parent_name        TYPE clike OPTIONAL
        parent_id          TYPE i OPTIONAL.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class_name_group | the name group of the class of the method
    "! @parameter class | the class of the method
    "! @parameter method_name_group | the name group of the method name
    "! @parameter method | the method name
    METHODS store_id
      IMPORTING class_name_group  TYPE clike
                class             TYPE clike
                method_name_group TYPE clike
                method            TYPE clike.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class_name_group | the name group of the class of the method
    "! @parameter class | the class of the method
    "! @parameter method_name_group | the name group of the method name
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING class_name_group  TYPE clike
                class             TYPE clike
                method_name_group TYPE clike
                method            TYPE clike
      RETURNING VALUE(id)         TYPE i.
protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_method_id,
             class_name_group  TYPE string,
             class             TYPE string,
             method_name_group TYPE string,
             method            TYPE string,
             id                TYPE i,
           END OF ty_method_id.
    DATA: g_method_ids TYPE HASHED TABLE OF ty_method_id WITH UNIQUE KEY class_name_group class method_name_group method.
ENDCLASS.
CLASS CL_FAMIX_METHOD IMPLEMENTATION.
  METHOD add.
    g_model->add_entity(
               EXPORTING elementname = g_elementname
                         is_named_entity = abap_true
                         can_be_referenced_by_name = abap_false
                         name = name
               IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Method'.
  ENDMETHOD.
  METHOD get_id.
    FIELD-SYMBOLS <method_id> LIKE LINE OF g_method_ids.

    READ TABLE g_method_ids ASSIGNING <method_id> WITH TABLE KEY class_name_group = class_name_group
                                                                 class = class
                                                                 method_name_group = method_name_group
                                                                 method = method.
    IF sy-subrc EQ 0. "OK
      id = <method_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.
  METHOD set_parent_type.
    ASSERT parent_name IS SUPPLIED OR parent_id IS SUPPLIED.
    IF parent_name IS SUPPLIED.
      g_model->add_reference_by_name( EXPORTING element_id = element_id
                                                element_type = element_type
                                                element_name_group = element_name_group
                                                element_name = element_name
                                                attribute_name    = 'parentType'
                                                type_of_reference       = parent_element
                                                name_group_of_reference = parent_name_group
                                                name_of_reference = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              attribute_name = 'parentType'
                                              reference_id   = parent_id ).
    ENDIF.
  ENDMETHOD.
  METHOD store_id.
    DATA ls_method_id LIKE LINE OF g_method_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_method_id.
    ls_method_id-id = g_last_used_id.
    ls_method_id-class_name_group = class_name_group.
    ls_method_id-class = class.
    ls_method_id-method_name_group = method_name_group.
    ls_method_id-method = method.
    INSERT ls_method_id INTO TABLE g_method_ids.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_class DEFINITION INHERITING FROM CL_famix_container_entity
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Set if it is an interface
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    METHODS is_interface
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL .
protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_CLASS IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Class'.
  ENDMETHOD.
  METHOD is_interface.
    g_model->add_boolean( EXPORTING element_id = element_id
                                    element_type = element_type
                                    element_name_group = element_name_group
                                    element_name = element_name
                                    attribute_name = 'isInterface'
                                    is_true        = abap_true ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_association DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.
protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_ASSOCIATION IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname               = g_elementname
                                        is_named_entity           = abap_false
                                        can_be_referenced_by_name = abap_false
                                        IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_access DEFINITION INHERITING FROM CL_famix_association
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Checks that accessor and variable of an access are a new access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS is_new_access
      IMPORTING
                accessor_id   TYPE i
                variable_id   TYPE i
      RETURNING VALUE(is_new) TYPE abap_bool.
    "! defines accessor and variable of an access
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS set_accessor_variable_relation
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        accessor_id        TYPE i
        variable_id        TYPE i.
protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_accessor_variable_id,
             accessor_id TYPE i,
             variable_id TYPE i,
           END OF  ty_accessor_variable_id.
    DATA: g_accessor_variable_ids TYPE HASHED TABLE OF ty_accessor_variable_id WITH UNIQUE KEY accessor_id variable_id.
ENDCLASS.
CLASS CL_FAMIX_ACCESS IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Access'.
  ENDMETHOD.
  METHOD is_new_access.
    READ TABLE g_accessor_variable_ids TRANSPORTING NO FIELDS WITH TABLE KEY accessor_id = accessor_id variable_id = variable_id.
    IF sy-subrc <> 0. "OK
      is_new = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD set_accessor_variable_relation.
    DATA ls_accessor_id LIKE LINE OF g_accessor_variable_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_accessor_id.
    ls_accessor_id-accessor_id = accessor_id.
    ls_accessor_id-variable_id = variable_id.
    INSERT ls_accessor_id INTO TABLE g_accessor_variable_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'variable'
                                            reference_id   = variable_id ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_invocation DEFINITION INHERITING FROM CL_famix_association
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS is_new_invocation_to_candidate
      IMPORTING
                sender_id     TYPE i
                candidates_id TYPE i
      RETURNING VALUE(is_new) TYPE abap_bool.

    "! defines an invocation
    "! this also models standard call by functions or methods to components other than attributes
    "! Us this method to reference the receiver using his id
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter sender_id | the id of the sender or calling method or function
    "! @parameter candidates_id | the id of the candidate, this is the used method or function of type BehaviouralEntity in case of method or function usage
    "! @parameter receiver_id | optional the id of the receiver or called method or function
    "! @parameter signature | optional a signature
    "! @parameter receiver_source_code | optional a receiver source code
    METHODS set_invocation_by_reference
      IMPORTING
        element_id           TYPE i
        element_type         TYPE clike OPTIONAL
        element_name_group   TYPE clike OPTIONAL
        element_name         TYPE clike OPTIONAL
        sender_id            TYPE i
        candidates_id        TYPE i OPTIONAL
        receiver_id          TYPE i OPTIONAL
        signature            TYPE clike OPTIONAL
        receiver_source_code TYPE clike OPTIONAL.

protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sender_candidate,
             sender_id     TYPE i,
             candidates_id TYPE i,
           END OF ty_sender_candidate.

    DATA g_sender_candidates TYPE HASHED TABLE OF ty_sender_candidate WITH UNIQUE KEY sender_id candidates_id.

ENDCLASS.
CLASS CL_FAMIX_INVOCATION IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Invocation'.
  ENDMETHOD.
  METHOD is_new_invocation_to_candidate.
    READ TABLE g_sender_candidates TRANSPORTING NO FIELDS WITH TABLE KEY sender_id = sender_id candidates_id = candidates_id.
    IF sy-subrc <> 0. "OK
      is_new = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD set_invocation_by_reference.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'sender'
                                            reference_id   = sender_id ).
    IF candidates_id IS SUPPLIED.
      DATA ls_sender_candidate LIKE LINE OF g_sender_candidates. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
      CLEAR ls_sender_candidate.
      ls_sender_candidate-sender_id = sender_id.
      ls_sender_candidate-candidates_id = candidates_id.
      INSERT ls_sender_candidate INTO TABLE g_sender_candidates.
      g_model->add_reference_by_id( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name attribute_name = 'candidates'
                                              reference_id   = candidates_id ).
    ENDIF.

    IF receiver_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              attribute_name = 'receiver'
                                              reference_id   = receiver_id ).
    ENDIF.
    IF signature IS SUPPLIED.
      g_model->add_string( EXPORTING element_id = element_id
                                     element_type = element_type
                                     element_name_group = element_name_group
                                     element_name = element_name
                                     attribute_name = 'signature'
                                     string         = signature ).
    ENDIF.
    IF receiver_source_code IS SUPPLIED.
      g_model->add_string( EXPORTING element_id = element_id
                                     element_type = element_type
                                     element_name_group = element_name_group
                                     element_name = element_name attribute_name = 'receiverSourceCode'
                                     string         = receiver_source_code ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_inheritance DEFINITION INHERITING FROM CL_famix_association
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter subclass_element | the FAMIX element of the subclass Type
    "! @parameter subclass_name_group | the name group of the subclass
    "! @parameter subclass_name | the name of the subclass
    "! @parameter superclass_element | the FAMIX element of the subclass Type
    "! @parameter superclass_name_group | the name group
    "! @parameter superclass_name | the name of the subclass of the superclass
    METHODS set_sub_and_super_class
      IMPORTING
        element_id            TYPE i
        subclass_element      TYPE clike
        subclass_name_group   TYPE clike
        subclass_name         TYPE clike
        superclass_element    TYPE clike
        superclass_name_group TYPE clike
        superclass_name       TYPE clike.

protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_INHERITANCE IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Inheritance'.
  ENDMETHOD.
  METHOD set_sub_and_super_class.
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                      attribute_name          = 'subclass'
                                      type_of_reference             = subclass_element
                                      name_group_of_reference = subclass_name_group
                                      name_of_reference       = subclass_name ).
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                      attribute_name          = 'superclass'
                                      type_of_reference             = superclass_element
                                      name_group_of_reference = superclass_name_group
                                      name_of_reference       = superclass_name ).

  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_custom_source_lng DEFINITION INHERITING FROM cl_famix_entity
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING name                          TYPE clike
                          name_group                    TYPE clike
                EXPORTING VALUE(exists_already_with_id) TYPE i
                          VALUE(id)                     TYPE i.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_CUSTOM_SOURCE_LNG IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name = name
                                        name_group = name_group
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.
ENDCLASS.

class ZCX_2MSE_EXTR3_CLASSES_WR_TYPE definition
  inheriting from CX_NO_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.
CLASS ZCX_2MSE_EXTR3_CLASSES_WR_TYPE IMPLEMENTATION.
  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.

class CL_SOMIX_ENTITY definition
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !MODEL type ref to CL_MODEL .
protected section.

  data G_MODEL type ref to CL_MODEL .
  data G_ELEMENTNAME type STRING .
  data G_LAST_USED_ID type I .
private section.
ENDCLASS.
CLASS cl_somix_element DEFINITION
  INHERITING FROM cl_somix_entity
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.
class CL_SOMIX_COMPONENT definition
  inheriting from CL_SOMIX_ELEMENT
  abstract
  create public .

public section.
protected section.

  data IS_PART_OF type ref to CL_SOMIX_COMPONENT .
  data PART_SPECIFICATION type STRING .
private section.
ENDCLASS.
CLASS cl_somix_coupling DEFINITION
  INHERITING FROM cl_somix_entity
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS cl_somix_access DEFINITION
  INHERITING FROM cl_somix_coupling
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS set_accessor_accessed_relation
      IMPORTING
        element_id   TYPE i
        accessor_id  TYPE i
        accessed_id  TYPE i
        is_write     TYPE abap_bool
        is_read      TYPE abap_bool
        is_dependent TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_accessor_accessed_id,
             accessor_id TYPE i,
             accessed_id TYPE i,
           END OF  ty_accessor_accessed_id.
    DATA: g_accessor_accessed_ids TYPE HASHED TABLE OF ty_accessor_accessed_id WITH UNIQUE KEY accessor_id accessed_id.
ENDCLASS.
CLASS cl_somix_call DEFINITION
  INHERITING FROM cl_somix_coupling
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS set_caller_called_relation
      IMPORTING
        element_id TYPE i
        caller_id  TYPE i
        called_id  TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_caller_called,
             caller_id TYPE i,
             called_id TYPE i,
           END OF ty_caller_called.

    DATA g_caller_called_ids TYPE HASHED TABLE OF ty_caller_called WITH UNIQUE KEY caller_id called_id.
ENDCLASS.
CLASS cl_somix_code DEFINITION
  INHERITING FROM cl_somix_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO cl_model .
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
        VALUE(id)                     TYPE i
      CHANGING
        unique_name                   TYPE clike.
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
CLASS cl_somix_data DEFINITION
  INHERITING FROM cl_somix_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO cl_model .
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
class CL_SOMIX_EXTRACTION definition
  inheriting from CL_SOMIX_ENTITY
  create public .

public section.
protected section.
private section.
ENDCLASS.
CLASS cl_somix_grouping DEFINITION
  INHERITING FROM cl_somix_element
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !model TYPE REF TO cl_model .
    "! Call method to store ID before add is used the next time for the same type of element
    METHODS add
      IMPORTING
        !grouping_name_group          TYPE clike
        !grouping                     TYPE clike
        !technical_type               TYPE clike
        !link_to_editor               TYPE clike
      EXPORTING
        VALUE(exists_already_with_id) TYPE i
        VALUE(id)                     TYPE i
      CHANGING
        unique_name                   TYPE clike.
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
CLASS cl_somix_parentchild DEFINITION
  INHERITING FROM cl_somix_entity
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: ty_helper_type TYPE c LENGTH 1.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING
                parent_id TYPE i
                child_id  TYPE i
                is_main   TYPE abap_bool
      RETURNING VALUE(id) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_parent_child_id,
             parent_id  TYPE i,
             child_id   TYPE i,
             element_id TYPE i,
           END OF  ty_parent_child_id.
    DATA: g_parent_child_ids TYPE HASHED TABLE OF ty_parent_child_id WITH UNIQUE KEY parent_id child_id.
ENDCLASS.

" Obsolete:



CLASS cl_famix_module DEFINITION INHERITING FROM cl_famix_named_entity
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

protected section.
private section.
ENDCLASS.
CLASS CL_FAMIX_MODULE IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name = name
                                        name_group = name_group
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Module'.
  ENDMETHOD.
ENDCLASS.

******************************************** End Include Z_FAMIX_ABAP *****************************

CLASS CL_EXTR3_ACCESS_OR_INVOCATN DEFINITION DEFERRED.
CLASS CL_EXTR3_ASSOCIATION DEFINITION DEFERRED.
CLASS CL_EXTR3_MODEL_BUILDER DEFINITION DEFERRED.
CLASS CL_EXTR3_ELEMENTS DEFINITION DEFERRED.

"! I know all elements and associations between elements that are currently known.
"! I provide general methods to add new elements and associations between elements.
CLASS cl_extr3_element_manager DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! A unique identifier for each object extracted
    TYPES element_id_type TYPE i .
    TYPES:
      BEGIN OF association_type,
        element_id1 TYPE element_id_type,
        element_id2 TYPE element_id_type,
        ass_type    TYPE c LENGTH 30, "To prevent problem with local classes, better would be: cl_extr3_association=>ass_type,
        association TYPE REF TO cl_extr3_association,
      END OF association_type .
    TYPES:
      associations_type TYPE STANDARD TABLE OF association_type WITH KEY element_id1 element_id2 ass_type association .

    DATA model TYPE REF TO cl_model .
    DATA famix_package TYPE REF TO cl_famix_package .
    DATA famix_class TYPE REF TO cl_famix_class .
    DATA famix_method TYPE REF TO cl_famix_method .
    DATA famix_attribute TYPE REF TO cl_famix_attribute .
    DATA famix_invocation TYPE REF TO cl_famix_invocation .
    DATA famix_access TYPE REF TO cl_famix_access .
    DATA famix_file_anchor TYPE REF TO cl_famix_file_anchor .

    DATA somix_extraction TYPE REF TO cl_somix_extraction.
    DATA somix_grouping TYPE REF TO cl_somix_grouping.
    DATA somix_code TYPE REF TO cl_somix_code.
    DATA somix_data TYPE REF TO cl_somix_data.
    DATA somix_call TYPE REF TO cl_somix_call.
    DATA somix_access TYPE REF TO cl_somix_access.
    DATA somix_parentchild TYPE REF TO cl_somix_parentchild.

    DATA exclude_found_sap_intf TYPE abap_bool READ-ONLY .
    DATA interface_use_structure TYPE abap_bool READ-ONLY .
    DATA model_builder TYPE REF TO cl_extr3_model_builder .
    DATA use_somix TYPE abap_bool READ-ONLY .

    METHODS constructor
      IMPORTING
        !i_model_builder           TYPE REF TO cl_extr3_model_builder
        !i_exclude_found_sap_intf  TYPE abap_bool
        !i_interface_use_structure TYPE abap_bool
        !i_use_somix               TYPE abap_bool OPTIONAL .
    "! Call if an element might be added.
    "! Add the element if it is not already part of the model.
    METHODS add_element
      IMPORTING
        !element          TYPE REF TO cl_extr3_elements
        !is_specific      TYPE abap_bool
      RETURNING
        VALUE(element_id) TYPE cl_extr3_element_manager=>element_id_type .
    METHODS add_association
      IMPORTING
        !element_1   TYPE element_id_type
        !element_2   TYPE element_id_type
        !association TYPE REF TO cl_extr3_association .
    "! Call so that the classes that contain the collected elements determine further informations that are required for the model.
    METHODS collect_infos
      IMPORTING
        !sysid TYPE string OPTIONAL .
    "! Call to build the mse model
    METHODS make_model
      RETURNING
        VALUE(r_result) TYPE cl_model=>lines_type .
    METHODS get_element
      IMPORTING
        !i_element_id   TYPE element_id_type
      RETURNING
        VALUE(r_result) TYPE REF TO cl_extr3_elements .
    METHODS get_associations
      IMPORTING
        !i_element_id       TYPE element_id_type
      RETURNING
        VALUE(associations) TYPE associations_type .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id TYPE element_id_type,
             "! A reference to the instance that handles this object
             element    TYPE REF TO cl_extr3_elements,
           END OF element_type.
    TYPES elements_type TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements TYPE elements_type.
    TYPES associations1_type TYPE SORTED TABLE OF association_type WITH UNIQUE KEY element_id1 element_id2 ass_type.
    TYPES associations2_type TYPE SORTED TABLE OF association_type WITH UNIQUE KEY element_id2 element_id1 ass_type.
    DATA associations1 TYPE associations1_type.
    DATA associations2 TYPE associations2_type.
    DATA next_element_id TYPE i.
ENDCLASS.
"! I am the top superclass for all classes that require the element manager.
CLASS cl_extr3 DEFINITION
  CREATE PROTECTED .

  PUBLIC SECTION.
    "! Call once to clear all global variables. This is required before an extraction is repeated
    CLASS-METHODS clear_all.
    CONSTANTS: ng_source_language TYPE string VALUE 'SOURCE_LANGUAGE',
               ng_abap_package TYPE string VALUE 'ABAP_PACKAGE',
               ng_abap_class TYPE string VALUE 'ABAP_CLASS',
               ng_abap_method TYPE string VALUE 'ABAP_METHOD',
               ng_abap_attribute TYPE string VALUE 'ABAP_ATTRIBUTE',
               ng_abap_program TYPE string VALUE 'ABAP_PROGRAM',
               ng_abap_webdynpro TYPE string VALUE 'ABAP_WEBDYNPRO',
               ng_database_schema TYPE string VALUE 'DATABASE_SCHEMA',
               ng_sap_table TYPE string VALUE 'SAP_TABLE'.
  PROTECTED SECTION.
    DATA element_manager TYPE REF TO cl_extr3_element_manager.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO cl_extr3_element_manager.
  PRIVATE SECTION.
ENDCLASS.
"! I describe an association between elements.
"! I have sub classes that specify concrete types of associations.
CLASS cl_extr3_association DEFINITION
  INHERITING FROM cl_extr3.

  PUBLIC SECTION.
    types ass_type TYPE c LENGTH 30.
    DATA type TYPE ass_type READ-ONLY.

    CONSTANTS: parent_package_ass LIKE type VALUE 'parent_package',
               access_ass         LIKE type VALUE 'access',
               invocation_ass     LIKE type VALUE 'invocation'.

    METHODS make_model
      IMPORTING
        association TYPE cl_extr3_element_manager=>association_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS cl_extr3_access_or_invocatn DEFINITION
  INHERITING FROM cl_extr3_association
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS _get_somix_id_used_and_using
      IMPORTING
        i_association TYPE cl_extr3_element_manager=>association_type
      EXPORTING
        e_using_id    TYPE i
        e_used_id     TYPE i.
    METHODS _get_famix_id_used_and_using
      IMPORTING
        i_association     TYPE cl_extr3_element_manager=>association_type
      EXPORTING
        e_using_method_id TYPE i
        e_used_id         TYPE i.
  PRIVATE SECTION.
ENDCLASS.
CLASS cl_extr3_access DEFINITION
  INHERITING FROM cl_extr3_access_or_invocatn
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO cl_extr3_access.
    METHODS add
      IMPORTING
        accessed_element_id1  TYPE cl_extr3_element_manager=>element_id_type
        accessing_element_id2 TYPE cl_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_access.

    TYPES: BEGIN OF association_type,
             accessed_element_id1  TYPE cl_extr3_element_manager=>element_id_type,
             accessing_element_id2 TYPE cl_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY accessed_element_id1 accessing_element_id2.
ENDCLASS.
"! I am the abstract super class of all elements.
"! My subclasses know the details of elements.
CLASS cl_extr3_elements DEFINITION
  INHERITING FROM cl_extr3.

  PUBLIC SECTION.

    "! True if further informations are collected
    DATA infos_are_collected TYPE abap_bool.
    "! Collect further informations
    METHODS collect_infos IMPORTING sysid TYPE string.

    DATA type TYPE c LENGTH 30.

    CONSTANTS: package_type          LIKE type VALUE 'package',
               table_type            LIKE type VALUE 'table',
               class_type            LIKE type VALUE 'class',
               program_type          LIKE type VALUE 'program',
               web_dynpro_comps_type LIKE type VALUE 'web_dynpro_components'.

    METHODS make_model
      IMPORTING
        element_id   TYPE cl_extr3_element_manager=>element_id_type
        associations TYPE cl_extr3_element_manager=>associations_type.

    METHODS name
      IMPORTING
        element_id   TYPE cl_extr3_element_manager=>element_id_type
      EXPORTING
        element_type TYPE string
        parent_name TYPE string
        name TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.
CLASS cl_extr3_invocation DEFINITION
  INHERITING FROM cl_extr3_access_or_invocatn
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO cl_extr3_invocation.
    METHODS add
      IMPORTING
        invoced_element_id1  TYPE cl_extr3_element_manager=>element_id_type
        invocing_element_id2 TYPE cl_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_invocation.

    TYPES: BEGIN OF association_type,
             invoced_element_id1  TYPE cl_extr3_element_manager=>element_id_type,
             invocing_element_id2 TYPE cl_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY invoced_element_id1 invocing_element_id2.

ENDCLASS.
CLASS cl_extr3_parent_package DEFINITION
  INHERITING FROM cl_extr3_association
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO cl_extr3_parent_package.
    METHODS add
      IMPORTING
        element_id        TYPE cl_extr3_element_manager=>element_id_type
        parent_element_id TYPE cl_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_parent_package.

    TYPES: BEGIN OF association_type,
             element_id1 TYPE cl_extr3_element_manager=>element_id_type,
             element_id2 TYPE cl_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY element_id1 element_id2.

ENDCLASS.
"! I know how to build required associations
"! I have subclasses with concrete specifications that are used to find or build concrete associations.
CLASS cl_extr3_association_build DEFINITION
  INHERITING FROM cl_extr3.

  PUBLIC SECTION.

    METHODS search_down
      IMPORTING
        element_id TYPE cl_extr3_element_manager=>element_id_type.

    METHODS search_up
      IMPORTING
        element_id TYPE cl_extr3_element_manager=>element_id_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS cl_extr3_classes DEFINITION
  INHERITING FROM cl_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: is_class_type  TYPE seoclstype VALUE 0,
               interface_type TYPE seoclstype VALUE 1,
               attribute_type TYPE seocmptype VALUE 0,
               method_type    TYPE seocmptype VALUE 1,
               event_type     TYPE seocmptype VALUE 2.



    TYPES: BEGIN OF ty_class_component,
             clsname TYPE string,
             cmpname TYPE string,
             cmptype TYPE seocmptype,
             mtdtype TYPE seomtdtype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE STANDARD TABLE OF ty_class_component WITH KEY clsname cmpname.
    DATA: class_components TYPE ty_class_components.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO cl_extr3_classes.
    METHODS add
      IMPORTING
        class                   TYPE string
        is_specific             TYPE abap_bool
      EXPORTING
        VALUE(is_added)         TYPE abap_bool
        VALUE(new_element_id)   TYPE cl_extr3_element_manager=>element_id_type
        VALUE(class_components) TYPE ty_class_components.
    METHODS add_component
      IMPORTING
        clsname               TYPE string
        cmpname               TYPE string
        is_specific           TYPE abap_bool
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS class_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE string
        VALUE(clstype)    TYPE seoclstype
        VALUE(exists)     TYPE abap_bool.
    METHODS comp_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE string
        VALUE(cmpname)    TYPE string
        VALUE(cmptype)    TYPE seocmptype
        VALUE(exists)     TYPE abap_bool.
    METHODS is_redefinition_of_method
      IMPORTING
        invoced_element_id1  TYPE i
        invocing_element_id2 TYPE i
      RETURNING
        VALUE(r_result)      TYPE abap_bool.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_classes.

    TYPES: BEGIN OF element_type,
             element_id TYPE cl_extr3_element_manager=>element_id_type,
             class_name TYPE string,
             clstype    TYPE seoclstype,
             adt_link   TYPE string,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_class_name TYPE HASHED TABLE OF element_type WITH UNIQUE KEY class_name.

    TYPES: BEGIN OF element_comp_type,
             element_id TYPE cl_extr3_element_manager=>element_id_type,
             clsname    TYPE string,
             cmpname    TYPE string,
             cmptype    TYPE seocmptype,
             mtdtype    TYPE seomtdtype,
             adt_link   TYPE string,
           END OF element_comp_type.
    DATA elements_comp_element_id TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY element_id.
    DATA elements_comp_clsname_cmpname TYPE SORTED TABLE OF element_comp_type WITH UNIQUE KEY clsname cmpname.

    TYPES: BEGIN OF element_metarel_type,
             element_id TYPE cl_extr3_element_manager=>element_id_type,
             refclsname TYPE string,
             reltype    TYPE seoreltype,
           END OF element_metarel_type.
    DATA elements_metarel_element_id TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY element_id.
    DATA elements_metarel_refclsname TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY refclsname.

    TYPES: BEGIN OF redefined_type,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
             mtdname    TYPE seocpdname,
           END OF redefined_type.

    TYPES: BEGIN OF redefined_method_type,
             clsname            TYPE seoclsname,
             defined_in_clsname TYPE seoclsname,
             method             TYPE seocpdname,
           END OF redefined_method_type.
    DATA redefined_methods TYPE HASHED TABLE OF redefined_method_type WITH UNIQUE KEY method clsname defined_in_clsname.
    METHODS _add_component
      IMPORTING
        clsname               TYPE string
        cmpname               TYPE string
        is_specific           TYPE abap_bool
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(is_added_now)   TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.

    METHODS _add_metarel
      IMPORTING
        clsname     TYPE string
        is_specific TYPE abap_bool.

    "! Call me only after checking that the component to be added is not already added.
    METHODS _add_single_component_to_class
      IMPORTING
        i_found_class_name      TYPE string
        i_found_cmpname         TYPE string
        i_found_cmptype         TYPE seocmptype
        i_found_mtdtype         TYPE seomtdtype
        is_specific             TYPE abap_bool
      RETURNING
        VALUE(r_new_element_id) TYPE cl_extr3_element_manager=>element_id_type.

    METHODS _get_redefined
      IMPORTING
        class           TYPE string
      RETURNING
        VALUE(r_result) TYPE cl_extr3_classes=>ty_class_components.

ENDCLASS.
"! I describe an element of type package
CLASS cl_extr3_packages DEFINITION
  INHERITING FROM cl_extr3_elements
  CREATE PRIVATE
.

  PUBLIC SECTION.

    TYPES:
      ty_s_pack                         TYPE RANGE OF tadir-devclass.

    TYPES: BEGIN OF ty_package,
             package       TYPE devclass,
             parentpackage TYPE parentcl,
           END OF ty_package.
    TYPES ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO cl_extr3_packages.
    METHODS add
      IMPORTING package               TYPE devclass
      EXPORTING VALUE(is_added)       TYPE abap_bool
                VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS devclass
      IMPORTING
        i_element_id    TYPE i
      RETURNING
        VALUE(r_result) TYPE devclass.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
    METHODS _does_package_exists
      IMPORTING
        i_package     TYPE devclass
      RETURNING
        VALUE(exists) TYPE abap_bool.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id TYPE cl_extr3_element_manager=>element_id_type,
             devclass   TYPE devclass,
           END OF element_type.
    CLASS-DATA instance TYPE REF TO cl_extr3_packages.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_devclass TYPE HASHED TABLE OF element_type WITH UNIQUE KEY devclass.
ENDCLASS.
CLASS cl_extr3_programs DEFINITION
  INHERITING FROM cl_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
        i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO cl_extr3_programs.
    CONSTANTS: type_function          TYPE string VALUE 'FUNCTION',
               type_function_include  TYPE string VALUE 'FUNCTION_INCLUDE',
               type_program           TYPE string VALUE 'PROGRAM',
               type_bw_transformation TYPE string VALUE 'BW_TRAN'.
    METHODS add
      IMPORTING
        program               TYPE program
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS add_function
      IMPORTING
        function              TYPE string
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS program_name
      IMPORTING
        i_element_id                        TYPE i
      EXPORTING
        VALUE(program_type)                 TYPE string
        VALUE(program)                      TYPE progname
        VALUE(external_program_name_class)  TYPE string
        VALUE(external_program_name_method) TYPE string
        VALUE(program_attribute_1)          TYPE string
        VALUE(program_attribute_2)          TYPE string
        VALUE(subc)                         TYPE subc.
    METHODS add_function_group
      IMPORTING
        fgr            TYPE string
      EXPORTING
        is_added       TYPE abap_bool
        new_element_id TYPE i.

    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_programs.
    TYPES: BEGIN OF element_type,
             element_id            TYPE cl_extr3_element_manager=>element_id_type,
             program               TYPE progname,
             external_program_name TYPE string,
             subc                  TYPE subc,
             program_type          TYPE string,
             program_attribute_1   TYPE string,
             program_attribute_2   TYPE string,
             adt_or_bwmt_link      TYPE string,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_program TYPE HASHED TABLE OF element_type WITH UNIQUE KEY program.
    METHODS _convert_program_2_ext_name
      IMPORTING
        i_element_program   TYPE progname
      EXPORTING
        program_type        TYPE string
        program_attribute_1 TYPE string
        program_attribute_2 TYPE string
        VALUE(r_result)     TYPE string.
    METHODS _extract_function_name
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        function_group    TYPE rs38l_area
        function          TYPE rs38l_fnam
        function_include  TYPE string
        VALUE(r_result)   TYPE string.
    METHODS _extract_sap_bw_logic
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        tranid            TYPE rstranid
        VALUE(r_result)   TYPE string.
    METHODS _get_names_for_function_groups
      IMPORTING
        i_element                   TYPE cl_extr3_programs=>element_type
      RETURNING
        VALUE(name_of_mapped_class) TYPE string.
ENDCLASS.
"! I describe elements of type table
CLASS cl_extr3_tables DEFINITION
  INHERITING FROM cl_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    DATA database_schema  TYPE db_schema READ-ONLY.
    METHODS constructor
      IMPORTING
        !i_element_manager TYPE REF TO cl_extr3_element_manager .
    CLASS-METHODS clear .
    CLASS-METHODS get_instance
      IMPORTING
        !i_element_manager TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(r_instance)  TYPE REF TO cl_extr3_tables .
    METHODS add
      IMPORTING
        !table                TYPE string
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type .
    METHODS table_name
      IMPORTING
        !i_element_id   TYPE i
      RETURNING
        VALUE(r_result) TYPE tabname .

    METHODS collect_infos
        REDEFINITION .
    METHODS make_model
        REDEFINITION .
    METHODS name
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_tables.
    TYPES: BEGIN OF element_type,
             element_id TYPE cl_extr3_element_manager=>element_id_type,
             tabname    TYPE tabname,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_tabname TYPE HASHED TABLE OF element_type WITH UNIQUE KEY tabname.
ENDCLASS.
CLASS cl_extr3_web_dynpro_comp DEFINITION
  INHERITING FROM cl_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO cl_extr3_web_dynpro_comp.
    METHODS add
      IMPORTING
        wdy_component_name    TYPE wdy_component_name
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS add_component
      IMPORTING
        wdy_component_name    TYPE wdy_component_name
        wdy_controller_name   TYPE wdy_controller_name
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
    METHODS wdy_component_name
      IMPORTING
        element_id                TYPE i
      EXPORTING
        VALUE(wdy_component_name) TYPE wdy_component_name.
    METHODS wdy_controller_name
      IMPORTING
        element_id                 TYPE i
      EXPORTING
        VALUE(wdy_component_name)  TYPE wdy_component_name
        VALUE(wdy_controller_name) TYPE wdy_controller_name.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO cl_extr3_web_dynpro_comp.
    TYPES: BEGIN OF element_type,
             element_id         TYPE cl_extr3_element_manager=>element_id_type,
             wdy_component_name TYPE wdy_component_name,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_wdy_component_name TYPE HASHED TABLE OF element_type WITH UNIQUE KEY wdy_component_name.
    TYPES: BEGIN OF element_comp_type,
             element_id          TYPE cl_extr3_element_manager=>element_id_type,
             wdy_component_name  TYPE wdy_component_name,
             wdy_controller_name TYPE wdy_controller_name,
           END OF element_comp_type.
    DATA elements_comp_element_id TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY element_id.
    DATA elements_comp_comp_contr_name TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY wdy_component_name wdy_controller_name.
    METHODS _add_component
      IMPORTING
        wdy_component_name    TYPE wdy_component_name
        wdy_controller_name   TYPE wdy_controller_name
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE cl_extr3_element_manager=>element_id_type.
ENDCLASS.
CLASS cl_extr3_tadir_builder DEFINITION
  INHERITING FROM cl_extr3_association_build
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO cl_extr3_element_manager.
    METHODS search_down REDEFINITION.
    METHODS search_up REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: tables                TYPE REF TO cl_extr3_tables,
          classes               TYPE REF TO cl_extr3_classes,
          programs              TYPE REF TO cl_extr3_programs,
          web_dynpro_components TYPE REF TO cl_extr3_web_dynpro_comp,
          parent_package        TYPE REF TO cl_extr3_parent_package.
ENDCLASS.
CLASS cl_extr3_where_used_builder DEFINITION
  INHERITING FROM cl_extr3_association_build
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS seop .

    METHODS set_dynamic_read
      IMPORTING
        !i_dynamic_read TYPE string OPTIONAL .

    METHODS search_down
        REDEFINITION .
    METHODS search_up
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.


    TYPES:
      BEGIN OF wbcrossgt_type,
        otype    TYPE char2,
        name     TYPE eu_lname,
        include  TYPE programm,
        direct   TYPE sgrade,
        indirect TYPE sgrade,
      END OF wbcrossgt_type ,
      wbcrossgts_type TYPE SORTED TABLE OF wbcrossgt_type WITH UNIQUE KEY otype name include,

      BEGIN OF cross_type,
        type    TYPE char1,
        name    TYPE seu_name,
        include TYPE syrepid,
      END OF cross_type,
      cross_types TYPE STANDARD TABLE OF cross_type WITH DEFAULT KEY,

      BEGIN OF wbcrossi_type,
        name    TYPE eu_lname,
        include TYPE program,
        " Ignore field master, it is in most cases identical to field include
      END OF wbcrossi_type,
      wbcrossi_types TYPE STANDARD TABLE OF wbcrossi_type WITH DEFAULT KEY.

    DATA: g_dynamic_usage TYPE SORTED TABLE OF wbcrossgt WITH UNIQUE KEY otype name include.
ENDCLASS.
"! I build all initial elements that are the starting point for searching further elements.
CLASS cl_extr3_initial_elements DEFINITION
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_package,
             package       TYPE devclass,
             parentpackage TYPE parentcl,
           END OF ty_package.
    TYPES ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.

    CONSTANTS: select_class_method TYPE string VALUE 'Class',
               select_table        TYPE string VALUE 'Table',
               select_program      TYPE string VALUE 'Program',
               select_function     TYPE string VALUE 'Function'.

    TYPES: ty_s_pack TYPE RANGE OF tadir-devclass .
    TYPES:
      BEGIN OF ty_tdevc_test,
        devclass TYPE devclass,
        parentcl TYPE parentcl,
      END OF ty_tdevc_test.
    TYPES ty_t_tdevc_test TYPE HASHED TABLE OF ty_tdevc_test WITH UNIQUE KEY devclass.
    METHODS select_packages
      IMPORTING
        !top_packages           TYPE ty_s_pack
        !sub_packages_filter    TYPE ty_s_pack OPTIONAL
        !including_sub_packages TYPE abap_bool DEFAULT abap_false.
    TYPES: ty_filter TYPE string.
    METHODS select_specific
      IMPORTING
        model_builder         TYPE REF TO cl_extr3_model_builder
        element_manager       TYPE REF TO cl_extr3_element_manager
        i_element_type_filter TYPE ty_filter
        i_parent_name_filter  TYPE ty_filter
        i_name_filter         TYPE ty_filter.
    METHODS get_selected
      RETURNING VALUE(r_packages) TYPE ty_packages.

    "! @parameter tdevc_test | provide test data for table TDEVC during unit tests.
    METHODS constructor
      IMPORTING
        !tdevc_test TYPE ty_t_tdevc_test OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_package_store,
             package           TYPE devclass,
             parentpackage     TYPE parentcl,
             subclass_searched TYPE abap_bool,
             is_to_be_returned TYPE abap_bool,
           END OF ty_package_store.
    TYPES ty_packages_store TYPE HASHED TABLE OF ty_package_store WITH UNIQUE KEY package.

    DATA g_selected_packages TYPE ty_packages.
    "! Select packages according to filter transfered by report
    "! @parameter top_packages | Select packages
    "! @parameter sub_packages_filter | Optional: Include sub packages only if they are filtered by this filter
    "! @parameter including_sub_packages | Default false: Search sub packages
    "! Filled during tests
    DATA g_tdevc_test TYPE ty_t_tdevc_test.
    DATA g_is_test TYPE abap_bool.
    METHODS _select_top_packages
      IMPORTING
        i_top_packages    TYPE cl_extr3_packages=>ty_s_pack
      RETURNING
        VALUE(r_packages) TYPE cl_extr3_packages=>ty_packages.
    TYPES:
      ty_packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    METHODS _select_sub_packages
      IMPORTING
        i_packages_to_search_sub TYPE ty_packages_to_search_sub
      RETURNING
        VALUE(r_packages)        TYPE cl_extr3_packages=>ty_packages.
    METHODS _select_class
      IMPORTING
        name_filter           TYPE cl_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_class_method
      IMPORTING
        name_filter           TYPE cl_extr3_initial_elements=>ty_filter
        parent_name_filter    TYPE cl_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_table
      IMPORTING
        name_filter           TYPE cl_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO cl_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_program
      IMPORTING
        element_manager       TYPE REF TO cl_extr3_element_manager
        name_filter           TYPE cl_extr3_initial_elements=>ty_filter
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_function
      IMPORTING
        element_manager       TYPE REF TO cl_extr3_element_manager
        name_filter           TYPE cl_extr3_initial_elements=>ty_filter
      RETURNING
        VALUE(new_element_id) TYPE i.
ENDCLASS.
"! I know the level where an element was added to the model.
"! I know whether it was found in upward or downward search.
CLASS cl_extr3_model_builder DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF found_element_type,
             where            TYPE c LENGTH 1,
             level            TYPE i,
             "! Not zero in case an element is found in up and down search. If filled this is the level for downsearch
             alternate_level  TYPE i,
             element_type     TYPE string,
             parent_name      TYPE string,
             name             TYPE string,
             specific         TYPE abap_bool,
             up_search_done   TYPE abap_bool,
             down_search_done TYPE abap_bool,
           END OF found_element_type.
    TYPES: found_elements_type TYPE STANDARD TABLE OF found_element_type.
    METHODS search
      IMPORTING
        i_search_up   TYPE i
        i_search_down TYPE i.
    "! I am called once to notify that the initial selection of elements is started.
    "! All elements added to the model before my method search is called belong to the level 0
    METHODS initial_selection_started.
    "! Called whenever a new element ID was added to the model.
    "! @parameters i_is_specific | set to true if the element is added due to a specific search. It is for instance to be false, if all components of a class are added.
    METHODS new_element_id
      IMPORTING
        i_element_id  TYPE i
        i_is_specific TYPE abap_bool.
    METHODS initialize
      IMPORTING i_element_manager TYPE REF TO cl_extr3_element_manager
                i_dynamic_read    TYPE string OPTIONAL.
    METHODS write_found_elements
      IMPORTING
        write TYPE abap_bool OPTIONAL
      EXPORTING
        fes   TYPE found_elements_type.
    METHODS usage_of_single_element.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF found_in_level_type,
             element_id                  TYPE cl_extr3_element_manager=>element_id_type,
             "! A flag to mark all elements that are part of the initial selection
             found_in_initial_selection  TYPE abap_bool,
             "! Elements that where added when the main search is finished
             found_in_post_selection     TYPE abap_bool,
             "! Marks that an initially selected element is analyzed for lower and higher levels as requested
             initially_selected_analyzed TYPE abap_bool,
             "! The level where an element is first found. Needed to stop searching as specified.
             "! Also required to determine whether an upward or downward search will be done.
             found_in_level_upsearch     TYPE i,
             found_in_level_downsearch   TYPE i,
             "! Used to analyze usages of a single element.
             "! Marks an element that is specifically marked.
             "! In case a specific search is done, only elements with this flag are used for an where used analysis
             specific                    TYPE abap_bool,
           END OF found_in_level_type.
    TYPES found_in_levels_type TYPE HASHED TABLE OF found_in_level_type WITH UNIQUE KEY element_id.

    DATA: found_in_levels               TYPE found_in_levels_type,
          is_initial_selection          TYPE abap_bool,
          is_up_search                  TYPE abap_bool,
          "! Add newly found elements during upsearch in this level
          level_for_found_in_upsearch   TYPE i,
          is_down_search                TYPE abap_bool,
          "! Add newly found elements during downsearch in this level
          level_for_found_in_downsearch TYPE i,
          is_post_selection             TYPE abap_bool.

    TYPES: BEGIN OF builder_type,
             association_builder TYPE REF TO cl_extr3_association_build,
           END OF builder_type.
    DATA element_manager TYPE REF TO cl_extr3_element_manager.
    "! Use for initial search
    DATA association_builders_init TYPE STANDARD TABLE OF builder_type.
    "! Use for final search
    DATA association_builders_post TYPE STANDARD TABLE OF builder_type.
    "! Use for search
    DATA association_builders TYPE STANDARD TABLE OF builder_type.

    DATA: tadir_builder      TYPE REF TO cl_extr3_tadir_builder,
          where_used_builder TYPE REF TO cl_extr3_where_used_builder.
    "! A single element is analyzed of usage and using
    DATA is_usage_of_single_element TYPE abap_bool.
    METHODS _post_search.
    METHODS _initial_search.
    METHODS _search_up
      IMPORTING
        i_search_up TYPE i.
    METHODS _search_down
      IMPORTING
        i_search_down TYPE i.

ENDCLASS.
"! I am the starting point for an extraction. I am called from the main report.
class CL_EXTRACT3 definition
  final
  create public .

public section.

  types:
    ty_s_pack TYPE RANGE OF tadir-devclass .
  types:
    ty_string_range TYPE RANGE OF char45 .

  constants TECHTYPE_ABAPPACKAGE type STRING value 'ABAPPackage' ##NO_TEXT.
  constants TECHTYPE_ABAPMETHOD type STRING value 'ABAPMethod' ##NO_TEXT.
  constants TECHTYPE_ABAPCLASSATTRIBUTE type STRING value 'ABAPClassAttribute' ##NO_TEXT.
  constants TECHTYPE_ABAP_FUNCTION type STRING value 'ABAPFunktion' ##NO_TEXT.
  constants TECHTYPE_WEBDYNPRO_CONTROLLER type STRING value 'ABAPWebDynproController' ##NO_TEXT.
  constants MODIFIER_ABAPGLOBALCLASS type STRING value 'ABAPGlobalClass' ##NO_TEXT.
  constants MODIFIER_ABAPGLOBALINTERFACE type STRING value 'ABAPGlobalInterface' ##NO_TEXT.
  constants MODIFIER_WEBDYNPRO_COMPONENT type STRING value 'ABAPWebDynproComponent' ##NO_TEXT.
  constants MODIFIER_DBTABLE type STRING value 'DBTable' ##NO_TEXT.
  constants MODIFIER_PROGRAM type STRING value 'ABAPProgram' ##NO_TEXT.
  constants MODIFIER_FUNCTION_GROUP type STRING value 'ABAPFunktionGroup' ##NO_TEXT.
  constants MODIFIER_BW_TRANSFORMATION type STRING value 'BWTransformation' ##NO_TEXT.
  constants MODIFIER_UNKNOWN type STRING value 'UNKNOWN' ##NO_TEXT.

  class-methods CHECK_IF_TESTED
    returning
      value(IS_TESTED) type ABAP_BOOL .
  methods CONSTRUCTOR .
    "! Main start to do the extraction
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    "! @parameter i_exclude_found_sap_intf | exclude found interfaces in SAP namespace in the where-used analysis
  methods EXTRACT
    importing
      !MODEL_BUILDER type ref to CL_EXTR3_MODEL_BUILDER
      !ELEMENT_MANAGER type ref to CL_EXTR3_ELEMENT_MANAGER
      !INITIAL_ELEMENTS type ref to CL_EXTR3_INITIAL_ELEMENTS
      !I_SEARCH_UP type I
      !I_SEARCH_DOWN type I
      !I_EXCLUDE_FOUND_SAP_INTF type ABAP_BOOL
    exporting
      !MSE_MODEL type CL_MODEL=>LINES_TYPE
      value(NOTHING_DONE) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: g_check_for_test_done TYPE abap_bool,
                g_is_tested           TYPE abap_bool.
ENDCLASS.



CLASS CL_EXTR3_ACCESS IMPLEMENTATION.
  METHOD add.

    DATA association TYPE association_type.

    ASSERT accessed_element_id1 IS NOT INITIAL.
    ASSERT accessing_element_id2 IS NOT INITIAL.

    association-accessed_element_id1 = accessed_element_id1.
    association-accessing_element_id2 = accessing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-accessed_element_id1
                                                element_2   = association-accessing_element_id2
                                                association = me ).

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = access_ass..
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA using_method_id TYPE i.
    DATA using_id TYPE i.
    DATA used_id TYPE i.


    IF element_manager->use_somix EQ 'X'.
      _get_somix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_id    = using_id
                                              e_used_id     = used_id ).

      ASSERT using_id IS NOT INITIAL.
      ASSERT used_id IS NOT INITIAL.
    ELSE.
      _get_famix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_method_id = using_method_id
                                              e_used_id         = used_id ).

      ASSERT using_method_id IS NOT INITIAL.
      ASSERT used_id IS NOT INITIAL.
    ENDIF.

    DATA last_id2 TYPE i.
    IF element_manager->use_somix EQ 'X'.
      last_id2 = element_manager->somix_access->add( ).
      element_manager->somix_access->set_accessor_accessed_relation(
        EXPORTING
          element_id   = last_id2
          accessor_id  = using_id
          accessed_id  = used_id
          is_write     = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
          is_read      = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
          is_dependent = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
      ).
    ELSE.
      last_id2 = element_manager->famix_access->add( ).
      element_manager->famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id2
                                                                               accessor_id = using_method_id
                                                                               variable_id = used_id ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_ACCESS_OR_INVOCATN IMPLEMENTATION.
  METHOD _get_famix_id_used_and_using.

    DATA: invoced_element  TYPE REF TO cl_extr3_elements,
          invocing_element TYPE REF TO cl_extr3_elements.

    DATA used_id TYPE i.

    invoced_element = element_manager->get_element( i_element_id = i_association-element_id1 ).

    invocing_element = element_manager->get_element( i_element_id = i_association-element_id2 ).

    CASE invoced_element->type.
      WHEN invoced_element->class_type.
        DATA classes TYPE REF TO cl_extr3_classes.
        DATA: invoced_class_name TYPE string,
              invoced_cmpname    TYPE string,
              invoced_cmptype    TYPE seocmptype.

        classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = i_association-element_id1
                             IMPORTING class_name = invoced_class_name
                                       cmpname    = invoced_cmpname
                                       cmptype    = invoced_cmptype ).
        CASE invoced_cmptype.
          WHEN classes->attribute_type.

            e_used_id = element_manager->famix_attribute->get_id(  name_group = ng_abap_class
                                                                   class      = invoced_class_name
                                                                   attribute  = invoced_cmpname ).
          WHEN classes->method_type OR classes->event_type.
            e_used_id = element_manager->famix_method->get_id( class_name_group = ng_abap_class
                                                             class            = invoced_class_name
                                                             method_name_group = ng_abap_method
                                                             method           = invoced_cmpname ).
        ENDCASE.
      WHEN invoced_element->table_type.
        DATA tables TYPE REF TO cl_extr3_tables.
        DATA tabname TYPE tabname.
        tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).
        tabname = tables->table_name( i_element_id = i_association-element_id1 ).

        e_used_id = element_manager->famix_attribute->get_id(  name_group = ng_sap_table
                                                               class      = tabname
                                                               attribute  = tabname ).
      WHEN invoced_element->program_type.
        DATA programs2 TYPE REF TO cl_extr3_programs.
        DATA: invoced_ext_progr_name_class  TYPE string,
              invoced_ext_progr_name_method TYPE string.

        programs2 = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).
        programs2->program_name( EXPORTING i_element_id = i_association-element_id1
                                 IMPORTING external_program_name_class = invoced_ext_progr_name_class
                                           external_program_name_method = invoced_ext_progr_name_method ).

        e_used_id = element_manager->famix_method->get_id( class_name_group = ng_abap_program
                                                           class             = invoced_ext_progr_name_class
                                                           method_name_group = ng_abap_program
                                                           method            = invoced_ext_progr_name_method ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    DATA: invoicing_famix_class       TYPE string,
          invoicing_famix_method      TYPE string,
          invoicing_class_name_group  TYPE string,
          invoicing_method_name_group TYPE string.

    CASE invocing_element->type.
      WHEN invocing_element->class_type.

        invoicing_class_name_group = ng_abap_class.
        invoicing_method_name_group = ng_abap_method.

        DATA: invocing_class_name TYPE string,
              invocing_cmpname    TYPE string.

        classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).

        classes->comp_name( EXPORTING element_id  = i_association-element_id2
                             IMPORTING class_name = invocing_class_name
                                       cmpname    = invocing_cmpname ).

        invoicing_famix_class = invocing_class_name.
        invoicing_famix_method = invocing_cmpname.

      WHEN invocing_element->web_dynpro_comps_type.

        invoicing_class_name_group = ng_abap_webdynpro.
        invoicing_method_name_group = ng_abap_webdynpro.

        DATA web_dynpro_component TYPE REF TO cl_extr3_web_dynpro_comp.

        DATA: invocing_wdy_component_name  TYPE wdy_component_name,
              invocing_wdy_controller_name TYPE wdy_controller_name.


        web_dynpro_component = cl_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

        web_dynpro_component->wdy_controller_name( EXPORTING element_id          = i_association-element_id2
                                                   IMPORTING wdy_component_name  = invocing_wdy_component_name
                                                             wdy_controller_name = invocing_wdy_controller_name ).

        invoicing_famix_class = invocing_wdy_component_name.
        invoicing_famix_method = invocing_wdy_controller_name.

      WHEN invocing_element->program_type.

        invoicing_class_name_group = ng_abap_program.
        invoicing_method_name_group = ng_abap_program.

        DATA programs TYPE REF TO cl_extr3_programs.

        programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        programs->program_name( EXPORTING i_element_id          = i_association-element_id2
                                IMPORTING external_program_name_class = invoicing_famix_class
                                          external_program_name_method = invoicing_famix_method ).

      WHEN OTHERS.
        ASSERT 1 = 2.

    ENDCASE.

    DATA using_method_id TYPE i.
    ASSERT invoicing_class_name_group IS NOT INITIAL.
    ASSERT invoicing_method_name_group IS NOT INITIAL.
    e_using_method_id = element_manager->famix_method->get_id( class_name_group = invoicing_class_name_group
                                                               class  = invoicing_famix_class
                                                               method_name_group = invoicing_method_name_group
                                                               method = invoicing_famix_method ).

  ENDMETHOD.
  METHOD _get_somix_id_used_and_using.

    DATA: invoced_element  TYPE REF TO cl_extr3_elements,
          invocing_element TYPE REF TO cl_extr3_elements.

    DATA used_id TYPE i.

    invocing_element = element_manager->get_element( i_element_id = i_association-element_id2 ).

    invoced_element = element_manager->get_element( i_element_id = i_association-element_id1 ).

    CASE invoced_element->type.
      WHEN invoced_element->class_type.
        DATA classes TYPE REF TO cl_extr3_classes.
        DATA: invoced_class_name TYPE string,
              invoced_cmpname    TYPE string,
              invoced_cmptype    TYPE seocmptype.

        classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id  = i_association-element_id1
                             IMPORTING class_name = invoced_class_name
                                       cmpname    = invoced_cmpname
                                       cmptype    = invoced_cmptype ).
        CASE invoced_cmptype.
          WHEN classes->attribute_type.

            e_used_id = element_manager->somix_data->get_id(  grouping_name_group = ng_abap_class
                                                              grouping            = invoced_class_name
                                                              data_name_group     = ng_abap_attribute
                                                              data                = invoced_cmpname ).
          WHEN classes->method_type OR classes->event_type.
            e_used_id = element_manager->somix_code->get_id( grouping_name_group = ng_abap_class
                                                             grouping            = invoced_class_name
                                                             code_name_group     = ng_abap_method
                                                             code                = invoced_cmpname ).
        ENDCASE.
      WHEN invoced_element->table_type.
        DATA tables TYPE REF TO cl_extr3_tables.
        DATA tabname TYPE tabname.
        tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).
        tabname = tables->table_name( i_element_id = i_association-element_id1 ).

        e_used_id = element_manager->somix_data->get_id( grouping_name_group = ng_database_schema
                                                         grouping            = tables->database_schema
                                                         data_name_group     = ng_sap_table
                                                         data                = tabname ).

      WHEN invoced_element->program_type.
        DATA programs2 TYPE REF TO cl_extr3_programs.
        DATA: invoced_ext_progr_name_class  TYPE string,
              invoced_ext_progr_name_method TYPE string.

        programs2 = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).
        programs2->program_name( EXPORTING i_element_id = i_association-element_id1
                                 IMPORTING external_program_name_class = invoced_ext_progr_name_class
                                           external_program_name_method = invoced_ext_progr_name_method ).

        e_used_id = element_manager->somix_code->get_id( grouping_name_group = ng_abap_program
                                                         grouping            = invoced_ext_progr_name_class
                                                         code_name_group     = ng_abap_program
                                                         code                = invoced_ext_progr_name_method ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    DATA: invocing_somix_grouping      TYPE string,
          invocing_somix_code          TYPE string,
          invocing_grouping_name_group TYPE string,
          invocing_code_name_group     TYPE string.

    CASE invocing_element->type.
      WHEN invocing_element->class_type.

        invocing_grouping_name_group = ng_abap_class.
        invocing_code_name_group = ng_abap_method.

        DATA: invocing_class_name TYPE string,
              invocing_cmpname    TYPE string.

        classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).

        classes->comp_name( EXPORTING element_id = i_association-element_id2
                            IMPORTING class_name = invocing_class_name
                                      cmpname    = invocing_cmpname ).

        invocing_somix_grouping = invocing_class_name.
        invocing_somix_code = invocing_cmpname.

      WHEN invocing_element->web_dynpro_comps_type.

        invocing_grouping_name_group = ng_abap_webdynpro.
        invocing_code_name_group = ''. ##TODO " Evaluate using a code name group here "ng_abap_webdynpro.

        DATA web_dynpro_component TYPE REF TO cl_extr3_web_dynpro_comp.

        DATA: invocing_wdy_component_name  TYPE wdy_component_name,
              invocing_wdy_controller_name TYPE wdy_controller_name.


        web_dynpro_component = cl_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

        web_dynpro_component->wdy_controller_name( EXPORTING element_id          = i_association-element_id2
                                                   IMPORTING wdy_component_name  = invocing_wdy_component_name
                                                             wdy_controller_name = invocing_wdy_controller_name ).

        invocing_somix_grouping = invocing_wdy_component_name.
        invocing_somix_code = invocing_wdy_controller_name.

      WHEN invocing_element->program_type.

        invocing_grouping_name_group = ng_abap_program.
        invocing_code_name_group = ng_abap_program.

        DATA programs TYPE REF TO cl_extr3_programs.

        programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        programs->program_name( EXPORTING i_element_id          = i_association-element_id2
                                IMPORTING external_program_name_class = invocing_somix_grouping
                                          external_program_name_method = invocing_somix_code ).

      WHEN OTHERS.
        ASSERT 1 = 2.

    ENDCASE.

    DATA using_method_id TYPE i.
    ASSERT invocing_grouping_name_group IS NOT INITIAL.
    ##TODO " Can this be activated again? ASSERT invocing_code_name_group IS NOT INITIAL.

    e_using_id = element_manager->somix_code->get_id( grouping_name_group = invocing_grouping_name_group
                                                      grouping            = invocing_somix_grouping
                                                      code_name_group     = invocing_code_name_group
                                                      code                = invocing_somix_code ).

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_ASSOCIATION IMPLEMENTATION.
  METHOD make_model.
    " I must be redefined
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_INVOCATION IMPLEMENTATION.
  METHOD add.

    DATA association TYPE association_type.

    ASSERT invoced_element_id1 IS NOT INITIAL.
    ASSERT invocing_element_id2 IS NOT INITIAL.

    association-invoced_element_id1 = invoced_element_id1.
    association-invocing_element_id2 = invocing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-invoced_element_id1
                                                element_2   = association-invocing_element_id2
                                                association = me ).

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = invocation_ass.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA using_id TYPE i.
    DATA used_id TYPE i.



    IF element_manager->use_somix EQ 'X'.
      _get_somix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_id    = using_id
                                              e_used_id     = used_id ).
    ELSE.
      _get_famix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_method_id = using_id
                                              e_used_id         = used_id ).
    ENDIF.

    ASSERT using_id IS NOT INITIAL.
    ASSERT used_id IS NOT INITIAL.

    IF element_manager->use_somix EQ 'X'.
      DATA call_id TYPE i.
      call_id = element_manager->somix_call->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id   = call_id
          caller_id  = using_id
          called_id  = used_id ).
    ELSE.
      DATA invocation_id TYPE i.
      invocation_id = element_manager->famix_invocation->add( ).
      element_manager->famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                 sender_id     = using_id
                                                                 candidates_id = used_id
                                                                 signature     = 'DUMMY' ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_PARENT_PACKAGE IMPLEMENTATION.
  METHOD add.

    DATA association TYPE association_type.

    association-element_id1 = element_id.
    association-element_id2 = parent_element_id.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-element_id1
                                                element_2   = association-element_id2
                                                association = me ).

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = parent_package_ass.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.
    " I am added in the elements them self to the model
  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_ASSOCIATION_BUILD IMPLEMENTATION.
  METHOD search_down.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD search_up.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_TADIR_BUILDER IMPLEMENTATION.
  METHOD constructor.

    super->constructor( i_element_manager = i_element_manager ).

    parent_package = cl_extr3_parent_package=>get_instance( i_element_manager = element_manager ).
    classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
    programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).
    tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).
    web_dynpro_components = cl_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

  ENDMETHOD.
  METHOD search_down.

    DATA: element            TYPE REF TO cl_extr3_elements,
          package            TYPE REF TO cl_extr3_packages,
          is_found           TYPE abap_bool,
          new_element_id     TYPE cl_extr3_element_manager=>element_id_type,
          class_name         TYPE string,
          tabname            TYPE string,
          function_group     TYPE string,
          program            TYPE progname,
          wdy_component_name TYPE wdy_component_name.

    element = element_manager->get_element( element_id ).

    IF element->type EQ element->package_type.

      package ?= element.

      DATA devclass TYPE devclass.

      devclass = package->devclass( element_id ).

      TYPES: BEGIN OF ty_tadir,
               pgmid    TYPE pgmid,
               object   TYPE trobjtype,
               obj_name TYPE sobj_name,
             END OF ty_tadir.

      DATA: tadir  TYPE ty_tadir,
            tadirs TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

      SELECT pgmid object obj_name FROM tadir INTO TABLE tadirs WHERE devclass = devclass.

      LOOP AT tadirs INTO tadir.

        is_found = abap_false.

        CASE tadir-pgmid.
          WHEN 'R3TR'.
            CASE tadir-object.
              WHEN 'CLAS' OR 'INTF'.

                class_name = tadir-obj_name.
                classes->add( EXPORTING class          = class_name
                                        is_specific    = abap_true
                              IMPORTING is_added       = is_found
                                        new_element_id = new_element_id ).

              WHEN 'DEVC'.
              WHEN 'FUGR'.

                function_group = tadir-obj_name.
                programs->add_function_group( EXPORTING fgr            = function_group
                                              IMPORTING is_added       = is_found
                                                        new_element_id = new_element_id ).
              WHEN 'PROG'.

                program = tadir-obj_name.
                programs->add( EXPORTING program        = program
                               IMPORTING is_added       = is_found
                                         new_element_id = new_element_id ).

              WHEN 'TABL'.

                tabname = tadir-obj_name.
                tables->add( EXPORTING table          = tabname
                             IMPORTING is_added       = is_found
                                       new_element_id = new_element_id ).

              WHEN 'WDYN'.

                wdy_component_name = tadir-obj_name.
                web_dynpro_components->add( EXPORTING wdy_component_name = wdy_component_name
                                            IMPORTING is_added       = is_found
                                                      new_element_id = new_element_id ).

              WHEN OTHERS.
                " TBD handle
            ENDCASE.

          WHEN OTHERS.
            "TBD handle
        ENDCASE.

        IF is_found EQ abap_true.

          parent_package->add( EXPORTING element_id        = new_element_id
                                         parent_element_id = element_id ).

        ENDIF.


      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD search_up.

    DATA: element        TYPE REF TO cl_extr3_elements,
          package        TYPE REF TO cl_extr3_packages,
          is_found       TYPE abap_bool,
          new_element_id TYPE cl_extr3_element_manager=>element_id_type,
          class_name     TYPE string,
          clstype        TYPE seoclstype,
          tabname        TYPE tabname.

    package = cl_extr3_packages=>get_instance( i_element_manager = element_manager ).

    element = element_manager->get_element( element_id ).

    DATA: object   TYPE trobjtype,
          obj_name TYPE sobj_name.

    CLEAR object.
    CLEAR obj_name.

    CASE element->type.
      WHEN element->class_type.
        classes->class_name( EXPORTING element_id = element_id
                             IMPORTING class_name = class_name
                                       clstype    = clstype ).
        obj_name = class_name.
        IF clstype EQ classes->is_class_type.
          object = 'CLAS'.
        ELSEIF clstype EQ classes->interface_type.
          object = 'INTF'.
        ELSE.
          ASSERT 1 = 2.
        ENDIF.
      WHEN element->table_type.
        tabname = tables->table_name( i_element_id = element_id ).
        object = 'TABL'.
        obj_name = tabname.
      WHEN element->program_type.
        DATA program_type TYPE string.
        DATA program TYPE progname.
        DATA program_attribute_1 TYPE string.
        programs->program_name(
          EXPORTING
            i_element_id                 = element_id
          IMPORTING
            program_type                 = program_type
            program                      = program
*            external_program_name_class  = external_program_name_class
*            external_program_name_method =
            program_attribute_1          = program_attribute_1
*            subc                         =
        ).
        IF program_type EQ programs->type_function OR program_type EQ programs->type_function_include.
          object = 'FUGR'.
          obj_name = program_attribute_1.
        ELSEIF program_type EQ programs->type_program.
          object = 'PROG'.
          obj_name = program.
        ENDIF.

    ENDCASE.

    TYPES: BEGIN OF ty_tadir,
             devclass TYPE devclass,
           END OF ty_tadir.

    DATA: tadir  TYPE ty_tadir,
          tadirs TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.
    IF object IS NOT INITIAL.
      SELECT devclass FROM tadir INTO TABLE tadirs WHERE pgmid = 'R3TR'
                                                     AND object = object
                                                     AND obj_name = obj_name.
      IF sy-subrc EQ 0.
        LOOP AT tadirs INTO tadir.

          package->add( EXPORTING package        = tadir-devclass
                        IMPORTING is_added       = is_found
                                  new_element_id = new_element_id ).

          IF is_found EQ abap_true.

            parent_package->add( EXPORTING element_id        = element_id
                                           parent_element_id = new_element_id ).

          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_WHERE_USED_BUILDER IMPLEMENTATION.
  METHOD search_down.

    DATA: element TYPE REF TO cl_extr3_elements.

    element = element_manager->get_element( element_id ).

    DATA invocation TYPE REF TO cl_extr3_invocation.
    invocation = cl_extr3_invocation=>get_instance( i_element_manager = element_manager ).


    DATA: include_name TYPE syrepid.

    CASE element->type.
      WHEN element->class_type.
        "TBD


        DATA: class_name TYPE string,
              cmpname    TYPE string,
              cmptype    TYPE seocmptype,
              exists     TYPE abap_bool.


        DATA classes TYPE REF TO cl_extr3_classes.
        classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
        classes->comp_name( EXPORTING element_id = element_id
              IMPORTING
                class_name = class_name
                cmpname    = cmpname
                cmptype    = cmptype
                exists     = exists
        ).

        IF exists EQ abap_false.
          RETURN.
        ENDIF.

        DATA: class_key TYPE seoclskey.
        DATA: includes    TYPE    seop_methods_w_include.
        class_key-clsname = class_name.

        IF cmptype EQ '1' OR cmptype EQ '2'.
          DATA clsname  TYPE seoclsname.
          clsname = class_key.
          CALL METHOD cl_oo_classname_service=>get_all_method_includes(
            EXPORTING
              clsname            = clsname
*             with_enhancements  =     " X = searches also for enh.method includes
            RECEIVING
              result             = includes
            EXCEPTIONS
              class_not_existing = 1
              OTHERS             = 2
                                   ).
          IF sy-subrc <> 0.
            " TBD Better error handling
            RETURN.
          ENDIF.

          " This method is described to be obsolete
*          CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
*            EXPORTING
*              clskey                       = class_key
*            IMPORTING
*              includes                     = includes
*            EXCEPTIONS
*              _internal_class_not_existing = 1
*              OTHERS                       = 2.
*          IF sy-subrc <> 0.
*            " TBD Better error handling
*            RETURN.
*          ENDIF.
          DATA: include TYPE LINE OF seop_methods_w_include.
          READ TABLE includes INTO include WITH KEY cpdkey-cpdname = cmpname.
          IF sy-subrc <> 0.
            " TBD ?
            RETURN.
          ELSE.

            "SAP_2_FAMIX_78 Provide downsearch for class methods

            include_name = include-incname.

          ENDIF.

        ELSE.
          RETURN.
        ENDIF.

*        CASE cmptype.
*          WHEN '1'. "Method
*          WHEN '2'. "Even
*        ENDCASE.

      WHEN element->table_type.
        "Is this needed?
      WHEN element->program_type.

        " Duplicate coding 1/2 see method search_up

        DATA programs2 TYPE REF TO cl_extr3_programs.
        programs2 = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        DATA: program             TYPE progname,
              program_type        TYPE string,
              program_attribute_2 TYPE string.



        programs2->program_name(
          EXPORTING
            i_element_id                 = element_id
          IMPORTING
            program                      = program
            program_type                 = program_type
            program_attribute_2          = program_attribute_2
*            external_program_name_class  =
*            external_program_name_method =
*            subc                         =
        ).
        CASE program_type.
          WHEN programs2->type_program.

            " SAP_2_FAMIX_73 Provide downsearch for programs

            include_name = program.

          WHEN programs2->type_function.

            include_name = program.

          WHEN OTHERS.
            " TBD
        ENDCASE.

    ENDCASE.

    IF include_name IS NOT INITIAL.

      DATA: wbcrossgts TYPE wbcrossgts_type,
            wbcrossgt  TYPE wbcrossgt_type,
            cross      TYPE cross_type,
            crosss     TYPE cross_types,
            wbcrossi   TYPE wbcrossi_type,
            wbcrossis  TYPE wbcrossi_types.

      SELECT otype name include direct indirect
        FROM wbcrossgt
        INTO TABLE wbcrossgts
        WHERE include = include_name.

      SELECT type name include FROM cross INTO TABLE crosss
        WHERE include = include_name.

      SELECT name include FROM wbcrossi INTO TABLE wbcrossis
        WHERE include = include_name.

      " Read dynamic usages

      DATA: w TYPE wbcrossgt.
      LOOP AT g_dynamic_usage INTO w
        WHERE include = include_name.
        MOVE-CORRESPONDING w TO wbcrossgt.
        INSERT wbcrossgt INTO TABLE wbcrossgts.
      ENDLOOP.

      LOOP AT crosss INTO cross.

        DATA: is_added        TYPE abap_bool,
              uses_element_id TYPE cl_extr3_element_manager=>element_id_type.

        CLEAR is_added.

        DATA programs TYPE REF TO cl_extr3_programs.
        programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        IF cross-type EQ 'R' OR cross-type EQ 'F'.
          DATA: program_found  TYPE progname.
          IF cross-type EQ 'R'.
            " SAP_2_FAMIX_67 Find programs in down search
            program_found = cross-name.
            programs->add( EXPORTING program        = program_found
                           IMPORTING is_added       = is_added
                                     new_element_id = uses_element_id ).
          ELSEIF cross-type EQ 'F'.
            " SAP_2_FAMIX_72 Find functions in down search
            DATA function TYPE string.
            function = cross-name.
            programs->add_function( EXPORTING function       = function
                                    IMPORTING is_added       = is_added
                                              new_element_id = uses_element_id ).
          ELSE.
            ASSERT 1 = 2.
          ENDIF.

          IF uses_element_id IS INITIAL.
            "TBD support this kind of elements
            CONTINUE.
          ENDIF.

          IF is_added EQ abap_true.

            element_manager->model_builder->new_element_id( EXPORTING i_element_id  = uses_element_id
                                                                      i_is_specific = abap_true ).

          ELSE.
            "TBD what is to be done here?

          ENDIF.
          invocation->add( EXPORTING invoced_element_id1  = uses_element_id
                                     invocing_element_id2 = element_id ).

*        ELSEIF cross-type EQ 'F'.
*          " TBD functions
        ELSE.
          " TBD ?
        ENDIF.

      ENDLOOP.

      LOOP AT wbcrossis INTO wbcrossi.

        CLEAR is_added.

        programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        " SAP_2_FAMIX_70 Find includes in downsearch

        " This is an include, but it is reported as program, TBD change this
        program_found = wbcrossi-name.
        programs->add( EXPORTING program        = program_found
                       IMPORTING is_added       = is_added
                                 new_element_id = uses_element_id ).

        IF uses_element_id IS INITIAL.
          "TBD support this kind of elements
          CONTINUE.
        ENDIF.

        IF is_added EQ abap_true.

          element_manager->model_builder->new_element_id( EXPORTING i_element_id  = uses_element_id
                                                                    i_is_specific = abap_true ).

        ELSE.
          "TBD what is to be done here?

        ENDIF.

        invocation->add( EXPORTING invoced_element_id1  = uses_element_id
                                   invocing_element_id2 = element_id ).

      ENDLOOP.

      LOOP AT wbcrossgts INTO wbcrossgt.

        CLEAR is_added.

        CASE wbcrossgt-otype.
          WHEN 'ME'.
            DATA: class  TYPE string,
                  method TYPE string.
            SPLIT wbcrossgt-name AT '\ME:' INTO class method.

            DATA: part1 TYPE string,
                  part2 TYPE string.

            SPLIT class AT '\IN:' INTO part1 part2.

            IF part2 IS NOT INITIAL.
              CONTINUE." TBD specify this better
            ENDIF.

            DATA: temp TYPE string.
            temp = class && |~| && method.

            IF cmpname EQ temp. " Implementation of interface methods are in the where used list. These are added explicitely in the class coding. So filter here.

              CONTINUE.

            ENDIF.

            classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).

            " SAP_2_FAMIX_75 Find class methods in downsearch

            ASSERT class IS NOT INITIAL.
            TRY.
                classes->add_component(
                  EXPORTING
                    clsname        = class
                    cmpname        = method
                    is_specific    = abap_true
                  IMPORTING
                    is_added       = is_added
                    new_element_id = uses_element_id ).

                IF uses_element_id IS INITIAL.
                  "TBD support this kind of elements
                  CONTINUE.
                ENDIF.
              CATCH zcx_2mse_extr3_classes_wr_type.
                " This attribute is indeed a type, but types are not stored to the model
                CONTINUE.
            ENDTRY.

            IF is_added EQ abap_true.

              element_manager->model_builder->new_element_id( EXPORTING i_element_id  = uses_element_id
                                                                        i_is_specific = abap_true ).

            ELSE.
              "TBD what is to be done here?

            ENDIF.

            DATA: is_redefinition_of_method TYPE abap_bool.

            is_redefinition_of_method = classes->is_redefinition_of_method( invoced_element_id1  = uses_element_id
                                                                            invocing_element_id2 = element_id ).

            IF is_redefinition_of_method EQ abap_true.

              invocation->add( EXPORTING invoced_element_id1  = element_id
                                         invocing_element_id2 = uses_element_id ).
            ELSE.

              invocation->add( EXPORTING invoced_element_id1  = uses_element_id
                                         invocing_element_id2 = element_id ).

            ENDIF.

          WHEN 'DA'.

            DATA: attribute TYPE string.

            SPLIT wbcrossgt-name AT '\DA:' INTO class attribute.

            SPLIT class AT '\ME:' INTO part1 part2.

            IF part2 IS NOT INITIAL.
              CONTINUE." TBD specify this better
            ENDIF.

            SPLIT class AT '\IN:' INTO part1 part2.

            IF part2 IS NOT INITIAL.
              CONTINUE." TBD specify this better
            ENDIF.

            IF attribute IS NOT INITIAL.

              classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).

              " SAP_2_FAMIX_76 Find class attributes in downsearch
              ASSERT class IS NOT INITIAL.
              TRY.
                  classes->add_component(
                    EXPORTING
                      clsname        = class
                      cmpname        = attribute
                      is_specific    = abap_false
                    IMPORTING
                      is_added       = is_added
                      new_element_id = uses_element_id ).

                  IF uses_element_id IS INITIAL.
                    "TBD support this kind of elements
                    CONTINUE.
                  ENDIF.
                CATCH zcx_2mse_extr3_classes_wr_type.
                  " This attribute is indeed a type, but types are not stored to the model
                  CONTINUE.
              ENDTRY.

              IF is_added EQ abap_true.

                element_manager->model_builder->new_element_id( EXPORTING i_element_id  = uses_element_id
                                                                          i_is_specific = abap_true ).

              ELSE.
                "TBD what is to be done here?

              ENDIF.

              DATA access TYPE REF TO cl_extr3_access.
              access = cl_extr3_access=>get_instance( i_element_manager = element_manager ).

              IF uses_element_id IS NOT INITIAL. " This may be some irregular object, that is not a class attribute
                access->add( EXPORTING accessed_element_id1  = uses_element_id
                                       accessing_element_id2 = element_id ).
              ENDIF.

            ENDIF.

          WHEN 'TY'.

            DATA tabclass TYPE tabclass.

            SELECT SINGLE tabclass FROM dd02l INTO tabclass WHERE tabname = wbcrossgt-name.

            IF sy-subrc EQ 0.

              DATA tables TYPE REF TO cl_extr3_tables.
              tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).
              DATA: new_table TYPE string.
              new_table = wbcrossgt-name.

              " SAP_2_FAMIX_77 Find database tables in downsearch

              tables->add( EXPORTING table          = new_table
                           IMPORTING new_element_id = uses_element_id
                                     is_added       = is_added ).

              IF uses_element_id IS INITIAL.
                "TBD support this kind of elements
                CONTINUE.
              ENDIF.

              IF is_added EQ abap_true.

                element_manager->model_builder->new_element_id( EXPORTING i_element_id  = uses_element_id
                                                                          i_is_specific = abap_true ).

              ELSE.
                "TBD what is to be done here?

              ENDIF.
              IF uses_element_id IS NOT INITIAL.
                access = cl_extr3_access=>get_instance( i_element_manager = element_manager ).

                access->add( EXPORTING accessed_element_id1  = uses_element_id
                                       accessing_element_id2 = element_id ).
              ENDIF.

            ENDIF.

        ENDCASE.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD search_up.

    DATA: element   TYPE REF TO cl_extr3_elements,
          is_access TYPE abap_bool.

    element = element_manager->get_element( element_id ).

    DATA classes TYPE REF TO cl_extr3_classes.
    classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).

    DATA access TYPE REF TO cl_extr3_access.
    access = cl_extr3_access=>get_instance( i_element_manager = element_manager ).

    DATA invocation TYPE REF TO cl_extr3_invocation.
    invocation = cl_extr3_invocation=>get_instance( i_element_manager = element_manager ).

    CASE element->type.
      WHEN element->class_type.
        DATA class_name TYPE string.
        DATA cmpname TYPE string.
        DATA cmptype TYPE seocmptype.
        DATA clstype  TYPE seoclstype.
        DATA exists TYPE abap_bool.

        IF element_manager->exclude_found_sap_intf EQ abap_true.

          classes->class_name( EXPORTING element_id = element_id
                               IMPORTING class_name = class_name
                                         clstype    = clstype
                                         exists     = exists ).
          IF exists EQ abap_true.

            IF clstype EQ classes->interface_type.
              IF class_name CP 'Y*'
              OR class_name CP 'Z*'
              OR class_name CP '/*' .
                " OK
              ELSE.

                " Do not collect, it should be OK just to leave the method here
                RETURN.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

        classes->comp_name( EXPORTING element_id = element_id
                            IMPORTING class_name = class_name
                                      cmpname    = cmpname
                                      cmptype = cmptype ).

        IF element_manager->exclude_found_sap_intf EQ abap_true AND class_name CP 'IF*'.

          " Do not collect, it should be OK just to leave the method here
          RETURN.

        ENDIF.

        DATA: otype           TYPE char2,
              where_used_name TYPE eu_lname.

        CASE cmptype.
          WHEN classes->method_type.
            otype = 'ME'.
          WHEN classes->attribute_type.
            otype = 'DA'.

            is_access = abap_true.

          WHEN classes->event_type.
            otype = 'EV'.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        FIND '~' IN cmpname.

        IF sy-subrc <> 0.

          where_used_name = class_name && |\\| && otype && |:| && cmpname.

        ELSE.

          DATA: interface_name TYPE string,
                method_name    TYPE string.

          SPLIT cmpname AT '~' INTO interface_name method_name.

          where_used_name = class_name && |\\IN:| && interface_name && |\\| && otype && |:| && method_name.

        ENDIF.

      WHEN element->table_type.

        DATA tables TYPE REF TO cl_extr3_tables.
        tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).

        DATA table TYPE tabname.

        table = tables->table_name( i_element_id = element_id ).

        otype = 'TY'.

        where_used_name = table.

        is_access = abap_true.

      WHEN element->program_type.

        " Duplicate coding 2/2 see method search_down

        DATA programs2 TYPE REF TO cl_extr3_programs.
        programs2 = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

        DATA: program             TYPE progname,
              program_type        TYPE string,
              program_attribute_2 TYPE string.



        programs2->program_name(
          EXPORTING
            i_element_id                 = element_id
          IMPORTING
            program                      = program
            program_type                 = program_type
            program_attribute_2          = program_attribute_2
*            external_program_name_class  =
*            external_program_name_method =
*            subc                         =
        ).

        DATA: cross_type TYPE char1,
              cross_name TYPE seu_name.
        CASE program_type.
          WHEN programs2->type_program.
            cross_type = 'R'.
            cross_name = program.
          WHEN programs2->type_function.
            cross_type = 'F'.
            cross_name = program_attribute_2.
          WHEN OTHERS.
            " TBD
        ENDCASE.


    ENDCASE.

    IF where_used_name IS NOT INITIAL OR cross_name IS NOT INITIAL.

      DATA: wbcrossgts TYPE wbcrossgts_type,
            wbcrossgt  TYPE wbcrossgt_type,
            cross      TYPE cross_type,
            crosss     TYPE cross_types.

      IF otype IS NOT INITIAL.

        SELECT otype name include direct indirect
          FROM wbcrossgt
          INTO TABLE wbcrossgts
          WHERE otype = otype
            AND name = where_used_name.

      ENDIF.

      IF cross_type IS NOT INITIAL.

        SELECT type name include FROM cross INTO TABLE crosss
          WHERE type = cross_type
            AND name = cross_name.

        LOOP AT crosss INTO cross.

          " Most where used information is in table wbcrossgt. Now some information is read from table cross. Use nonetheless wbcrossgt
          " for further processing. There is an integration test, that assures the correctness of the whole coding.

          CLEAR wbcrossgt.

          wbcrossgt-name = cross-name.
          wbcrossgt-include = cross-include.

          INSERT wbcrossgt INTO TABLE wbcrossgts.

        ENDLOOP.

      ENDIF.

      " Read dynamic usages

      DATA: w TYPE wbcrossgt.
      LOOP AT g_dynamic_usage INTO w
        WHERE otype = otype
          AND name = where_used_name.
        MOVE-CORRESPONDING w TO wbcrossgt.
        INSERT wbcrossgt INTO TABLE wbcrossgts.
      ENDLOOP.

      LOOP AT wbcrossgts INTO wbcrossgt.

        DATA: is_added           TYPE abap_bool,
              used_by_element_id TYPE cl_extr3_element_manager=>element_id_type.

        CLEAR is_added.

        " Analyze where used table
        DATA: pgmid    TYPE pgmid,
              object   TYPE trobjtype,
              obj_name TYPE trobj_name.

        DATA found_class_name TYPE string.
        DATA found_cmpname TYPE string.

        cl_oo_include_naming=>get_trkey_by_include(
          EXPORTING
            progname        = wbcrossgt-include
          IMPORTING
            pgmid           = pgmid
            object          = object
            obj_name        = obj_name
          EXCEPTIONS
            no_objecttype   = 1
            invalid_include = 2
            internal_error  = 3
            OTHERS          = 4
        ).
        IF sy-subrc EQ 0.

          CASE object.
            WHEN 'METH'.
              found_class_name = obj_name+0(30).
              " TBD include code here?
              found_cmpname = obj_name+30(61).

              DATA: temp TYPE string.
              temp = class_name && |~| && cmpname.

              IF found_cmpname <> temp. " Implementation of interface methods are in the where used list. These are added explicitely in the class coding. So filter here.
                TRY.
                    classes->add_component(
                      EXPORTING
                        clsname        = found_class_name
                        cmpname        = found_cmpname
                        is_specific    = abap_false
                      IMPORTING
                        is_added       = is_added
                        new_element_id = used_by_element_id ).
                    IF is_added EQ abap_true.

                      element_manager->model_builder->new_element_id( EXPORTING i_element_id  = used_by_element_id
                                                                                i_is_specific = abap_true ).

                    ELSE.
                      "TBD what is to be done here?

                    ENDIF.
                  CATCH zcx_2mse_extr3_classes_wr_type.
                    " This attribute is indeed a type, but types are not stored to the model
                    " Do nothing to ignore this attribute
                ENDTRY.

              ENDIF.

            WHEN OTHERS.
              " TBD is code required here?
          ENDCASE.
*          <include_2_component>-is_class_component = abap_true.

        ELSE.


          "Check for usage in Web Dynpro ABAP
          DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.


            SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = wbcrossgt-include AND srtf2 = 0.


          IF sy-subrc EQ 0.
            DATA web_dynpro_component TYPE REF TO cl_extr3_web_dynpro_comp.
            web_dynpro_component = cl_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

            web_dynpro_component->add_component( EXPORTING wdy_component_name  = ls_wd_sourcemap-component_name
                                                           wdy_controller_name = ls_wd_sourcemap-controller_name
                                                 IMPORTING is_added            = is_added
                                                           new_element_id      = used_by_element_id ).

          ELSE.

            DATA programs TYPE REF TO cl_extr3_programs.
            programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).

            programs->add( EXPORTING program        = wbcrossgt-include
                           IMPORTING is_added       = is_added
                                     new_element_id = used_by_element_id ).

          ENDIF.

        ENDIF.

        IF is_added EQ abap_true.

          IF is_access EQ abap_true.

            access->add( EXPORTING accessed_element_id1  = element_id
                                   accessing_element_id2 = used_by_element_id ).

          ELSE.

            DATA: is_redefinition_of_method TYPE abap_bool.

            is_redefinition_of_method = classes->is_redefinition_of_method( invoced_element_id1  = element_id
                                                                            invocing_element_id2 = used_by_element_id ).

            IF is_redefinition_of_method EQ ''.

              invocation->add( EXPORTING invoced_element_id1  = element_id
                                         invocing_element_id2 = used_by_element_id ).

            ELSE.

              invocation->add( EXPORTING invoced_element_id1  = used_by_element_id
                                         invocing_element_id2 = element_id ).

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD set_dynamic_read.

    DATA: s     TYPE string,
          class TYPE string,
          t     TYPE TABLE OF string,
          w2    TYPE wbcrossgt.

    CLEAR g_dynamic_usage.

    s =  i_dynamic_read.
    TRANSLATE s TO UPPER CASE.
    CONDENSE s.
    SPLIT s AT space INTO TABLE t.

    LOOP AT t INTO class.

      DATA dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY.

      CALL METHOD (class)=>where_used
        IMPORTING
          data = dyn_usage.

      LOOP AT dyn_usage INTO w2.

        INSERT w2 INTO TABLE g_dynamic_usage.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_CLASSES IMPLEMENTATION.
  METHOD add.

    DATA: element      TYPE element_type,
          element_comp TYPE element_comp_type.

    READ TABLE elements_class_name INTO element WITH KEY class_name = class.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
      LOOP AT elements_comp_clsname_cmpname INTO element_comp WHERE clsname = class.
        DATA class_component TYPE cl_extr3_classes=>ty_class_component.
        class_component-clsname = element_comp-clsname.
        class_component-cmpname = element_comp-cmpname.
        class_component-cmptype = element_comp-cmptype.
        class_component-mtdtype = element_comp-mtdtype.
        INSERT class_component INTO TABLE class_components.
      ENDLOOP.
    ELSE.

      " Does table exists?
      DATA: found_class_name TYPE seoclsname,
            found_class_type TYPE seoclstype.

        " No blank between ( and found... to be 7.02 compatible
        SELECT SINGLE clsname clstype FROM seoclass INTO (found_class_name , found_class_type ) WHERE clsname = class.

      IF found_class_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = is_specific ).
        element-element_id = new_element_id.
        element-class_name = class.
        element-clstype = found_class_type.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_class_name.

      ENDIF.


        SELECT clsname cmpname cmptype mtdtype
          FROM seocompo
          INTO CORRESPONDING FIELDS OF TABLE class_components
          WHERE cmptype <> 3 " A type
            AND clsname = class.


      " Redefined methods may be in table SEOREDEF

      DATA: redefined_methods TYPE STANDARD TABLE OF seoredef WITH DEFAULT KEY,
            rm                TYPE seoredef.

      SELECT * FROM seoredef INTO TABLE redefined_methods WHERE clsname = class.
      DATA: cp TYPE ty_class_component.
      LOOP AT redefined_methods INTO rm.
        CLEAR cp.
        cp-clsname = class.
        cp-cmpname = rm-mtdname.
        cp-cmptype = 1. " Method
        cp-mtdtype = 0. " Method
        APPEND cp TO class_components.
      ENDLOOP.



      DATA: redefined_class_components TYPE ty_class_components,
            redefined_class_component  TYPE ty_class_component.

      redefined_class_components = _get_redefined( class ).

      LOOP AT redefined_class_components INTO redefined_class_component.
        INSERT redefined_class_component INTO TABLE class_components.
      ENDLOOP.

      SORT class_components.
      DELETE ADJACENT DUPLICATES FROM class_components.

      LOOP AT class_components INTO class_component.

        _add_component( EXPORTING clsname        = class_component-clsname
                                  cmpname        = class_component-cmpname
                                  is_specific    = is_specific ).

      ENDLOOP.

      _add_metarel( clsname        = class
                    is_specific    = is_specific ). ##TODO " Is it correct to set this specific?

    ENDIF.

  ENDMETHOD.
  METHOD add_component.

    DATA: is_added_now TYPE abap_bool.

    add( EXPORTING class          = clsname
                    is_specific   = is_specific
         IMPORTING is_added       = is_added ).

    IF is_added EQ abap_true.

      _add_component( EXPORTING clsname        = clsname
                                cmpname        = cmpname
                                is_specific    = is_specific
                      IMPORTING is_added       = is_added
                                new_element_id = new_element_id
                                is_added_now   = is_added_now ).

      IF is_specific EQ abap_true
      AND new_element_id IS NOT INITIAL.

        element_manager->model_builder->new_element_id( EXPORTING i_element_id  = new_element_id
                                                                  i_is_specific = abap_true ).

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD class_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.

    IF sy-subrc EQ 0.

      class_name = element-class_name.
      clstype = element-clstype.
      exists = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD collect_infos.

    DATA: part1            TYPE string,
          part2_classes    TYPE string,
          part2_interfaces TYPE string,
          name             TYPE string,
          part3            TYPE string.

    " Get ADT Link to class or interface

    CONCATENATE 'adt://' sysid '/sap/bc/adt/oo/' INTO part1.

    part2_classes = 'classes/'.

    part2_interfaces = 'interfaces/'.

    part3 = '/source/main'.

    FIELD-SYMBOLS: <element> TYPE element_type.

    LOOP AT elements_element_id ASSIGNING <element>.

      name = <element>-class_name.

      TRANSLATE name TO LOWER CASE.

      IF <element>-clstype EQ is_class_type.

        CONCATENATE part1 part2_classes name part3 INTO <element>-adt_link.

      ELSEIF <element>-clstype EQ interface_type.

        CONCATENATE part1 part2_interfaces name part3 INTO <element>-adt_link.

      ELSE.
        ASSERT 1 = 2.
      ENDIF.


      " Get ADT Link to components

      DATA: cifkey           TYPE seoclskey,
            cifref           TYPE REF TO if_oo_clif_incl_naming,
            clsref           TYPE REF TO if_oo_class_incl_naming,
            intref           TYPE REF TO if_oo_interface_incl_naming,
            source           TYPE seop_source_string,
            source_protected TYPE seop_source_string,
            source_private   TYPE seop_source_string,
            source_line      TYPE LINE OF seop_source_string.

      cifkey-clsname = <element>-class_name.

      CLEAR source.
      CLEAR source_protected.
      CLEAR source_private.

      CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
        EXPORTING
          cifkey = cifkey
        RECEIVING
          cifref = cifref
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        " :-(
      ELSE.
        CASE cifref->clstype.
          WHEN seoc_clstype_class.
            clsref ?= cifref.
            READ REPORT clsref->public_section
              INTO source.
            READ REPORT clsref->protected_section
              INTO source_protected.
            READ REPORT clsref->private_section
              INTO source_private.
          WHEN seoc_clstype_interface.
            intref ?= cifref.
            READ REPORT intref->public_section
              INTO source.
          WHEN OTHERS.
            " What is to be done?
        ENDCASE.
      ENDIF.

      APPEND LINES OF source_protected TO source.
      APPEND LINES OF source_private TO source.

      DATA: line_no TYPE i,
            line    TYPE string.

      line_no = 0.

*      LOOP AT source INTO source_line.
*        add 1 to line_no.
*        line = source_line.
*        condense line.
*        split line at ' ' into
*      ENDLOOP.
      DATA: tokens         TYPE STANDARD TABLE OF stokes,
            token          LIKE LINE OF tokens,
            next_component TYPE seocmptype,
            next_line      TYPE i.

      DATA statements TYPE STANDARD TABLE OF sstmnt.

      SCAN ABAP-SOURCE source TOKENS INTO tokens STATEMENTS INTO statements.

      LOOP AT tokens INTO token.
        ADD 1 TO line_no.
        IF line_no EQ next_line.

          DATA: element_comp TYPE element_comp_type.

          READ TABLE elements_comp_clsname_cmpname INTO element_comp
            WITH TABLE KEY
              clsname = <element>-class_name
              cmpname = token-str.

          IF sy-subrc <> 0.
            " What to do?
          ELSE.

            FIELD-SYMBOLS <element_comp> TYPE element_comp_type.

            READ TABLE elements_comp_element_id ASSIGNING <element_comp>
              WITH TABLE KEY element_id = element_comp-element_id.

            IF sy-subrc <> 0.
              " What to do?
            ELSE.

              DATA: row TYPE string.
              row = token-row.

              DATA: adt_link TYPE string.

              CONCATENATE <element>-adt_link '#start=' row ',1' INTO adt_link.

              CONDENSE adt_link NO-GAPS.

              IF <element_comp>-adt_link IS INITIAL. " Are there further hits?

                <element_comp>-adt_link = adt_link.

              ENDIF.

            ENDIF.

          ENDIF.

        ELSE.
          CLEAR next_line.
          CLEAR next_component.
          CASE token-str.
            WHEN 'CLASS-DATA' OR 'DATA'.
              next_component = attribute_type.
              next_line = line_no + 1.
            WHEN 'CLASS-METHODS' OR 'METHODS'.
              next_component = method_type.
              next_line = line_no + 1.
            WHEN 'CLASS-EVENTS' OR 'EVENTS'.
              next_component = event_type.
              next_line = line_no + 1.
          ENDCASE.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD comp_name.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.

    IF sy-subrc EQ 0.

      class_name = element_comp-clsname.
      cmpname = element_comp-cmpname.
      cmptype = element_comp-cmptype.
      exists = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
    ENDIF.
    instance->type = class_type.
    r_instance = instance.
  ENDMETHOD.
  METHOD is_redefinition_of_method.

    DATA: invoced  TYPE element_comp_type,
          invocing TYPE element_comp_type.

    READ TABLE elements_comp_element_id INTO invoced WITH KEY element_id = invoced_element_id1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE elements_comp_element_id INTO invocing WITH KEY element_id = invocing_element_id2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF invoced-cmpname <> invocing-cmpname.
      RETURN.
    ENDIF.

    DATA: r TYPE redefined_method_type.

    READ TABLE redefined_methods INTO r WITH TABLE KEY method = invocing-cmpname
                                                       clsname = invocing-clsname
                                                       defined_in_clsname = invoced-clsname.

    IF sy-subrc EQ 0.
      r_result = 'X'.
    ENDIF.


  ENDMETHOD.
  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    IF sy-subrc EQ 0.

      DATA: last_id        TYPE i,
            class_id       TYPE i,
            file_anchor_id TYPE i.

      IF element-clstype EQ is_class_type.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
        IF element_manager->use_somix EQ 'X'.
          DATA: unique_name TYPE string.
          unique_name = |sap.{ element-class_name }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_class
                                                          grouping            = element-class_name
                                                          technical_type      = cl_extract3=>modifier_abapglobalclass
                                                          link_to_editor      = element-adt_link
                                                IMPORTING id                  = class_id
                                                CHANGING  unique_name         = unique_name ).

        ELSE. " SOMIX

          element_manager->famix_class->add( EXPORTING name_group = ng_abap_class
                                                       name       = element-class_name
                                                       modifiers  = cl_extract3=>modifier_abapglobalclass
                                             IMPORTING id         = last_id ).

          IF element-adt_link IS NOT INITIAL.

            element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                               file_name  = element-adt_link
                                                     IMPORTING id         = file_anchor_id ).

            IF file_anchor_id IS NOT INITIAL.
              element_manager->famix_class->set_source_anchor_by_id(
                EXPORTING
                  element_id         = last_id
                  source_anchor_id   = file_anchor_id
              ).

            ENDIF.

          ENDIF.

        ENDIF. " SOMIX

      ELSEIF element-clstype EQ interface_type.
        IF element_manager->use_somix EQ 'X'.

          unique_name = |sap.{ element-class_name }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_class
                                                          grouping            = element-class_name
                                                          technical_type      = cl_extract3=>modifier_abapglobalinterface
                                                          link_to_editor      = element-adt_link
                                                IMPORTING id                  = class_id
                                                CHANGING  unique_name         = unique_name ).

        ELSE. " SOMIX
          " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
          " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
          element_manager->famix_class->add( EXPORTING name_group = ng_abap_class
                                                       name       = element-class_name
                                                       modifiers  = cl_extract3=>modifier_abapglobalinterface
                                             IMPORTING id         = last_id ).
          " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
          element_manager->famix_class->is_interface( element_id = last_id ).

          IF element-adt_link IS NOT INITIAL.

            element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                               file_name  = element-adt_link
                                                     IMPORTING id         = file_anchor_id ).

            IF file_anchor_id IS NOT INITIAL.
              element_manager->famix_class->set_source_anchor_by_id(
                EXPORTING
                  element_id         = last_id
                  source_anchor_id   = file_anchor_id
              ).

            ENDIF.

          ENDIF.

        ENDIF. " SOMIX

      ELSE.
        ASSERT 1 = 2.
      ENDIF.

      DATA association TYPE cl_extr3_element_manager=>association_type.
      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = cl_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO cl_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        IF element_manager->use_somix EQ 'X'.
          DATA: package_id TYPE i.
          DATA: devclass TYPE devclass.
          devclass = package->devclass( i_element_id = association-element_id2 ).
          unique_name = |sap.{ devclass }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                          grouping               = devclass
                                                          technical_type         = cl_extract3=>techtype_abappackage
                                                          link_to_editor         = ''
                                                IMPORTING id                     = package_id
                                                CHANGING  unique_name            = unique_name ).

          element_manager->somix_parentchild->add(  EXPORTING parent_id  = package_id
                                                              child_id   = class_id
                                                              is_main    = ''  ).

        ELSE. " SOMIX

          element_manager->famix_class->set_parent_package( element_id     = last_id
                                                            parent_package = package->devclass( i_element_id = association-element_id2 )
                                                            parent_package_name_group = ng_abap_package ).

        ENDIF. " SOMIX

      ENDLOOP.

    ELSE.

      DATA element_comp TYPE element_comp_type.

      READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.
      ASSERT sy-subrc EQ 0.
      CASE element_comp-cmptype.
        WHEN attribute_type.


*    DATA last_id TYPE i.!

          IF element_manager->use_somix EQ 'X'.

            unique_name = |sap.{ element_comp-clsname }.{ element_comp-cmpname }|.
            element_manager->somix_data->add( EXPORTING grouping_name_group = ng_abap_class
                                                        grouping    = element_comp-clsname
                                                        data_name_group = ng_abap_attribute ##TODO " Improve coding generally. Without grouping name the data name is not uniquw
                                                        data = element_comp-cmpname
                                                        technical_type = cl_extract3=>techtype_abapclassattribute
                                                        link_to_editor  = element-adt_link
                                              IMPORTING id = last_id
                                              CHANGING  unique_name         = unique_name ).

            unique_name = |sap.{ element_comp-clsname }|.
            element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_class
                                                            grouping               = element_comp-clsname
                                                            technical_type         = '' " Leave unchanged
                                                            link_to_editor         = ''
                                                  IMPORTING id                     = class_id
                                                  CHANGING  unique_name            = unique_name ).
            element_manager->somix_parentchild->add( EXPORTING parent_id = class_id
                                                               child_id  = last_id
                                                               is_main   = 'X' ).

          ELSE. " SOMIX

            element_manager->famix_attribute->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).

            element_manager->famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                          parent_element = 'FAMIX.Class'
                                                          parent_name_group = ng_abap_class
                                                          parent_name    = element_comp-clsname ).

            IF element_comp-adt_link IS NOT INITIAL.

              element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                                 file_name  = element_comp-adt_link
                                                         IMPORTING id         = file_anchor_id ).

              IF file_anchor_id IS NOT INITIAL.
                element_manager->famix_attribute->set_source_anchor_by_id(
                  EXPORTING
                    element_id         = last_id
                    source_anchor_id   = file_anchor_id
                ).

              ENDIF.

            ENDIF.

            element_manager->famix_attribute->store_id( EXPORTING name_group = ng_abap_class
                                                                  class      = element_comp-clsname
                                                                  attribute  = element_comp-cmpname ).

          ENDIF. " SOMIX

*            sap_attribute->add( EXPORTING class     = class-clsname
*                                          attribute = component-cmpname ).
        WHEN method_type OR event_type.
          " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
          " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method

          IF element_manager->use_somix EQ 'X'.

            unique_name = |sap.{ element_comp-clsname }.{ element_comp-cmpname }|.
            element_manager->somix_code->add( EXPORTING grouping_name_group = ng_abap_class
                                                        grouping            = element_comp-clsname
                                                        code_name_group     = cl_extr3=>ng_abap_method
                                                        code                = element_comp-cmpname
                                                        technical_type      = cl_extract3=>techtype_abapmethod
                                                        link_to_editor      = element-adt_link
                                              IMPORTING id                  = last_id
                                              CHANGING  unique_name         = unique_name ).

            unique_name = |sap.{ element_comp-clsname }|.
            element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_class
                                                            grouping               = element_comp-clsname
                                                            technical_type         = '' " Leave unchanged
                                                            link_to_editor         = '' " Leave unchanged
                                                  IMPORTING id                     = class_id
                                                  CHANGING  unique_name            = unique_name ).
            element_manager->somix_parentchild->add( EXPORTING parent_id = class_id
                                                               child_id  = last_id
                                                               is_main   = 'X' ).

          ELSE. " SOMIX

            element_manager->famix_method->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).

            " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
            " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
            element_manager->famix_method->set_signature( element_id = last_id
                                           signature = element_comp-cmpname ).

            element_manager->famix_method->set_parent_type( EXPORTING element_id = last_id
                                                       parent_element = 'FAMIX.Class'
                                                       parent_name_group = ng_abap_class
                                                       parent_name    = element_comp-clsname ).

            IF element_comp-adt_link IS NOT INITIAL.

              element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                                 file_name  = element_comp-adt_link
                                                         IMPORTING id         = file_anchor_id ).

              IF file_anchor_id IS NOT INITIAL.
                element_manager->famix_method->set_source_anchor_by_id(
                  EXPORTING
                    element_id         = last_id
                    source_anchor_id   = file_anchor_id
                ).

              ENDIF.

            ENDIF.

            element_manager->famix_method->store_id( EXPORTING class_name_group = ng_abap_class
                                                               class  = element_comp-clsname
                                                               method_name_group = ng_abap_method
                                                               method = element_comp-cmpname ).

          ENDIF. " SOMIX


*            sap_method->add( EXPORTING class  = class-clsname
*                                       method = component-cmpname ).
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

    ENDIF.

  ENDMETHOD.
  METHOD name.
    DATA: class_name TYPE string,
          clstype    TYPE seoclstype,
          exists     TYPE abap_bool.

    class_name( EXPORTING element_id = element_id
                IMPORTING class_name =  class_name
                          clstype    = clstype
                          exists     = exists ).

    IF exists EQ abap_true.

      CASE clstype.
        WHEN is_class_type.
          element_type = |ABAPClass|.
        WHEN interface_type.
          element_type = |ABAPInterface|.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      parent_name = ||.
      name = class_name.

    ELSE.

      DATA: cmpname TYPE string,
            cmptype TYPE seocmptype.

      comp_name( EXPORTING element_id = element_id
                   IMPORTING class_name = class_name
                             cmpname    = cmpname
                             cmptype    = cmptype
                             exists     = exists ).

      ASSERT exists EQ abap_true.

      DATA element TYPE element_type.

      READ TABLE elements_class_name INTO element WITH KEY class_name = class_name.
      ASSERT sy-subrc EQ 0.
      clstype = element-clstype.

      CASE clstype.
        WHEN is_class_type.
          CASE cmptype.
            WHEN attribute_type.
              element_type = |ABAPClassAttribute|.
            WHEN method_type.
              element_type = |ABAPClassMethod|.
            WHEN event_type.
              element_type = |ABAPClassEvent|.
            WHEN OTHERS.
              ASSERT 1 = 2.
          ENDCASE.
        WHEN interface_type.
          CASE cmptype.
            WHEN attribute_type.
              element_type = |ABAPInterfaceAttribute|.
            WHEN method_type.
              element_type = |ABAPInterfaceMethod|.
            WHEN event_type.
              element_type = |ABAPInterfaceEvent|.
            WHEN OTHERS.
              ASSERT 1 = 2.
          ENDCASE.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      parent_name = class_name.
      name = cmpname.

    ENDIF.

  ENDMETHOD.
  METHOD _add_component.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_clsname_cmpname INTO element_comp WITH KEY clsname  = clsname cmpname = cmpname.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element_comp-element_id.
    ELSE.

      " Does component exists?
      DATA: found_class_name TYPE string,
            found_cmpname    TYPE string,
            found_cmptype    TYPE seocmptype,
            found_mtdtype    TYPE seomtdtype.

        SELECT SINGLE clsname cmpname cmptype mtdtype FROM seocompo
          INTO (found_class_name, found_cmpname, found_cmptype, found_mtdtype ) WHERE clsname = clsname
                                                                                   AND cmpname = cmpname.

      IF found_class_name IS INITIAL.
        " Is it a redefined method?

        DATA: redefined_methods TYPE STANDARD TABLE OF seoredef WITH DEFAULT KEY,
              rm                TYPE seoredef.

        SELECT * FROM seoredef INTO TABLE redefined_methods WHERE clsname = clsname
                                                              AND mtdname = cmpname.

        LOOP AT redefined_methods INTO rm.
          found_class_name = clsname.
          found_cmpname = rm-mtdname.
          found_cmptype = 1. " Method
          found_mtdtype = 0. " Method
          " A method name has to be unique, so assume only a single entry here
          EXIT.
        ENDLOOP.

      ENDIF.

      IF found_class_name IS NOT INITIAL.

        is_added = abap_true.

        IF found_cmptype EQ 3. " Is type
          RAISE EXCEPTION TYPE zcx_2mse_extr3_classes_wr_type.
        ENDIF.

      ELSE.

        DATA: redefined_class_components TYPE ty_class_components,
              redefined_class_component  TYPE ty_class_component.

        redefined_class_components = _get_redefined( clsname ).

        READ TABLE redefined_class_components INTO redefined_class_component WITH KEY clsname = clsname cmpname = cmpname.

        IF sy-subrc EQ 0.

          found_class_name = redefined_class_component-clsname.
          found_cmpname = redefined_class_component-cmpname.
          found_cmptype = redefined_class_component-cmptype.
          found_mtdtype = redefined_class_component-mtdtype.

          is_added = abap_true.

        ENDIF.

      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = _add_single_component_to_class( i_found_class_name = found_class_name
                                                         i_found_cmpname    = found_cmpname
                                                         i_found_cmptype    = found_cmptype
                                                         i_found_mtdtype    = found_mtdtype
                                                         is_specific        = is_specific ).
        is_added_now = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD _add_metarel.

    DATA: relation        TYPE seometarel,
          relations       TYPE STANDARD TABLE OF seometarel,
          element_metarel TYPE element_metarel_type,
          is_added        TYPE abap_bool,
          new_element_id  TYPE i.

    DATA access TYPE REF TO cl_extr3_access.
    access = cl_extr3_access=>get_instance( i_element_manager = element_manager ).

    DATA invocation TYPE REF TO cl_extr3_invocation.
    invocation = cl_extr3_invocation=>get_instance( i_element_manager = element_manager ).


      SELECT * FROM seometarel INTO TABLE relations WHERE clsname = clsname
                                                      AND version = 1
                                                      AND state   = 1.


    LOOP AT relations INTO relation WHERE reltype = 1 OR reltype = 2.

      element_metarel-element_id = new_element_id.
      element_metarel-refclsname = relation-refclsname.
      element_metarel-reltype = relation-reltype.

      INSERT element_metarel INTO TABLE elements_metarel_element_id.
      INSERT element_metarel INTO TABLE elements_metarel_refclsname.

      IF relation-reltype EQ 1. " Interface

        DATA: interface_class_components TYPE ty_class_components,
              interface_class_component  TYPE ty_class_component,
              reclsname_string           TYPE string.

        reclsname_string = relation-refclsname.

        me->add( EXPORTING class            = reclsname_string
                           is_specific      = is_specific
                 IMPORTING is_added         = is_added
                           new_element_id   = new_element_id
                           class_components = interface_class_components ).

        LOOP AT interface_class_components INTO interface_class_component.

          new_element_id = _add_single_component_to_class( i_found_class_name = clsname
                                                           i_found_cmpname    = |{ interface_class_component-clsname }~{ interface_class_component-cmpname }|
                                                           i_found_cmptype    = interface_class_component-cmptype
                                                           i_found_mtdtype    = interface_class_component-mtdtype
                                                           is_specific        = is_specific  ).

          DATA interface_element_id TYPE cl_extr3_element_manager=>element_id_type .

          me->add_component( EXPORTING clsname        = interface_class_component-clsname
                                       cmpname        = interface_class_component-cmpname
                                       is_specific    = is_specific
                              IMPORTING "*              is_added       =
                                        new_element_id = interface_element_id ).

          IF interface_class_component-cmptype EQ attribute_type.

            " Connections between attributes are not expressible in FAMIX, or?

*            access->add( EXPORTING accessed_element_id1  = new_element_id
*                                   accessing_element_id2 = interface_element_id ).

          ELSE.

            IF element_manager->interface_use_structure EQ abap_true.

              invocation->add( EXPORTING invoced_element_id1  = interface_element_id
                                         invocing_element_id2 = new_element_id ).

            ELSE.

              invocation->add( EXPORTING invoced_element_id1  = new_element_id
                                         invocing_element_id2 = interface_element_id ).

            ENDIF.

          ENDIF.


        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD _add_single_component_to_class.

    " Add single component to class

    DATA element_comp2 TYPE element_comp_type.

    ASSERT i_found_cmpname IS NOT INITIAL.

    r_new_element_id = element_manager->add_element( element = me
                                                     is_specific = is_specific ).
    element_comp2-element_id = r_new_element_id.
    element_comp2-clsname = i_found_class_name.
    element_comp2-cmpname = i_found_cmpname.
    element_comp2-cmptype = i_found_cmptype.
    element_comp2-mtdtype = i_found_mtdtype.

    INSERT element_comp2 INTO TABLE elements_comp_element_id .
    INSERT element_comp2 INTO TABLE elements_comp_clsname_cmpname .

  ENDMETHOD.
  METHOD _get_redefined.

    DATA: redefined_components TYPE STANDARD TABLE OF redefined_type WITH DEFAULT KEY,
          redefined_component  TYPE redefined_type,
          found                TYPE abap_bool,
          superclass           TYPE seoclsname,
          component            TYPE cl_extr3_classes=>ty_class_component.


      SELECT * FROM  seoredef INTO CORRESPONDING FIELDS OF TABLE redefined_components
        WHERE clsname = class
          AND version = 1.


    LOOP AT redefined_components INTO redefined_component.
      superclass = redefined_component-refclsname.
      found = ''.
      WHILE found EQ ''.

        CLEAR component.


          SELECT SINGLE clsname cmpname cmptype mtdtype
            FROM seocompo
            INTO component
            WHERE cmptype <> 3 " A type
              AND clsname = superclass
              AND cmpname = redefined_component-mtdname.


        IF sy-subrc EQ 0.
          found = 'X'.


          DATA redefined_method TYPE redefined_method_type.
          redefined_method-method = component-cmpname.
          redefined_method-clsname = class.
          redefined_method-defined_in_clsname = component-clsname.

          INSERT redefined_method INTO TABLE redefined_methods. "Allow duplicate inserting here

          component-clsname = class .
          INSERT component INTO TABLE r_result.
        ELSE.

          " Find next superclass


            SELECT SINGLE refclsname FROM seometarel INTO superclass
              WHERE clsname = superclass
                AND version = 1.


          IF sy-subrc <> 0.
            found = 'X'.
            " Nothing found
          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_ELEMENTS IMPLEMENTATION.
  METHOD COLLECT_INFOS.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD make_model.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
  METHOD NAME.
    " Redefine me
    ASSERT 1 = 2.
  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_PACKAGES IMPLEMENTATION.
  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_devclass INTO element WITH KEY devclass = package.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      DATA exists TYPE abap_bool.

      exists = _does_package_exists( package ).
      IF exists EQ abap_true.
        is_added = abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_false ).

        element-element_id = new_element_id.
        element-devclass = package.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_devclass.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD collect_infos.
  ENDMETHOD.
  METHOD devclass.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-devclass.

  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = package_type.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    IF element_manager->use_somix EQ 'X'.

      DATA: unique_name TYPE string.
      unique_name = |sap.{ element-devclass }|.
      element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_package
                                                      grouping            = element-devclass
                                                      technical_type      = cl_extract3=>techtype_abappackage
                                                      link_to_editor      = ''
                                            CHANGING  unique_name         = unique_name ).

    ELSE. " SOMIX

      element_manager->famix_package->add( name       = element-devclass
                                           name_group = ng_abap_package ).

    ENDIF. " SOMIX

  ENDMETHOD.
  METHOD name.

    DATA devclass TYPE devclass.
    devclass = devclass( i_element_id = element_id ).

    element_type = |ABAPPackage|.
    parent_name = ||.
    name = devclass.


  ENDMETHOD.
  METHOD _does_package_exists.

    " SAP_2_FAMIX_66

    " Local packages start with a $ and have no entry in table TADIR. So report them always as existing

    IF i_package+0(1) EQ '$'.
      exists = abap_true.
      RETURN.
    ENDIF.

    " Does package exists?
    DATA found_obj_name TYPE sobj_name.
      SELECT SINGLE obj_name FROM tadir INTO found_obj_name WHERE pgmid = 'R3TR'
                                                              AND object = 'DEVC'
                                                              AND obj_name = i_package.

    IF sy-subrc EQ 0.
      exists = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_PROGRAMS IMPLEMENTATION.
  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_program INTO element WITH KEY program = program.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does the program exist?
      DATA found_program TYPE progname.
      DATA found_subc    TYPE subc.
        " No blank between ( and found... to be 7.02 compatible
        SELECT SINGLE name subc FROM progdir INTO (found_program, found_subc ) WHERE name = program.
      IF found_program IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_true ).
        element-element_id = new_element_id.
        element-program = found_program.
        _convert_program_2_ext_name( EXPORTING i_element_program = found_program
                                     IMPORTING program_type = element-program_type
                                               program_attribute_1 = element-program_attribute_1
                                               program_attribute_2 = element-program_attribute_2
                                               r_result = element-external_program_name ).
        element-subc = found_subc.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD add_function.

    DATA: program_found TYPE progname,
          tf            TYPE tfdir.
    " TBD find a better solution for this
    SELECT SINGLE * FROM tfdir INTO tf WHERE funcname = function .
    IF tf IS NOT INITIAL.
      "TBD handle error
    ENDIF.
    program_found = tf-pname.
    SHIFT program_found LEFT BY 3 PLACES.
    program_found = program_found && |U| && tf-include.

    add( EXPORTING program        = program_found
         IMPORTING is_added       = is_added
                   new_element_id = new_element_id ).

  ENDMETHOD.
  METHOD add_function_group.

    DATA pname TYPE pname.

    TYPES: BEGIN OF ty_function_group,
             include TYPE includenr,
           END OF ty_function_group.

    DATA: fg  TYPE ty_function_group,
          fgs TYPE STANDARD TABLE OF ty_function_group WITH DEFAULT KEY.

    pname = |SAPL| && fgr.

    SELECT include FROM tfdir INTO TABLE fgs WHERE pname = pname.

    LOOP AT fgs INTO fg.

      DATA progname TYPE progname.

      progname = |L| && fgr && |U| && fg-include.

      DATA is_found TYPE abap_bool.

      DATA: fg_new_element_id TYPE cl_extr3_element_manager=>element_id_type.

      add( EXPORTING program        = progname
           IMPORTING is_added       = is_found
                     new_element_id = fg_new_element_id ).

    ENDLOOP.

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD collect_infos.

    FIELD-SYMBOLS: <p> TYPE element_type,
                   <e> TYPE element_type.

    LOOP AT elements_program ASSIGNING <p>.

      IF <p>-program_type EQ type_program.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/programs/programs/' <p>-program_attribute_1 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_function.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.
        TRANSLATE <p>-program_attribute_2 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/functions/groups/' <p>-program_attribute_1 '/fmodules/' <p>-program_attribute_2 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_function_include.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.
        TRANSLATE <p>-program_attribute_2 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/functions/groups/' <p>-program_attribute_1 '/includes/' <p>-program_attribute_2 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_bw_transformation.

        CONCATENATE 'bwmt://' sysid '/sap/bw/modeling/trfn/' <p>-program_attribute_1 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-adt_or_bwmt_link IS NOT INITIAL.

        READ TABLE elements_element_id ASSIGNING <e> WITH TABLE KEY element_id = <p>-element_id.
        ASSERT sy-subrc EQ 0.

        <e>-adt_or_bwmt_link = <p>-adt_or_bwmt_link.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = program_type.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA: last_id              TYPE i,
          file_anchor_id       TYPE i,
          name_group           TYPE string,
          modifier             TYPE string,
          techtype             TYPE string,
          name_of_mapped_class TYPE string,
          is_main              TYPE abap_bool.
*      famix_package->add( name = table-devclass ).

    IF element-program_type EQ type_program.
      name_group = 'ABAP_PROGRAM'.
      modifier = cl_extract3=>modifier_program.
      techtype = cl_extract3=>modifier_program.
      name_of_mapped_class = element-external_program_name.
    ELSEIF element-program_type EQ type_bw_transformation.
      name_group = 'BW_TRANSFORMATION'.
      modifier = cl_extract3=>modifier_bw_transformation.
      techtype = cl_extract3=>modifier_bw_transformation.
      name_of_mapped_class = element-external_program_name.
    ELSEIF element-program_type EQ type_function OR element-program_type = type_function_include.
      name_group = 'ABAP_FUNCTIONGROUP'.
      modifier = cl_extract3=>modifier_function_group.
      techtype = cl_extract3=>techtype_abap_function.
*      name_of_mapped_class = element-external_program_name.
      name_of_mapped_class = _get_names_for_function_groups( element ).
      is_main = 'X'. "Display function group always for an ABAP function

*      " Get parent package for function group
*      DATA devclass TYPE tadir-devclass.
*      SELECT SINGLE devclass FROM tadir INTO devclass WHERE pgmid = 'R3TR' AND object = 'FUGR' AND obj_name = element-program_attribute_1.

    ELSE.
      name_group = 'UNKNOWN'.
      modifier = cl_extract3=>modifier_unknown.
      techtype = cl_extract3=>modifier_unknown.
      name_of_mapped_class = element-external_program_name.
    ENDIF.

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'

    IF element_manager->use_somix EQ 'X'.

      DATA: unique_name TYPE string.
      unique_name = |sap.{ name_of_mapped_class }|.
      element_manager->somix_grouping->add( EXPORTING grouping_name_group  = name_group
                                                      grouping             = name_of_mapped_class
                                                      technical_type       = modifier
                                                      link_to_editor       = element-adt_or_bwmt_link
                                            IMPORTING id                   = last_id
                                            CHANGING  unique_name          = unique_name ).

      DATA association TYPE cl_extr3_element_manager=>association_type.

      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = cl_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO cl_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        DATA: package_id TYPE i.

        DATA: devclass TYPE devclass.
        devclass = package->devclass( i_element_id = association-element_id2 ).

        unique_name = |sap.{ devclass }|.

        element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                        grouping               = devclass
                                                        technical_type         = cl_extract3=>techtype_abappackage
                                                        link_to_editor         = ''
                                              IMPORTING id                     = package_id
                                                CHANGING  unique_name         = unique_name ).

        element_manager->somix_parentchild->add( EXPORTING parent_id = package_id
                                                           child_id  = last_id
                                                           is_main   = '' ).

      ENDLOOP.

    ELSE. " SOMIX

      element_manager->famix_class->add( EXPORTING name_group             = name_group
                                                   name                   = name_of_mapped_class
                                                   modifiers              = modifier
                                         IMPORTING id         = last_id ).

      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = cl_extr3_association=>parent_package_ass.

        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id
                                                                    parent_package     = package->devclass( i_element_id = association-element_id2 )
                                                                    parent_package_name_group = ng_abap_package ).

      ENDLOOP.

    ENDIF. " SOMIX

    DATA dummy_method_id TYPE i.

    IF element_manager->use_somix EQ 'X'.

      unique_name = |sap.{ name_of_mapped_class }|.

      element_manager->somix_code->add( EXPORTING grouping_name_group = 'ABAP_PROGRAM' ##TODO " Analyze how to improve "name_group
                                                  grouping            = name_of_mapped_class
                                                  code_name_group     = cl_extr3=>ng_abap_program
                                                  code                = element-external_program_name
                                                  technical_type      = techtype
                                                  link_to_editor      = element-adt_or_bwmt_link
                                        IMPORTING id                  = dummy_method_id
                                        CHANGING  unique_name         = unique_name ).

      element_manager->somix_parentchild->add( EXPORTING parent_id = last_id
                                                         child_id  = dummy_method_id
                                                         is_main   = is_main ).

    ELSE. " SOMIX

      element_manager->famix_method->add( EXPORTING name = element-external_program_name
                                          IMPORTING id   = dummy_method_id ).

      element_manager->famix_method->set_signature( element_id = dummy_method_id
                                                     signature = element-external_program_name ).

      element_manager->famix_method->set_parent_type( EXPORTING element_id        = dummy_method_id
                                                                parent_element    = 'FAMIX.Class'
                                                                parent_name_group = name_group
                                                                parent_name       = name_of_mapped_class ).

      element_manager->famix_method->store_id( EXPORTING class_name_group = ng_abap_program
                                                         class  = name_of_mapped_class
                                                         method_name_group = ng_abap_program
                                                         method = element-external_program_name ).

      IF element-adt_or_bwmt_link IS NOT INITIAL.

        element_manager->famix_file_anchor->add( EXPORTING element_id = dummy_method_id " Required for Moose 6.1
                                                           file_name  = element-adt_or_bwmt_link
                                                   IMPORTING id         = file_anchor_id ).

        IF file_anchor_id IS NOT INITIAL.
          element_manager->famix_method->set_source_anchor_by_id(
            EXPORTING
              element_id         = dummy_method_id
              source_anchor_id   = file_anchor_id
          ).

        ENDIF.

      ENDIF.

    ENDIF. " SOMIX

  ENDMETHOD.
  METHOD name.

    element_type = |ABAPProgramOrFunctionOrSAPBW|.
    program_name( EXPORTING i_element_id          = element_id
                  IMPORTING external_program_name_method = name ).
    parent_name = ||.

  ENDMETHOD.
  METHOD program_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.
    program_type = element-program_type.
    program_attribute_1 = element-program_attribute_1.
    program_attribute_2 = element-program_attribute_2.
    program = element-program.
    IF element-program_type EQ type_function OR element-program_type = type_function_include.
      external_program_name_class = _get_names_for_function_groups( i_element = element ).
    ELSE.
      external_program_name_class = element-external_program_name.
    ENDIF.
    external_program_name_method = element-external_program_name.
    subc = element-subc.

  ENDMETHOD.
  METHOD _convert_program_2_ext_name.

    CLEAR program_type.
    CLEAR program_attribute_1.

    DATA: tranid           TYPE rstranid,
          function_group   TYPE rs38l_area,
          function         TYPE rs38l_fnam,
          function_include TYPE string,
          is_tested        TYPE abap_bool.

    CLEAR program_type.
    CLEAR program_attribute_1.
    CLEAR program_attribute_2.

    IF i_element_program+0(3) EQ |ZGP|.
      " Do check only when potentially needed to improve performance
      is_tested = cl_extract3=>check_if_tested( ).
    ENDIF.

    IF i_element_program+0(1) EQ |L|.

      _extract_function_name( EXPORTING i_element_program = i_element_program
                              IMPORTING function_group = function_group
                                        function = function
                                        function_include = function_include
                                        r_result = r_result ).

      IF function IS NOT INITIAL.

        program_type = type_function.
        program_attribute_1 = function_group.
        program_attribute_2 = function.

      ELSEIF function_include IS NOT INITIAL.

        program_type = type_function_include.
        program_attribute_1 = function_group.
        program_attribute_2 = function_include.

      ENDIF.

    ELSEIF i_element_program+0(3) EQ |ZGP|. "Only on test system, currently no SAP BW working there
      ##TODO " Find better way to determine Unit Test
      _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                             IMPORTING tranid = tranid
                                       r_result = r_result ).

      program_type = type_bw_transformation.
      program_attribute_1 = tranid.

    ELSEIF i_element_program+0(2) EQ |GP|.

      _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                             IMPORTING tranid = tranid
                                       r_result = r_result ).

      program_type = type_bw_transformation.
      program_attribute_1 = tranid.

    ELSE.

      r_result = i_element_program.

      program_type = type_program.
      program_attribute_1 = i_element_program.

    ENDIF.

  ENDMETHOD.
  METHOD _extract_function_name.

    " Extract function name

    DATA: length                TYPE i,
          postfix_position      TYPE i,
          include_type_position TYPE i,
          function_group_length TYPE i,
          include_type          TYPE string,
          include               TYPE includenr,
          temp                  TYPE string,
          pname                 TYPE pname,
          funcname              TYPE rs38l_fnam.

    CLEAR function.
    CLEAR function_group.
    CLEAR function_include.

    length = strlen( i_element_program ).

    IF length < 5.

      r_result = i_element_program.

    ELSE.

      postfix_position = length - 2.

      include_type_position = length - 3.

      function_group_length = length - 4.

      include = i_element_program+postfix_position(2).

      include_type = i_element_program+include_type_position(1).

      temp = i_element_program+0(include_type_position).

      IF include_type EQ |F| OR include_type EQ |U|.

        function_group = temp+1(function_group_length).

      ENDIF.

      IF include_type EQ |F|.

        r_result = i_element_program.

        function_include = i_element_program.

      ELSEIF include_type EQ |U|.

        postfix_position = length - 3.

        CONCATENATE 'SAP' temp INTO pname.


          SELECT SINGLE funcname FROM tfdir INTO funcname WHERE pname = pname
                                                            AND include = include.


        IF sy-subrc <> 0.
          r_result = i_element_program.
        ELSE.
          r_result = |F-| && funcname.

          function = funcname.

        ENDIF.
      ELSE.

        r_result = i_element_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD _extract_sap_bw_logic.

    " Extract SAP BW logic

    DATA: element_program         TYPE progname,
          transformation_progr_id TYPE rstran_progid,
          length                  TYPE i,
          id_length               TYPE i,
          transformation          TYPE rstran,
          is_tested               TYPE abap_bool.

    CLEAR tranid.

    element_program = i_element_program.

    IF element_program+0(3) = 'ZGP'.
      IF element_program = 'ZGP003N8S45LS1FG375G2BN69Q4G'.
        is_tested = cl_extract3=>check_if_tested( ).
        IF is_tested = 'X'.
          SHIFT element_program LEFT BY 1 PLACES.
        ENDIF.
      ENDIF.
    ENDIF.

    length = strlen( i_element_program ).
    id_length = length - 2.

    transformation_progr_id = i_element_program+2(id_length).

    IF element_program = 'GP003N8S45LS1FG375G2BN69Q4G' AND is_tested = 'X'.
      CLEAR transformation.
      transformation-tranid = |123|.
      transformation-sourcetype = |ODSO|.
      transformation-sourcename = |Z2MSET001|.
      transformation-targettype = |CUBE|.
      transformation-targetname = |Z2MSET002|.
    ELSE.

      SELECT SINGLE * FROM rstran INTO transformation WHERE objvers = 'A'
                                                        AND tranprog = transformation_progr_id.

    ENDIF.

    IF sy-subrc <> 0.
      r_result = i_element_program.
    ELSE.
      r_result = |BW-| && transformation-sourcetype && |-|
                       && transformation-sourcename && |-|
                       && transformation-targettype && |-|
                       && transformation-targetname.
      " In case of InfoSources there are multiple blanks in the field SOURCENAME
      " Remove all but a single blank
      CONDENSE r_result.

      tranid = transformation-tranid.

    ENDIF.

  ENDMETHOD.
  METHOD _get_names_for_function_groups.

    CONCATENATE 'FGR-' i_element-program_attribute_1 INTO name_of_mapped_class.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_TABLES IMPLEMENTATION.
  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_tabname INTO element WITH KEY tabname = table.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does table exists?
      DATA found_tabname TYPE tabname.
      DATA found_tabclass TYPE tabclass.
        " No blank between ( and found... to be 7.02 compatible
        SELECT tabname tabclass FROM dd02l INTO (found_tabname, found_tabclass ) WHERE tabname = table.

        ENDSELECT.
      IF     found_tabname IS NOT INITIAL
         AND found_tabclass <> 'INTTAB'  " Not structures
         AND found_tabclass <> 'APPEND'. " Not append structures
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_true ).
        element-element_id = new_element_id.
        element-tabname = table.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_tabname.

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD collect_infos.
  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = table_type.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA last_id TYPE i.
*      famix_package->add( name = table-devclass ).

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'

    DATA database_table_id TYPE i.

    IF element_manager->use_somix EQ 'X'.
      DATA: unique_name TYPE string.

      unique_name = |sap.{ database_schema }.{ element-tabname }|.

      element_manager->somix_data->add( EXPORTING grouping_name_group = ng_database_schema
                                                  grouping            = database_schema
                                                  data_name_group     = ng_sap_table
                                                  data                = element-tabname
                                                  technical_type      = cl_extract3=>modifier_dbtable
                                                  link_to_editor      = ''
                                        IMPORTING id                  = database_table_id
                                        CHANGING  unique_name         = unique_name ).

      DATA association TYPE cl_extr3_element_manager=>association_type.
      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = cl_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO cl_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        DATA: package_id TYPE i.
        DATA: devclass TYPE devclass.
        devclass = package->devclass( i_element_id = association-element_id2 ).

        unique_name = |sap.{ devclass }|.

        element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                        grouping               = devclass
                                                        technical_type         = cl_extract3=>techtype_abappackage
                                                        link_to_editor         = ''
                                              IMPORTING id                     = package_id
                                              CHANGING  unique_name         = unique_name ).

        element_manager->somix_parentchild->add( EXPORTING parent_id = package_id
                                                           child_id  = database_table_id
                                                           is_main   = '' ).

      ENDLOOP.

    ELSE. " SOMIX

      element_manager->famix_class->add( EXPORTING name_group             = 'ABAP_TABLE'
                                                   name                   = element-tabname
                                                   modifiers              = cl_extract3=>modifier_dbtable
                                         IMPORTING id         = last_id ).

      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = cl_extr3_association=>parent_package_ass.

        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id ##TODO " Determine the package for a new table
                                                                    parent_package     = package->devclass( i_element_id = association-element_id2 )
                                                                    parent_package_name_group = ng_abap_package ).

      ENDLOOP.

    ENDIF. " SOMIX
    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table

    IF element_manager->use_somix EQ 'X'.

    ELSE. "SOMIX

      element_manager->famix_attribute->add( EXPORTING name                   = element-tabname
                                             IMPORTING id                     = database_table_id ).

      element_manager->famix_attribute->set_parent_type( EXPORTING element_id         = database_table_id
                                                  parent_id          = last_id ).

      element_manager->famix_attribute->store_id( EXPORTING name_group = ng_sap_table
                                                            class     = element-tabname
                                                            attribute = element-tabname ).

    ENDIF. "SOMIX

  ENDMETHOD.
  METHOD name.

    DATA: table TYPE tabname.

    table = table_name( i_element_id = element_id ).

    element_type = |ABAPDatabaseTable|.
    parent_name = ||.
    name = table.

  ENDMETHOD.
  METHOD table_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-tabname.

  ENDMETHOD.
  METHOD constructor.
    super->constructor( i_element_manager = i_element_manager ).

    " Determine database schema to group database tables accordingly.
    DATA: dbinfo TYPE dbrelinfo.
    CALL FUNCTION 'DB_DBRELINFO' IMPORTING dbinfo = dbinfo.
    database_schema = dbinfo-dbschema.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_WEB_DYNPRO_COMP IMPLEMENTATION.
  METHOD add.
    " WDY_COMPONENT
    " WDY_CONTROLLER

    DATA element TYPE element_type.

    READ TABLE elements_wdy_component_name INTO element WITH KEY  wdy_component_name  =  wdy_component_name .
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does Web Dynpro Component exists?
      DATA: found_wdy_component_name TYPE wdy_component_name.

        SELECT SINGLE component_name FROM wdy_component INTO found_wdy_component_name
          WHERE component_name = wdy_component_name
            AND version = 'A'.

      IF found_wdy_component_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_false ).
        element-element_id = new_element_id.
        element-wdy_component_name = found_wdy_component_name.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_wdy_component_name.

      ENDIF.

      TYPES: BEGIN OF ty_class_component,
               component_name  TYPE wdy_component_name,
               controller_name TYPE wdy_controller_name,
             END OF ty_class_component.
      TYPES ty_class_components TYPE STANDARD TABLE OF ty_class_component WITH KEY component_name controller_name.
      DATA: class_components TYPE ty_class_components,
            class_component  TYPE ty_class_component.


        SELECT component_name controller_name
          FROM wdy_controller
          INTO CORRESPONDING FIELDS OF TABLE class_components
          WHERE component_name = wdy_component_name
            AND version = 'A'.


      LOOP AT class_components INTO class_component.

        _add_component( EXPORTING wdy_component_name        = class_component-component_name
                                  wdy_controller_name        = class_component-controller_name ).

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD add_component.

    add( EXPORTING wdy_component_name = wdy_component_name
         IMPORTING is_added           = is_added ).

    IF is_added EQ abap_true.

      _add_component( EXPORTING wdy_component_name  = wdy_component_name
                                wdy_controller_name = wdy_controller_name
                      IMPORTING is_added            = is_added
                                new_element_id      = new_element_id ).

      element_manager->model_builder->new_element_id( EXPORTING i_element_id  = new_element_id
                                                                i_is_specific = abap_true ).

    ENDIF.

  ENDMETHOD.
  METHOD clear.
    CLEAR instance.
  ENDMETHOD.
  METHOD collect_infos.
  ENDMETHOD.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
    ENDIF.
    instance->type = web_dynpro_comps_type.
    r_instance = instance.
  ENDMETHOD.
  METHOD make_model.

    DATA: element           TYPE element_type,
          element_component TYPE element_comp_type.

    DATA component_id TYPE i.
    DATA controller_id TYPE i.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    IF sy-subrc EQ 0.

      IF element_manager->use_somix EQ 'X'.

        DATA: unique_name TYPE string.
        unique_name = |sap.{ element-wdy_component_name }|.

        element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_webdynpro
                                                        grouping            = element-wdy_component_name
                                                        technical_type      = cl_extract3=>modifier_webdynpro_component
                                                        link_to_editor      = ''
                                              IMPORTING id                  = component_id
                                              CHANGING  unique_name         = unique_name ).

        DATA association TYPE cl_extr3_element_manager=>association_type.
        LOOP AT associations INTO association WHERE element_id1 = element_id
                                                AND association->type = cl_extr3_association=>parent_package_ass.
          DATA package TYPE REF TO cl_extr3_packages.
          package ?= element_manager->get_element( i_element_id = association-element_id2 ).

          DATA: package_id TYPE i.
          DATA: devclass TYPE devclass.
          devclass = package->devclass( i_element_id = association-element_id2 ).
          unique_name = |sap.{ devclass }|.

          element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                          grouping               = devclass
                                                          technical_type         = cl_extract3=>techtype_abappackage
                                                          link_to_editor         = ''
                                                IMPORTING id                     = package_id
                                                CHANGING  unique_name         = unique_name ).

          element_manager->somix_parentchild->add( EXPORTING parent_id = package_id
                                                             child_id  = component_id
                                                             is_main   = '' ).

        ENDLOOP.


      ELSE. " SOMIX

        element_manager->famix_class->add( EXPORTING name_group = ng_abap_webdynpro
                                                     name       = element-wdy_component_name
                                                     modifiers  = cl_extract3=>modifier_webdynpro_component
                                           IMPORTING id         = component_id ).

        LOOP AT associations INTO association WHERE element_id1 = element_id
                                                AND association->type = cl_extr3_association=>parent_package_ass.
          package ?= element_manager->get_element( i_element_id = association-element_id2 ).
          element_manager->famix_class->set_parent_package( element_id     = component_id
                                                            parent_package = package->devclass( i_element_id = association-element_id2 )
                                                            parent_package_name_group = ng_abap_package ).

        ENDLOOP.

      ENDIF. " SOMIX

      LOOP AT elements_comp_comp_contr_name INTO element_component WHERE wdy_component_name = element-wdy_component_name.

        IF element_manager->use_somix EQ 'X'.
          unique_name = |sap.{ element-wdy_component_name }.{ element_component-wdy_controller_name }|.

          element_manager->somix_code->add( EXPORTING grouping_name_group = ng_abap_webdynpro
                                                      grouping            = element-wdy_component_name
                                                      code_name_group     = '' ##TODO " namegroup for WebFynpro Controller needed?
                                                      code                = element_component-wdy_controller_name
                                                      technical_type      = cl_extract3=>techtype_webdynpro_controller
                                                      link_to_editor      = ''
                                            IMPORTING id                  = controller_id
                                            CHANGING  unique_name         = unique_name ).
          element_manager->somix_parentchild->add( EXPORTING parent_id = component_id
                                                             child_id  = controller_id
                                                             is_main   = 'X' ).

        ELSE. " SOMIX

          element_manager->famix_method->add( EXPORTING name = element_component-wdy_controller_name
                                              IMPORTING id = controller_id ).

          element_manager->famix_method->set_signature( element_id = controller_id
                                         signature = element_component-wdy_controller_name ).

          element_manager->famix_method->set_parent_type(
            EXPORTING
              element_id         = controller_id
              parent_element     = 'FAMIX.Class'
              parent_id          = component_id ).

          "! TBD Really required, this appears to be not exact, no namegroup, ...
          element_manager->famix_method->store_id( EXPORTING class_name_group = ng_abap_webdynpro
                                                             class  = element-wdy_component_name
                                                             method_name_group = ng_abap_webdynpro
                                                             method = element_component-wdy_controller_name ).

        ENDIF. " SOMIX

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD name.

    DATA: wdy_component_name TYPE wdy_component_name.

    wdy_component_name( EXPORTING element_id         = element_id
                        IMPORTING wdy_component_name = wdy_component_name ).

    IF wdy_component_name IS NOT INITIAL.
      element_type = |WebDynproComponent|.
      parent_name = ||.
      name = wdy_component_name.
    ELSE.

      DATA: wdy_controller_name TYPE wdy_controller_name.

      wdy_controller_name( EXPORTING element_id         = element_id
                           IMPORTING wdy_component_name  = wdy_component_name
                                     wdy_controller_name = wdy_controller_name ).

      ASSERT wdy_controller_name IS NOT INITIAL.
      element_type = |WebDynproController|.
      parent_name = wdy_component_name.
      name = wdy_controller_name.

    ENDIF.

  ENDMETHOD.
  METHOD wdy_component_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    IF sy-subrc EQ 0.

      wdy_component_name = element-wdy_component_name.

    ENDIF.

  ENDMETHOD.
  METHOD wdy_controller_name.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.
    IF sy-subrc EQ 0.

      wdy_component_name = element_comp-wdy_component_name.
      wdy_controller_name = element_comp-wdy_controller_name.

    ENDIF.

  ENDMETHOD.
  METHOD _add_component.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_comp_contr_name INTO element_comp WITH KEY wdy_component_name  = wdy_component_name
                                                                        wdy_controller_name  = wdy_controller_name .
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element_comp-element_id.
    ELSE.

      " Does component exists?
      DATA: found_component_name  TYPE wdy_component_name,
            found_controller_name TYPE seocmpname.

        SELECT SINGLE component_name controller_name FROM wdy_controller
          " No blank between ( and found to be 7.02 compatible
          INTO (found_component_name, found_controller_name ) WHERE component_name  = wdy_component_name
                                                                 AND controller_name  = wdy_controller_name
                                                                 AND version = 'A'.

      IF found_component_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_false ).
        element_comp-element_id = new_element_id.
        element_comp-wdy_component_name = found_component_name.
        element_comp-wdy_controller_name = found_controller_name.

        INSERT element_comp INTO TABLE elements_comp_element_id .
        INSERT element_comp INTO TABLE elements_comp_comp_contr_name .

      ENDIF.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3 IMPLEMENTATION.
  METHOD clear_all.

    cl_extr3_access=>clear( ).
    cl_extr3_invocation=>clear( ).
    cl_extr3_parent_package=>clear( ).
    cl_extr3_classes=>clear( ).
    cl_extr3_packages=>clear( ).
    cl_extr3_programs=>clear( ).
    cl_extr3_tables=>clear( ).
    cl_extr3_web_dynpro_comp=>clear( ).

  ENDMETHOD.
  METHOD constructor.
    element_manager = i_element_manager.
  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_ELEMENT_MANAGER IMPLEMENTATION.
  METHOD add_association.

    DATA line TYPE association_type.
    line-element_id1 = element_1.
    line-element_id2 = element_2.
    line-ass_type    = association->type.
    line-association = association.
    INSERT line INTO TABLE associations1.
    INSERT line INTO TABLE associations2.

  ENDMETHOD.
  METHOD add_element.

    DATA element_line TYPE element_type.
    element_line-element_id = next_element_id.
    element_id = next_element_id.
    element_line-element =  element.
    INSERT element_line INTO TABLE elements.

    model_builder->new_element_id( i_element_id  = element_id
                                   i_is_specific = is_specific ).

    ADD 1 TO next_element_id.

  ENDMETHOD.
  METHOD collect_infos.

    DATA: element      TYPE element_type.

    LOOP AT elements INTO element.

      IF element-element->infos_are_collected EQ abap_false.

        element-element->collect_infos( sysid ).

        element-element->infos_are_collected = abap_true.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    model_builder = i_model_builder.

    exclude_found_sap_intf = i_exclude_found_sap_intf.

    interface_use_structure = i_interface_use_structure.

    use_somix = i_use_somix.

    next_element_id = 1.

    CREATE OBJECT model.

    IF use_somix EQ 'X'.

      CREATE OBJECT somix_extraction EXPORTING model = model.
      CREATE OBJECT somix_grouping EXPORTING model = model.
      CREATE OBJECT somix_code EXPORTING model = model.
      CREATE OBJECT somix_data EXPORTING model = model.
      CREATE OBJECT somix_call EXPORTING model = model.
      CREATE OBJECT somix_access EXPORTING model = model.
      CREATE OBJECT somix_parentchild EXPORTING model = model.

    ELSE. " SOMIX

      DATA f_custom_source_language TYPE REF TO cl_famix_custom_source_lng.
      CREATE OBJECT f_custom_source_language EXPORTING model = model.
      f_custom_source_language->add( name = 'SAP' name_group = cl_extr3=>ng_source_language ).

      CREATE OBJECT famix_package EXPORTING model = model.
      CREATE OBJECT famix_class EXPORTING model = model.
      CREATE OBJECT famix_method EXPORTING model = model.
      CREATE OBJECT famix_attribute EXPORTING model = model.
      CREATE OBJECT famix_invocation EXPORTING model = model.
      CREATE OBJECT famix_access EXPORTING model = model.
      CREATE OBJECT famix_file_anchor EXPORTING model = model.

    ENDIF. " SOMIX

  ENDMETHOD.
  METHOD get_associations.
    DATA association TYPE association_type.

    LOOP AT associations1 INTO association WHERE element_id1 = i_element_id.
      INSERT association INTO TABLE associations.
    ENDLOOP.

    LOOP AT associations2 INTO association WHERE element_id2 = i_element_id.
      INSERT association INTO TABLE associations.
    ENDLOOP.

    SORT associations.
    DELETE ADJACENT DUPLICATES FROM associations.

  ENDMETHOD.
  METHOD get_element.

    DATA element TYPE element_type.

    READ TABLE elements INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-element.

  ENDMETHOD.
  METHOD make_model.

    DATA: element      TYPE element_type,
          associations TYPE associations_type,
          association  TYPE association_type,
          step         TYPE i.

    DO 2 TIMES.

      step = sy-index.

      LOOP AT elements INTO element.

        " Add packages first to the Moose model. This simplifies building the model, as many elements have parent
        IF step EQ 1.
          IF element-element->type <> element-element->package_type.
            CONTINUE.
          ENDIF.
        ELSEIF element-element->type EQ element-element->package_type.
          CONTINUE.
        ENDIF.

        associations = get_associations( i_element_id = element-element_id ).

        element-element->make_model( element_id = element-element_id
                                     associations = associations ).

      ENDLOOP.

    ENDDO.

    DATA: access               TYPE REF TO cl_extr3_access,
          invocation           TYPE REF TO cl_extr3_invocation,
          association_instance TYPE REF TO cl_extr3_association.

    access = cl_extr3_access=>get_instance( i_element_manager = me ).
    invocation = cl_extr3_invocation=>get_instance( i_element_manager = me ).

    LOOP AT associations1 INTO association.

      CLEAR association_instance.

      IF association-association->type EQ association-association->access_ass.
        association_instance = access.
      ELSEIF association-association->type EQ association-association->invocation_ass.
        association_instance = invocation.
      ENDIF.

      IF association_instance IS BOUND.
        association_instance->make_model( association = association ).
      ENDIF.

    ENDLOOP.


    model->make_mse( IMPORTING mse_model = r_result ).

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_INITIAL_ELEMENTS IMPLEMENTATION.
  METHOD constructor.
    IF tdevc_test IS SUPPLIED.
      g_tdevc_test = tdevc_test.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD get_selected.

    r_packages = g_selected_packages.

  ENDMETHOD.
  METHOD select_packages.

    DATA: package  TYPE ty_package,
          packages TYPE ty_packages.

    DATA: package_store  TYPE ty_package_store,
          packages_store TYPE ty_packages_store.

    FIELD-SYMBOLS <packages_store> TYPE ty_package_store.

    CLEAR g_selected_packages.

    packages = _select_top_packages( top_packages ).

    LOOP AT packages INTO package.

      CLEAR package_store.
      package_store-package = package-package.
      package_store-is_to_be_returned = abap_true.
      INSERT package_store INTO TABLE packages_store.

    ENDLOOP.

    IF including_sub_packages EQ abap_true.

      DATA: packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package,
            something_to_search    TYPE abap_bool.

      something_to_search = abap_true.

      WHILE something_to_search EQ abap_true.

        CLEAR packages_to_search_sub.

        LOOP AT packages_store ASSIGNING <packages_store> WHERE subclass_searched EQ abap_false.

          <packages_store>-subclass_searched = abap_true.
          package-package = <packages_store>-package.
          INSERT package INTO TABLE packages_to_search_sub.

        ENDLOOP.
        IF packages_to_search_sub IS NOT INITIAL.

          packages = _select_sub_packages( i_packages_to_search_sub = packages_to_search_sub ).

          LOOP AT packages INTO package.
            CLEAR package_store.
            package_store-package = package-package.
            package_store-parentpackage = package-parentpackage.
            IF package-package IN sub_packages_filter.
              package_store-is_to_be_returned = abap_true.
            ENDIF.
            INSERT package_store INTO TABLE packages_store.
          ENDLOOP.

        ELSE.
          something_to_search = abap_false.
        ENDIF.
      ENDWHILE.

    ENDIF.

    LOOP AT packages_store INTO package_store WHERE is_to_be_returned = abap_true.

      package-package = package_store-package.
      package-parentpackage = package_store-parentpackage.
      INSERT package INTO TABLE g_selected_packages.

    ENDLOOP.

  ENDMETHOD.
  METHOD select_specific.

    DATA new_element_id TYPE i.

    model_builder->initial_selection_started( ).
    model_builder->usage_of_single_element( ).

    CASE i_element_type_filter.
      WHEN cl_extr3_initial_elements=>select_class_method.

        IF i_parent_name_filter IS INITIAL.

          new_element_id = _select_class( name_filter        = i_name_filter
                                          element_manager    = element_manager ).

        ELSE.

          new_element_id = _select_class_method( name_filter        = i_name_filter
                                                 parent_name_filter = i_parent_name_filter
                                                 element_manager    = element_manager ).

        ENDIF.

      WHEN cl_extr3_initial_elements=>select_table.

        new_element_id = _select_table( name_filter     = i_name_filter
                                        element_manager = element_manager ).

      WHEN cl_extr3_initial_elements=>select_program.

        new_element_id = _select_program( element_manager = element_manager
                                          name_filter     = i_name_filter ).

      WHEN cl_extr3_initial_elements=>select_function.

        new_element_id = _select_function( element_manager = element_manager
                                           name_filter     = i_name_filter ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    model_builder->new_element_id( EXPORTING i_element_id  = new_element_id
                                             i_is_specific = abap_true ).

  ENDMETHOD.
  METHOD _select_class_method.

    " Select class method

    DATA classes TYPE REF TO cl_extr3_classes.
    classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
    classes->add_component( EXPORTING clsname        = parent_name_filter
                                      cmpname        = name_filter
                                      is_specific    = abap_false
                            IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.
  METHOD _select_function.

    " Select function

    DATA programs2 TYPE REF TO cl_extr3_programs.
    programs2 = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).
    programs2->add_function( EXPORTING function       = name_filter
                             IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.
  METHOD _select_program.

    " Select program

    DATA programname TYPE program.
    programname = name_filter.

    DATA programs TYPE REF TO cl_extr3_programs.
    programs = cl_extr3_programs=>get_instance( i_element_manager = element_manager ).
    programs->add( EXPORTING program        = programname
                   IMPORTING new_element_id = new_element_id
    ).

  ENDMETHOD.
  METHOD _select_sub_packages.

    CLEAR r_packages.

    IF g_is_test EQ abap_false.

      SELECT devclass AS package parentcl AS parentpackage FROM tdevc INTO TABLE r_packages
        FOR ALL ENTRIES IN i_packages_to_search_sub
        WHERE parentcl = i_packages_to_search_sub-package.

    ELSE.

      DATA: package      TYPE ty_package,
            package_test TYPE ty_tdevc_test.

      LOOP AT g_tdevc_test INTO package_test.
        READ TABLE i_packages_to_search_sub TRANSPORTING NO FIELDS WITH TABLE KEY package = package_test-parentcl.
        IF sy-subrc EQ 0.
          package-package = package_test-devclass.
          package-parentpackage = package_test-parentcl.
          INSERT package INTO TABLE r_packages.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD _select_table.

    " Select table

    DATA tables TYPE REF TO cl_extr3_tables.
    tables = cl_extr3_tables=>get_instance( i_element_manager = element_manager ).
    tables->add( EXPORTING table          = name_filter
                 IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.
  METHOD _select_top_packages.

    CLEAR r_packages.

    IF g_is_test EQ abap_false.
      SELECT devclass AS package parentcl AS parentpackage FROM tdevc INTO TABLE r_packages WHERE devclass IN i_top_packages
        ORDER BY devclass.
    ELSE.
      DATA package TYPE ty_package.
      LOOP AT g_tdevc_test INTO package WHERE devclass IN i_top_packages.
        INSERT package INTO TABLE r_packages.
      ENDLOOP.
      SORT r_packages BY package.
    ENDIF.

  ENDMETHOD.
  METHOD _select_class.

    DATA: classes          TYPE REF TO cl_extr3_classes,
          class_components TYPE cl_extr3_classes=>ty_class_components,
          cc               TYPE cl_extr3_classes=>ty_class_component.
    classes = cl_extr3_classes=>get_instance( element_manager = element_manager ).
    classes->add( EXPORTING class            = name_filter
                            is_specific      = abap_true
                  IMPORTING new_element_id   = new_element_id
                            class_components = class_components ).

    LOOP AT class_components INTO cc.
      classes->add_component(
        EXPORTING
          clsname        = cc-clsname
          cmpname        = cc-cmpname
          is_specific    = abap_true ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTR3_MODEL_BUILDER IMPLEMENTATION.
  METHOD initialize.

    element_manager = i_element_manager.

    DATA association_builder_init TYPE builder_type.

    CREATE OBJECT tadir_builder EXPORTING i_element_manager = i_element_manager.

    association_builder_init-association_builder = tadir_builder.
    INSERT association_builder_init INTO TABLE association_builders_init.

    DATA association_builder_post TYPE builder_type.

    CREATE OBJECT tadir_builder EXPORTING i_element_manager = i_element_manager.

    association_builder_post-association_builder = tadir_builder.
    INSERT association_builder_post INTO TABLE association_builders_post.

    DATA association_builder TYPE builder_type.

    CREATE OBJECT where_used_builder EXPORTING i_element_manager = i_element_manager.
    where_used_builder->set_dynamic_read( i_dynamic_read = i_dynamic_read ).

    association_builder-association_builder = where_used_builder.
    INSERT association_builder INTO TABLE association_builders.

  ENDMETHOD.
  METHOD initial_selection_started.
    is_initial_selection = abap_true.
  ENDMETHOD.
  METHOD new_element_id.

    DATA: found_in_level TYPE found_in_level_type,
          is_new_line    TYPE abap_bool.
    FIELD-SYMBOLS <found_in_level> TYPE found_in_level_type.

    ASSERT i_element_id IS NOT INITIAL.

    READ TABLE found_in_levels ASSIGNING <found_in_level> WITH TABLE KEY element_id = i_element_id.
    IF sy-subrc <> 0.

      is_new_line = abap_true.
      ASSIGN found_in_level TO <found_in_level>.
      <found_in_level>-element_id = i_element_id.

    ENDIF.

    IF is_initial_selection EQ abap_true.

      <found_in_level>-found_in_initial_selection = abap_true.
      IF i_is_specific EQ abap_true.

        <found_in_level>-specific = abap_true.

      ENDIF.

    ELSEIF is_up_search EQ abap_true.

      IF <found_in_level>-found_in_level_upsearch IS INITIAL.
        IF <found_in_level>-found_in_initial_selection EQ 'X' AND <found_in_level>-specific EQ 'X'.
          " Do overwrite elements which are initially found and specific with a different level.
          " This would cause errors in down search.
        ELSE.

          <found_in_level>-found_in_level_upsearch = level_for_found_in_upsearch.

        ENDIF.

      ENDIF.

      IF i_is_specific EQ abap_true AND <found_in_level>-specific EQ abap_false.
        IF <found_in_level>-found_in_initial_selection EQ 'X' AND <found_in_level>-specific EQ 'X'.
          " Do overwrite elements which are initially found and specific with a different level.
          " This would cause errors in down search.
        ELSE.

          <found_in_level>-found_in_level_upsearch = level_for_found_in_upsearch.
          <found_in_level>-specific = abap_true.

        ENDIF.

      ENDIF.

    ELSEIF is_down_search EQ abap_true.

      IF <found_in_level>-found_in_level_downsearch IS INITIAL.
        IF <found_in_level>-found_in_initial_selection EQ 'X' AND <found_in_level>-specific EQ 'X'.
          " Do overwrite elements which are initially found and specific with a different level.
          " This would not be correct
        ELSE.

          <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch.

        ENDIF.

      ENDIF.

      IF i_is_specific EQ abap_true AND <found_in_level>-specific EQ abap_false.
        IF <found_in_level>-found_in_initial_selection EQ 'X' AND <found_in_level>-specific EQ 'X'.
          " Do overwrite elements which are initially found and specific with a different level.
          " This would not be correct
        ELSE.

          <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch.
          <found_in_level>-specific = abap_true.

        ENDIF.

      ENDIF.

    ELSEIF is_post_selection EQ abap_true.

      <found_in_level>-found_in_post_selection = abap_true.

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

    IF is_new_line EQ abap_true.

      INSERT <found_in_level> INTO TABLE found_in_levels.
      ASSERT sy-subrc EQ 0.

    ENDIF.

  ENDMETHOD.
  METHOD search.

    _initial_search( ).

    _search_up( i_search_up ).

    _search_down( i_search_down ).

    _post_search( ).

  ENDMETHOD.
  METHOD usage_of_single_element.
    is_usage_of_single_element = abap_true.
  ENDMETHOD.
  METHOD write_found_elements.

    DATA fil TYPE found_in_level_type.
    DATA: fe    TYPE found_element_type.

    LOOP AT found_in_levels INTO fil.
      CLEAR fe.
      fe-specific = fil-specific.
      IF fil-found_in_initial_selection EQ abap_true.
        fe-where = |I|.

      ELSEIF fil-found_in_post_selection EQ abap_true.
        fe-where = |P|.

      ELSE.
        fe-where = |S|.
      ENDIF.

      IF fil-found_in_level_upsearch <> 0
         AND fil-found_in_level_downsearch <> 0.

        fe-level = fil-found_in_level_upsearch.
        fe-alternate_level = -1 * fil-found_in_level_downsearch.

      ELSEIF fil-found_in_level_upsearch <> 0.

        fe-level = fil-found_in_level_upsearch.

      ELSEIF fil-found_in_level_downsearch <> 0.

        fe-level = -1 * fil-found_in_level_downsearch.

      ELSE.
        fe-level = 0.
      ENDIF.

      DATA element TYPE REF TO cl_extr3_elements.

      element = element_manager->get_element( i_element_id = fil-element_id ).

      element->name( EXPORTING element_id   = fil-element_id
                     IMPORTING element_type = fe-element_type
                               parent_name  = fe-parent_name
                               name         = fe-name ).

      IF      is_usage_of_single_element EQ abap_true
          AND fe-specific EQ abap_false.

        CONTINUE.

      ENDIF.

      INSERT fe INTO TABLE fes.

    ENDLOOP.
    IF is_usage_of_single_element EQ abap_true.
      SORT fes BY level alternate_level element_type parent_name name.
    ELSE.
      SORT fes BY where level alternate_level element_type parent_name name.
    ENDIF.

    IF write EQ abap_true.

      WRITE: / 'Legend:'.
      WRITE: / 'W - "I" Found in initial search "P" Found in final search (packages that where not initially selected) "S" Found in regular search'.
      WRITE: / 'L - Level where an element is found'.
      WRITE: / 'AL - In case an element is found in down and up search, this is the level where it is found in down search'.
      WRITE: /.
      FORMAT COLOR COL_HEADING.
      WRITE: /(1) 'W',
             (3) 'L',
             (3) 'AL',
             (30) 'Type',
             (30) 'Name of Parent',
             (61) 'Name'.
      FORMAT COLOR COL_BACKGROUND.

      LOOP AT fes INTO fe.
        NEW-LINE.
        WRITE: /(1) fe-where,
                (3) fe-level,
                (3) fe-alternate_level,
                (30) fe-element_type,
                (30) fe-parent_name,
                (61) fe-name. " Interface method and attributes can yield to very long names. So 2 times 30 plus 1 should be enough.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD _initial_search.

    " Initial search

    DATA: found_in_level         TYPE found_in_level_type,
          first_initial_elements TYPE found_in_levels_type,
          association_builder    TYPE cl_extr3_model_builder=>builder_type.

    " found_in_levels will be updated in this method, so add this elements to a new temporary table.
    IF is_usage_of_single_element EQ abap_false.

      FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
      LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_initial_selection EQ abap_true.

        <fil>-initially_selected_analyzed = abap_true.

        INSERT <fil> INTO TABLE first_initial_elements.

      ENDLOOP.

      LOOP AT association_builders_init INTO association_builder.

        LOOP AT first_initial_elements INTO found_in_level.

          association_builder-association_builder->search_down( element_id = found_in_level-element_id ).

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    is_initial_selection = abap_false.

    IF is_usage_of_single_element EQ abap_false.

      " All initially selected elements are marked as specific so that they are correctly searched

      FIELD-SYMBOLS: <fil2>         TYPE found_in_level_type.
      LOOP AT found_in_levels ASSIGNING <fil2>.
        <fil2>-specific = abap_true.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD _post_search.

    DATA found_in_level TYPE cl_extr3_model_builder=>found_in_level_type.
    DATA association_builder TYPE cl_extr3_model_builder=>builder_type.

    " Post search

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Final actions of search'.

    is_post_selection = abap_true.

    DATA all_elements TYPE found_in_levels_type.

    all_elements = found_in_levels.

    LOOP AT association_builders_post INTO association_builder.

      LOOP AT all_elements INTO found_in_level.

*        IF     is_usage_of_single_element EQ abap_true
*           AND found_in_level-specific EQ abap_false.
*          CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
*        ENDIF.

        association_builder-association_builder->search_up( element_id = found_in_level-element_id ).

      ENDLOOP.

    ENDLOOP.

    is_post_selection = abap_false.

  ENDMETHOD.
  METHOD _search_down.

    " Search down

    is_down_search = abap_true.

    DATA: level_to_search_down      TYPE i,
          something_to_be_done_down TYPE abap_bool.

    IF i_search_down <> 0.

      something_to_be_done_down = abap_true.

      WHILE something_to_be_done_down EQ abap_true.

        DATA temp TYPE string.
        temp = |Search down for level { level_to_search_down }|."To be 7.02 compatible
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = temp.

        something_to_be_done_down = abap_false.

        FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
        LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_level_downsearch = level_to_search_down.
          IF
*          is_usage_of_single_element EQ abap_true
*             AND
             <fil>-specific EQ abap_false.
            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
          ENDIF.
          IF level_to_search_down EQ 0.
            IF <fil>-found_in_initial_selection EQ abap_false.
              " SAP_2_FAMIX_69      When more than a single level is searched down, the down search is not done for elements that where found in the search up
              CONTINUE. "Start searching down with the elements found in the initial selection. Ignore all that was found in upsearch
            ELSE.
              IF is_usage_of_single_element EQ abap_true AND (
                 <fil>-specific EQ abap_false OR
                 <fil>-found_in_level_upsearch > 0 ).
                CONTINUE. " No downsearch for elements that are in initially selected classes but are not initially selected.
                " They are not initially selected when found in level upsearch is greater than zero
              ENDIF.
            ENDIF.

          ENDIF.

          level_for_found_in_downsearch = <fil>-found_in_level_downsearch + 1.

          DATA association_builder TYPE cl_extr3_model_builder=>builder_type.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_down( element_id = <fil>-element_id ).

          ENDLOOP.

          something_to_be_done_down = abap_true.

        ENDLOOP.

        ADD 1 TO level_to_search_down.

        IF i_search_down >= 0 AND i_search_down <= level_to_search_down.

          something_to_be_done_down = abap_false.

        ENDIF.

      ENDWHILE.

    ENDIF.

    is_down_search = abap_false.

  ENDMETHOD.
  METHOD _search_up.

    " Search up

    is_up_search = abap_true.

    DATA: level_to_search_up      TYPE i,
          something_to_be_done_up TYPE abap_bool.

    IF i_search_up <> 0.

      something_to_be_done_up = abap_true.

      WHILE something_to_be_done_up EQ abap_true.

        DATA temp TYPE string.
        temp = |Search up for level { level_to_search_up }|. "To be 7.02 compatible
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = temp.

        something_to_be_done_up = abap_false.

        FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
        LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_level_upsearch = level_to_search_up.

          IF <fil>-specific EQ abap_false.

            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis

          ENDIF.

          level_for_found_in_upsearch = <fil>-found_in_level_upsearch + 1.

          DATA association_builder TYPE cl_extr3_model_builder=>builder_type.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_up( element_id = <fil>-element_id ).

          ENDLOOP.

          something_to_be_done_up = abap_true.

        ENDLOOP.

        ADD 1 TO level_to_search_up.

        IF i_search_up >= 0 AND i_search_up <= level_to_search_up.

          something_to_be_done_up = abap_false.

        ENDIF.

      ENDWHILE.

    ENDIF.

    " SAP_2_FAMIX_68        When more than a single level is searched up, the up search is not done for elements that where found in the search down
    " Fullfilled because the search down starts here

    is_up_search = abap_false.

  ENDMETHOD.
ENDCLASS.
CLASS CL_EXTRACT3 IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
  METHOD extract.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Collect initial elements'.

    model_builder->initial_selection_started( ).

*    DATA element_manager TYPE REF TO cl_extr3_element_manager.
*    CREATE OBJECT element_manager
*      EXPORTING
*        i_model_builder          = model_builder
*        i_exclude_found_sap_intf = i_exclude_found_sap_intf.

*    model_builder->initialize( i_element_manager = element_manager ).

    DATA packages_elements TYPE REF TO cl_extr3_packages.

    packages_elements = cl_extr3_packages=>get_instance( i_element_manager = element_manager ).

    DATA: packages TYPE cl_extr3_initial_elements=>ty_packages,
          package  TYPE cl_extr3_initial_elements=>ty_package.

    packages = initial_elements->get_selected( ).

    LOOP AT packages INTO package.

      packages_elements->add( EXPORTING package = package-package ).

    ENDLOOP.

    model_builder->search( i_search_up           = i_search_up
                           i_search_down         = i_search_down ).

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Make model file'.

    DATA sysid TYPE string.

    sysid = sy-sysid.

    element_manager->collect_infos( sysid ).

    mse_model = element_manager->make_model( ).

  ENDMETHOD.
  METHOD check_if_tested.

    IF g_check_for_test_done EQ 'X'.
      " Buffer result of call to call stack
      is_tested = g_is_tested.
    ELSE.

      DATA et_callstack  TYPE sys_callst  .
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          et_callstack = et_callstack.

      READ TABLE et_callstack TRANSPORTING NO FIELDS WITH KEY eventname = 'INVOKE_TEST_METHOD'.

      IF sy-subrc EQ 0.
        g_is_tested = 'X'.
        is_tested = 'X'.
        g_check_for_test_done = 'X'.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_ACCESS IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Access'.
  ENDMETHOD.
  METHOD set_accessor_accessed_relation.

    DATA ls_accessor_id LIKE LINE OF g_accessor_accessed_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_accessor_id.
    ls_accessor_id-accessor_id = accessor_id.
    ls_accessor_id-accessed_id = accessed_id.
    INSERT ls_accessor_id INTO TABLE g_accessor_accessed_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'accessed'
                                            reference_id   = accessed_id ).
    IF is_write EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isWrite'
                                      is_true            = 'X' ).
    ENDIF.
    IF is_read EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isRead'
                                      is_true            = 'X' ). " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
    ENDIF.
    IF is_dependent EQ 'X'.
      g_model->add_boolean( EXPORTING element_id         = element_id
                                      attribute_name     = 'isDependent'
                                      is_true            = 'X' ). " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_CALL IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Call'.
  ENDMETHOD.
  METHOD set_caller_called_relation.

    DATA ls_caller_called_id LIKE LINE OF g_caller_called_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_caller_called_id.
    ls_caller_called_id-caller_id = caller_id.
    ls_caller_called_id-called_id = called_id.
    INSERT ls_caller_called_id INTO TABLE g_caller_called_ids.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'caller'
                                            reference_id   = caller_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name = 'called'
                                            reference_id   = called_id ).

  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_CODE IMPLEMENTATION.
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
CLASS CL_SOMIX_COMPONENT IMPLEMENTATION.
ENDCLASS.
CLASS CL_SOMIX_COUPLING IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname               = g_elementname
                                   is_named_entity           = abap_false
                                   can_be_referenced_by_name = abap_false
                         IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_DATA IMPLEMENTATION.
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
CLASS CL_SOMIX_ELEMENT IMPLEMENTATION.
ENDCLASS.
CLASS CL_SOMIX_ENTITY IMPLEMENTATION.
  METHOD constructor.
    g_model = model.
  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_EXTRACTION IMPLEMENTATION.
ENDCLASS.
CLASS CL_SOMIX_GROUPING IMPLEMENTATION.
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
    FIELD-SYMBOLS <grouping_id> LIKE LINE OF g_grouping_ids.

    READ TABLE g_grouping_ids ASSIGNING <grouping_id> WITH TABLE KEY grouping_name_group = grouping_name_group
                                                                     grouping = grouping.
    IF sy-subrc EQ 0. "OK
      id = <grouping_id>-id.
    ELSE.
      g_model->add_entity( EXPORTING elementname = g_elementname
                                          is_named_entity = abap_true
                                          can_be_referenced_by_name = abap_true
                                          name_group = grouping_name_group
                                          name = grouping
                                IMPORTING exists_already_with_id = exists_already_with_id
                                          processed_id = id ).

    ENDIF.

    ASSERT unique_name IS NOT INITIAL.
    TRANSLATE unique_name TO LOWER CASE. " To be compatible with specification. Not case sensitive names are here in lower case.
    g_model->add_string( EXPORTING element_id     = id
                                   attribute_name = 'uniqueName'
                                   string         = unique_name ).

    IF technical_type IS NOT INITIAL.

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'technicalType'
                                     string         = technical_type ).

    ENDIF.

    IF link_to_editor IS NOT INITIAL.

      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'linkToEditor'
                                     string         = link_to_editor ).
    ENDIF.

    DATA ls_grouping_id LIKE LINE OF g_grouping_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_grouping_id.
    ls_grouping_id-id                  = id.
    ls_grouping_id-grouping_name_group = grouping_name_group.
    ls_grouping_id-grouping            = grouping.
    INSERT ls_grouping_id INTO TABLE g_grouping_ids.

    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.
CLASS CL_SOMIX_PARENTCHILD IMPLEMENTATION.
  METHOD add.

    DATA ls_parent_id LIKE LINE OF g_parent_child_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion

    READ TABLE g_parent_child_ids INTO ls_parent_id WITH TABLE KEY parent_id = parent_id
                                                                   child_id  = child_id.

    IF sy-subrc EQ 0.
      id = ls_parent_id-element_id.
    ELSE.

      g_model->add_entity( EXPORTING elementname               = g_elementname
                                     is_named_entity           = abap_false
                                     can_be_referenced_by_name = abap_false
                           IMPORTING processed_id = id ).
      g_model->add_reference_by_id( EXPORTING element_id = id
                                              attribute_name = 'parent'
                                              reference_id   = parent_id ).
      g_model->add_reference_by_id( EXPORTING element_id = id
                                              attribute_name = 'child'
                                              reference_id   = child_id ).

      IF is_main EQ 'X'.
        g_model->add_boolean( EXPORTING element_id         = id
                                        attribute_name     = 'isMain'
                                        is_true            = 'X' ).
      ENDIF.

      CLEAR ls_parent_id.
      ls_parent_id-parent_id = parent_id.
      ls_parent_id-child_id = child_id.
      ls_parent_id-element_id = id.
      INSERT ls_parent_id INTO TABLE g_parent_child_ids.
    ENDIF.
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.ParentChild'.
  ENDMETHOD.
ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_eltyp.
  PERFORM fill_f4_eltyp.

START-OF-SELECTION.

  " Inform users when selection is wrong

  IF s_pack IS INITIAL AND
     s_spack IS INITIAL AND
     p_eltyp IS INITIAL AND
     p_elpar IS INITIAL AND
     p_elnam IS INITIAL.

    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'Restrict elements to be extracted'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE: / 'The extractor is not able to extract the whole system'.
    WRITE: / 'You should start with a small quantity of elements, to limit problems when experimenting with the tool'.
    WRITE: / 'Choose a package with only a few 1000 elements'.
    WRITE: / 'Choose a small number for upward (downward) search if needed'.

    RETURN.

  ENDIF.

  " Process user selection

  DATA: mse_model TYPE cl_model=>lines_type.

  DATA sap_extractor TYPE REF TO cl_extract3.

  DATA: ls_pack_line LIKE LINE OF s_pack.
  DATA: lt_pack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_pack INTO ls_pack_line.
    APPEND ls_pack_line TO lt_pack.
  ENDLOOP.

  DATA: ls_spack_line LIKE LINE OF s_pack.
  DATA: lt_spack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_spack INTO ls_spack_line.
    APPEND ls_spack_line TO lt_spack.
  ENDLOOP.

  DATA model_builder TYPE REF TO cl_extr3_model_builder.
  CREATE OBJECT model_builder.


  DATA element_manager TYPE REF TO cl_extr3_element_manager.
  CREATE OBJECT element_manager
    EXPORTING
      i_model_builder           = model_builder
      i_exclude_found_sap_intf  = p_ex
      i_interface_use_structure = p_intrev
      i_use_somix               = p_somix.

  model_builder->initialize( i_element_manager = element_manager
                              i_dynamic_read = p_dyn ).

  DATA: initial_elements TYPE REF TO cl_extr3_initial_elements.
  CREATE OBJECT initial_elements.
  IF     lt_pack IS NOT INITIAL
     AND ( p_eltyp IS INITIAL AND p_elpar IS INITIAL AND p_elnam IS INITIAL ).

    initial_elements->select_packages( EXPORTING top_packages           = lt_pack
                                                 sub_packages_filter    = lt_spack
                                                 including_sub_packages = abap_true ).

  ELSEIF     lt_pack IS INITIAL
         AND ( p_eltyp IS NOT INITIAL OR p_elpar IS NOT INITIAL OR p_elnam IS NOT INITIAL ).

    DATA: p_eltyp_string TYPE string,
          p_elpar_string TYPE string,
          p_elnam_string TYPE string.

    p_eltyp_string = p_eltyp.
    p_elpar_string = p_elpar.
    p_elnam_string = p_elnam.

    IF p_eltyp IS INITIAL.

      FORMAT COLOR COL_NEGATIVE.
      WRITE: / |You have to specify the type of the searched element in field 'Element name'|.
      FORMAT COLOR COL_BACKGROUND.
      WRITE: / |Click into the field to get a value help|.
      RETURN.

    ENDIF.

    IF p_eltyp_string EQ cl_extr3_initial_elements=>select_function OR
       p_eltyp_string EQ cl_extr3_initial_elements=>select_program OR
       p_eltyp_string EQ cl_extr3_initial_elements=>select_table.

      IF p_elpar_string IS NOT INITIAL.

        FORMAT COLOR COL_NEGATIVE.
        WRITE: / 'Do not enter a parent name for this type of element'.
        FORMAT COLOR COL_BACKGROUND.

        RETURN.

      ENDIF.

    ELSEIF p_eltyp_string EQ cl_extr3_initial_elements=>select_class_method.

      " The parentname is empty when a whole class is analyzed and filled when only a component is analyzed

    ELSE.

      FORMAT COLOR COL_NEGATIVE.
      WRITE: / |You have to specify a valid value for the type of the searched element in field 'Element name'|.
      FORMAT COLOR COL_BACKGROUND.
      WRITE: / |The text you entered is not valid|.
      WRITE: / |Click into the field to get a value help|.

      RETURN.

    ENDIF.

    IF p_elnam_string IS INITIAL.

      FORMAT COLOR COL_NEGATIVE.
      WRITE: / |You have to enter the name of the searched element in the field 'Specific element'|.
      FORMAT COLOR COL_BACKGROUND.

      RETURN.

    ENDIF.

    initial_elements->select_specific( EXPORTING model_builder         = model_builder
                                                 element_manager       = element_manager
                                                 i_element_type_filter = p_eltyp_string
                                                 i_parent_name_filter  = p_elpar_string
                                                 i_name_filter         = p_elnam_string ).
  ELSE.

    FORMAT COLOR COL_TOTAL.
    WRITE: / 'Enter either a filter by package or by specific component'.
    FORMAT COLOR COL_BACKGROUND.

  ENDIF.

  CREATE OBJECT sap_extractor.

  DATA nothing_done TYPE boolean.
  sap_extractor->extract( EXPORTING model_builder            = model_builder
                                    element_manager          = element_manager
                                    initial_elements         = initial_elements
                                    i_search_up              = p_nup
                                    i_search_down            = p_ndown
                                    i_exclude_found_sap_intf = abap_true
                          IMPORTING mse_model                = mse_model
                                    nothing_done             = nothing_done ).

  IF nothing_done EQ abap_true.
    RETURN.
  ENDIF.

  DATA model_outputer TYPE REF TO cl_output_model.
  CREATE OBJECT model_outputer.
  model_outputer->make( mse_model = mse_model
                        g_parameter_download_file = p_down
                        i_default_prefix = p_df ).

  model_builder->write_found_elements( EXPORTING write = abap_true ).

FORM fill_f4_eltyp.

  TYPES: BEGIN OF ty_eltyp,
           eltyp TYPE text30,
         END OF ty_eltyp.

  DATA: lt_eltyps TYPE STANDARD TABLE OF ty_eltyp,
        ls_eltyp  TYPE ty_eltyp.

  ls_eltyp-eltyp = cl_extr3_initial_elements=>select_class_method.
  INSERT ls_eltyp INTO TABLE lt_eltyps.
  ls_eltyp-eltyp = cl_extr3_initial_elements=>select_table.
  INSERT ls_eltyp INTO TABLE lt_eltyps.
  ls_eltyp-eltyp = cl_extr3_initial_elements=>select_program.
  INSERT ls_eltyp INTO TABLE lt_eltyps.
  ls_eltyp-eltyp = cl_extr3_initial_elements=>select_function.
  INSERT ls_eltyp INTO TABLE lt_eltyps.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'P_ELTYP'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'p_eltyp'
      value_org   = 'S'
    TABLES
      value_tab   = lt_eltyps.

ENDFORM.
