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

"! This is an experimental prototype, that has errors
"!
"! The latest version are available on https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor
"!
"! The program follows the naming conventions proposed in the ABAP Programming Guidelines from 2009.
"!
"! Semantics of variables and parameters have to be very strict
"! The code shall be able to be read near to as fluent as a well written English text
"! Use ABAP Doc comments if the technical name is not precise enough
"! Tables are in most cases specified with plural (classes). But not always, mse_model is a table.
"!
"! Prefixes are omitted if the reading is simplified
"!
"! Classes are prefixed with cl_ the instances have no prefixes
"! Global attributes are normally prefixed with g_
"! Instances are normally instanciated only once, to simplify coding no singleton pattern is used
"!
"! Object shall be used only for real classes
"! Component shall be used for programming entities like function, class, method, program, database table, attribute, ...
"!
"! Short abbreviations are used if only locally used, in that case an ABAP Doc comments explains the variable
"! See the start of the report for this
"!
"! This is the original version since 23 March 2016 maintained in ABAP 7.31
"!
"! Thanks to Enno Wulff for providing the initial ABAP 7.31 version
"!
"! Last activation:
"! 04.11.2016 12:24 issue35 Rainer Winkler
"!
REPORT yrw1_mc_moose_extractor.
TABLES tadir. "So that select-options work

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE TEXT-001.

PARAMETERS: p_sap AS CHECKBOX DEFAULT 'X'.


SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE TEXT-002.

PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_wdyn AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_intf AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_tables AS CHECKBOX DEFAULT 'X'. "Analyze database tables
PARAMETERS: p_prog AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_iprog AS CHECKBOX DEFAULT ' '. "Internal parts of reports


PARAMETERS: rb_fpack RADIOBUTTON GROUP rbsl DEFAULT 'X'.
"! Filter using package
DATA g_filter_using_package TYPE abap_bool.
g_filter_using_package = rb_fpack.

PARAMETERS: p_pack TYPE parentcl DEFAULT ''.
"! Package to be analyzed
DATA g_parameter_package_to_analyze TYPE parentcl.
g_parameter_package_to_analyze = p_pack.

PARAMETERS: rb_fname RADIOBUTTON GROUP rbsl.
"! Filter using name
DATA g_filter_using_name TYPE abap_bool.
g_filter_using_name = rb_fname.

SELECT-OPTIONS s_pack FOR tadir-devclass.

SELECT-OPTIONS s_compsn FOR tadir-obj_name.

SELECTION-SCREEN END OF BLOCK block_selct_sap_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_using_comp WITH FRAME TITLE TEXT-003.

PARAMETERS: p_dm AS CHECKBOX DEFAULT ' '.
"! Usages outside package grouped
"! If false, a recursive search for using components is performed until no further using components are found
DATA g_param_usage_outpack_groupd TYPE abap_bool.
g_param_usage_outpack_groupd = p_dm.

SELECTION-SCREEN END OF BLOCK block_using_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_infos WITH FRAME TITLE TEXT-004.

PARAMETERS: p_list AS CHECKBOX DEFAULT ' '.
"! List Tokens of selected programs
DATA g_parameter_list_tokens TYPE abap_bool.
g_parameter_list_tokens = p_list.

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

PARAMETERS p_down AS CHECKBOX DEFAULT 'X'.
*"! Download model to file
*DATA g_parameter_download_file TYPE abap_bool.
*g_parameter_download_file = p_down.
SELECTION-SCREEN END OF BLOCK bl_model_settings.



" Begin Model
"! Specifies a model.
"! Create an instance only once, otherwise there will be multiple models each containing only part of the informations.
CLASS cl_model DEFINITION
.
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
      IMPORTING elementname                   TYPE clike
                name_group                    TYPE clike DEFAULT ''
                is_named_entity               TYPE abap_bool
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



  PRIVATE SECTION.
    TYPES: BEGIN OF element_in_model_type,
             element_id      TYPE i,
             is_named_entity TYPE abap_bool,
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
      ADD 1 TO g_attribute_id.
      ls_attribute-attribute_id   = g_attribute_id.
      APPEND ls_attribute TO g_attributes.
    ENDIF.

  ENDMETHOD.
  METHOD add_entity.

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
    ls_elements_in_model-element_type = elementname.
    INSERT ls_elements_in_model INTO TABLE g_elements_in_model.

    IF is_named_entity EQ abap_true.
      me->add_string( EXPORTING element_id = g_processed_id attribute_name = 'name' string = name ).
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
      ADD 1 TO g_attribute_id.
      ls_attribute-attribute_id   = g_attribute_id.
      APPEND ls_attribute TO g_attributes.
    ENDIF.

  ENDMETHOD.
  METHOD add_reference_by_name.

    FIELD-SYMBOLS <named_entity> LIKE LINE OF g_named_entities.

    READ TABLE g_named_entities ASSIGNING <named_entity> WITH TABLE KEY element_type = type_of_reference
                                                                        element_name_group = name_group_of_reference
                                                                        element_name = name_of_reference.
    ASSERT sy-subrc EQ 0. "OK

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
      ADD 1 TO g_attribute_id.
      ls_attribute-attribute_id   = g_attribute_id.
      INSERT ls_attribute INTO TABLE g_attributes.
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
      ADD 1 TO g_attribute_id.
      ls_attribute-attribute_id   = g_attribute_id.
      APPEND ls_attribute TO g_attributes.
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
      IF <element_in_model>-is_named_entity EQ abap_true.

        mse_model_line-line = mse_model_line-line && | (id: | && <element_in_model>-element_id && | )|.
      ENDIF.

      FIELD-SYMBOLS <attribute> LIKE LINE OF g_attributes.
      LOOP AT g_attributes ASSIGNING <attribute> WHERE element_id = <element_in_model>-element_id.

        APPEND mse_model_line TO mse_model.
        mse_model_line-line = |  (| && <attribute>-attribute_type.
        CASE <attribute>-value_type.
          WHEN string_value.

            mse_model_line-line = mse_model_line-line && | '| && <attribute>-name && |')|.

          WHEN reference_value.

            mse_model_line-line = mse_model_line-line && | (ref: | && <attribute>-reference && |))|.

          WHEN boolean_value.

            CASE <attribute>-boolean.
              WHEN abap_true.
                mse_model_line-line = mse_model_line-line && | true)|.
              WHEN abap_false.
                mse_model_line-line = mse_model_line-line && | false)|.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.

          WHEN OTHERS.
            ASSERT 1 = 2.
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
.
  PUBLIC SECTION.
    METHODS make
      IMPORTING
        mse_model TYPE cl_model=>lines_type
        g_parameter_download_file TYPE abap_bool.


ENDCLASS.
CLASS CL_OUTPUT_MODEL IMPLEMENTATION.
  METHOD make.
    " Download the file

    DATA: filename    TYPE string,
          pathname    TYPE string,
          fullpath    TYPE string,
          user_action TYPE i.

    IF g_parameter_download_file EQ abap_true.

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

    FIELD-SYMBOLS <mse_model_line> LIKE LINE OF mse_model.
    LOOP AT mse_model ASSIGNING <mse_model_line>.
      WRITE: / <mse_model_line>-line.
    ENDLOOP.
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
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
  PROTECTED SECTION.
    DATA g_elementname TYPE string.
    DATA g_model TYPE REF TO cl_model.
    DATA g_last_used_id TYPE i.

ENDCLASS.
CLASS CL_FAMIX_ENTITY IMPLEMENTATION.
  METHOD constructor.
    g_model = model.
  ENDMETHOD.
ENDCLASS.


CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM cl_famix_entity
.
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



CLASS cl_famix_named_entity DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT
.
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
                                         parent_package     TYPE clike.

  PROTECTED SECTION.


ENDCLASS.
CLASS CL_FAMIX_NAMED_ENTITY IMPLEMENTATION.
  METHOD add.
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
                                    attribute_name    = 'parentPackage' ).
  ENDMETHOD.
ENDCLASS.


CLASS cl_famix_parameter DEFINITION INHERITING FROM cl_famix_named_entity
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! Set the parent behavioural entity, either a method or a function
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_id | id of parent entity
    METHODS set_parent_behavioural_entity
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_id          TYPE i.


ENDCLASS.
CLASS CL_FAMIX_PARAMETER IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_false
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Parameter'.
  ENDMETHOD.
  METHOD set_parent_behavioural_entity.
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            element_type = element_type
                                            element_name_group = element_name_group
                                            element_name = element_name
                                            attribute_name = 'parentBehaviouralEntity'
                                            reference_id   = parent_id ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_attribute DEFINITION INHERITING FROM cl_famix_named_entity
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    METHODS store_id
      IMPORTING
        class     TYPE clike
        attribute TYPE clike.
    "! Returns the ID for a given attribute of a class
    "! Returns 0 if the attribute is not known
    "! @parameter class | the class of the attribute
    "! @parameter attribute | the attribute name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
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
    "! @parameter parent_name | the name of the parent element
    "! @parameter parent_id | the id of the parent element
    METHODS set_parent_type
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_element     TYPE clike OPTIONAL
        parent_name        TYPE clike OPTIONAL
        parent_id          TYPE i     OPTIONAL.

  PRIVATE SECTION.
    TYPES: BEGIN OF attribute_id_type,
             class     TYPE string,
             attribute TYPE string,
             id        TYPE i,
           END OF attribute_id_type.
    DATA: g_attribute_ids TYPE HASHED TABLE OF attribute_id_type WITH UNIQUE KEY class attribute.
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

    READ TABLE g_attribute_ids ASSIGNING <attribute_id> WITH TABLE KEY class = class attribute = attribute.
    IF sy-subrc EQ 0. "OK
      id = <attribute_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.
  METHOD set_parent_type.
    IF parent_element IS SUPPLIED AND parent_name IS SUPPLIED.
      g_model->add_reference_by_name( EXPORTING element_id        = element_id
                                                element_type       = element_type
                                                element_name_group = element_name_group
                                                element_name       = element_name
                                                attribute_name     = 'parentType'
                                                type_of_reference  = parent_element
                                                name_of_reference  = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING element_id        = element_id
                                                element_type       = element_type
                                                element_name_group = element_name_group
                                                element_name       = element_name
                                                attribute_name     = 'parentType'
                                                reference_id       = parent_id ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
  ENDMETHOD.
  METHOD store_id.
    DATA ls_attribute_id LIKE LINE OF g_attribute_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute_id.
    ls_attribute_id-id = g_last_used_id.
    ls_attribute_id-class = class.
    ls_attribute_id-attribute = attribute.
    INSERT ls_attribute_id INTO TABLE g_attribute_ids.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_container_entity DEFINITION INHERITING FROM cl_famix_named_entity ABSTRACT
.
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
                                    parent_container   TYPE clike.
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


ENDCLASS.
CLASS CL_FAMIX_CONTAINER_ENTITY IMPLEMENTATION.
  METHOD set_container.
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              type_of_reference       = container_element
                                              name_of_reference = parent_container
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



CLASS cl_famix_behavioural_entty DEFINITION INHERITING FROM cl_famix_container_entity ABSTRACT
.
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



CLASS cl_famix_namespace DEFINITION INHERITING FROM cl_famix_container_entity
.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.



ENDCLASS.
CLASS CL_FAMIX_NAMESPACE IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Namespace'.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_package DEFINITION INHERITING FROM cl_famix_named_entity
.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.



ENDCLASS.
CLASS CL_FAMIX_PACKAGE IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
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



CLASS cl_famix_module DEFINITION INHERITING FROM cl_famix_named_entity
.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.



ENDCLASS.
CLASS CL_FAMIX_MODULE IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Module'.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_method DEFINITION INHERITING FROM cl_famix_behavioural_entty
.
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
    "! @parameter parent_name | optional the name of the parent element
    "! @parameter parent_id | optional the id of the parent element
    METHODS set_parent_type
      IMPORTING
        element_id         TYPE i
        element_type       TYPE clike OPTIONAL
        element_name_group TYPE clike OPTIONAL
        element_name       TYPE clike OPTIONAL
        parent_element     TYPE clike
        parent_name        TYPE clike OPTIONAL
        parent_id          TYPE i OPTIONAL.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    METHODS store_id
      IMPORTING
        class  TYPE clike
        method TYPE clike.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                class     TYPE clike
                method    TYPE clike
      RETURNING VALUE(id) TYPE i.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_method_id,
             class  TYPE string,
             method TYPE string,
             id     TYPE i,
           END OF ty_method_id.
    DATA: g_method_ids TYPE HASHED TABLE OF ty_method_id WITH UNIQUE KEY class method.
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

    READ TABLE g_method_ids ASSIGNING <method_id> WITH TABLE KEY class = class
                                                               method = method.
    IF sy-subrc EQ 0. "OK
      id = <method_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.
  METHOD set_parent_type.
    IF parent_name IS SUPPLIED.
      g_model->add_reference_by_name( EXPORTING element_id = element_id
                                                element_type = element_type
                                                element_name_group = element_name_group
                                                element_name = element_name attribute_name    = 'parentType'
                                                type_of_reference       = parent_element
                                                name_of_reference = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name attribute_name = 'parentType'
                                              reference_id   = parent_id ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
  ENDMETHOD.
  METHOD store_id.
    DATA ls_method_id LIKE LINE OF g_method_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_method_id.
    ls_method_id-id = g_last_used_id.
    ls_method_id-class = class.
    ls_method_id-method = method.
    INSERT ls_method_id INTO TABLE g_method_ids.
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_class DEFINITION INHERITING FROM cl_famix_container_entity
.
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
.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.


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



CLASS cl_famix_access DEFINITION INHERITING FROM cl_famix_association
.
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



CLASS cl_famix_invocation DEFINITION INHERITING FROM cl_famix_association
.
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



CLASS cl_famix_inheritance DEFINITION INHERITING FROM cl_famix_association
.
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



CLASS cl_famix_reference DEFINITION INHERITING FROM cl_famix_association
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter target_id | the FAMIX id of the target element
    "! @parameter source_id | the FAMIX id of the source element
    METHODS set_target_source
      IMPORTING
        element_id TYPE i
        target_id  TYPE i
        source_id  TYPE i.


ENDCLASS.
CLASS CL_FAMIX_REFERENCE IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Reference'.
  ENDMETHOD.
  METHOD set_target_source.

    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name    = 'target'
                                            reference_id      = target_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name    = 'source'
                                            reference_id       = source_id ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_famix_custom_source_lng DEFINITION INHERITING FROM cl_famix_entity
.
  PUBLIC SECTION.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING name                          TYPE clike
                EXPORTING VALUE(exists_already_with_id) TYPE i
                          VALUE(id)                     TYPE i.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.


ENDCLASS.
CLASS CL_FAMIX_CUSTOM_SOURCE_LNG IMPLEMENTATION.
  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.
ENDCLASS.



CLASS cl_check_famix_model DEFINITION
.
  PUBLIC SECTION.
    "! Checks a model regarding the FAMIX attributes
    METHODS check IMPORTING model TYPE REF TO cl_model.

  PRIVATE SECTION.

    METHODS _check_attribute_parentpackage
      IMPORTING
        is_public_elements TYPE cl_model=>public_element_type.
    METHODS _check_attribute_name
      IMPORTING
        is_public_elements TYPE cl_model=>public_element_type.
ENDCLASS.
CLASS CL_CHECK_FAMIX_MODEL IMPLEMENTATION.
  METHOD check.

    DATA public_elements TYPE model->public_elements_type.

    public_elements = model->get_model( ).
    DATA ls_public_elements TYPE model->public_element_type.
    DATA ls_public_attribute TYPE  model->public_attribute_type.
    LOOP AT public_elements INTO ls_public_elements.

      _check_attribute_parentpackage( ls_public_elements ).

      _check_attribute_name( ls_public_elements ).

    ENDLOOP.

  ENDMETHOD.
  METHOD _check_attribute_name.

    DATA ls_public_attribute TYPE cl_model=>public_attribute_type.

    " Check attribute parentPackage
    DATA count_name TYPE i.
    CLEAR count_name.
    LOOP AT is_public_elements-public_attributes INTO ls_public_attribute WHERE attribute_type = 'name'.

      ADD 1 TO count_name.

      CONDENSE ls_public_attribute-string.
      IF ls_public_attribute-string IS INITIAL.
        FORMAT COLOR COL_NEGATIVE.

        " SAP_2_FAMIX_51        Return a message if the attribute name is empty
        WRITE: / 'Element ', is_public_elements-element_id, ' has an attribute name that is empty'.

        FORMAT COLOR COL_BACKGROUND.
      ENDIF.

    ENDLOOP.

    " SAP_2_FAMIX_50        Return a message if the attribute name occurs more than once

    IF count_name > 1.

      FORMAT COLOR COL_NEGATIVE.

      WRITE: / 'Element ', is_public_elements-element_id, ' has more than a single attribute name'.

      FORMAT COLOR COL_BACKGROUND.

    ENDIF.

  ENDMETHOD.
  METHOD _check_attribute_parentpackage.

    DATA ls_public_attribute TYPE cl_model=>public_attribute_type.

    " Check attribute parentPackage
    DATA count_parent_packages TYPE i.
    CLEAR count_parent_packages.
    LOOP AT is_public_elements-public_attributes INTO ls_public_attribute WHERE attribute_type = 'parentPackage'.

      ADD 1 TO count_parent_packages.

    ENDLOOP.

    " SAP_2_FAMIX_49        Return a message if the attribute parent package occurs more than once

    IF count_parent_packages > 1.

      FORMAT COLOR COL_NEGATIVE.

      WRITE: / 'Package ', is_public_elements-element_id, ' has more than a single parent package'.

      FORMAT COLOR COL_BACKGROUND.

    ENDIF.

    " SAP_2_FAMIX_62        Return a message if a class has no parent package assigned
    IF is_public_elements-element_type EQ 'FAMIX.Class'.
      IF count_parent_packages EQ 0.

        FORMAT COLOR COL_NEGATIVE.

        WRITE: / 'Class ', is_public_elements-element_id, ' has no parent package'.

        FORMAT COLOR COL_BACKGROUND.

      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.



CLASS cl_make_demo_model DEFINITION
.
  PUBLIC SECTION.
    CLASS-METHODS make
      EXPORTING
        mse_model TYPE cl_model=>lines_type.


ENDCLASS.
CLASS CL_MAKE_DEMO_MODEL IMPLEMENTATION.
  METHOD make.
    DATA model            TYPE REF TO cl_model.
    CREATE OBJECT model.

    DATA famix_namespace  TYPE REF TO cl_famix_namespace.
    CREATE OBJECT famix_namespace EXPORTING model = model.
    DATA famix_package      TYPE REF TO cl_famix_package.
    CREATE OBJECT famix_package EXPORTING model = model.
    DATA famix_class        TYPE REF TO cl_famix_class.
    CREATE OBJECT famix_class EXPORTING model = model.
    DATA famix_method         TYPE REF TO cl_famix_method.
    CREATE OBJECT famix_method EXPORTING model = model.
    DATA famix_attribute    TYPE REF TO cl_famix_attribute.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    DATA famix_inheritance  TYPE REF TO cl_famix_inheritance.
    CREATE OBJECT famix_inheritance EXPORTING model = model.
    DATA check_famix_model TYPE REF TO cl_check_famix_model.
    CREATE OBJECT check_famix_model.

    DATA last_id TYPE i.

    famix_namespace->add( name = 'aNamespace' ).
    famix_package->add( name = 'aPackage' ).
    " Add error to test issue22
    famix_package->add( name = 'bPackage' ).
    " End of adding error to test issue 22
    famix_package->add( EXPORTING name = 'anotherPackage' IMPORTING id = last_id ).
    famix_package->set_parent_package( element_id = last_id parent_package = 'aPackage' ).
    " Test SAP_2_FAMIX_52       Do not add attributes twice if they are added with identical attributes
    " Add the same attribute twice
    famix_package->set_parent_package( element_id = last_id parent_package = 'aPackage' ).
    " Add error to test issue22
    famix_package->set_parent_package( element_id = last_id parent_package = 'bPackage' ).
    " End of adding error to test issue 22
    " Add error to test issue24
    famix_class->add( name = '' ).
    " End of adding error to test issue 24
    famix_class->add( EXPORTING name = 'ClassA' IMPORTING id = last_id ).
    famix_class->set_container( EXPORTING element_id = last_id
                                          container_element = 'FAMIX.Namespace'
                                          parent_container  = 'aNamespace').
    famix_class->set_parent_package( element_id = last_id
                                     parent_package = 'aPackage' ).

    famix_method->add( EXPORTING name = 'methodA1' IMPORTING id = last_id ).
    famix_method->set_signature( element_id = last_id signature = 'methodA1()' ).
    famix_method->set_parent_type( element_id = last_id
                                   parent_element = 'FAMIX.Class'
                                   parent_name    = 'ClassA' ).
    famix_attribute->add( EXPORTING name = 'attributeA1' IMPORTING id = last_id ).
    famix_attribute->set_parent_type( element_id = last_id
                                      parent_element = 'FAMIX.Class'
                                      parent_name    = 'ClassA' ).
    famix_class->add( EXPORTING name = 'ClassB' IMPORTING id = last_id ).
    famix_class->set_container( element_id = last_id
                                container_element = 'FAMIX.Namespace'
                                parent_container  = 'aNamespace' ).
    famix_class->set_parent_package( element_id = last_id
                                     parent_package = 'anotherPackage' ).

    last_id = famix_inheritance->add( ).
    famix_inheritance->set_sub_and_super_class( EXPORTING element_id = last_id
                                                          subclass_element   = 'FAMIX.Class'
                                                          subclass_name_group = ''
                                                          subclass_name      = 'ClassB'
                                                          superclass_element = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name    = 'ClassA' ).

    check_famix_model->check( model = model ).

    model->make_mse( IMPORTING mse_model = mse_model ).
  ENDMETHOD.
ENDCLASS.

******************************************** End Include Z_FAMIX_ABAP *****************************

" include z_sap_2_famix
******************************************** Begin Include Z_SAP_2_FAMIX ****************************

"! This is the master class for all classes that build a model for the SAP system.
"! Its main usage is in the moment to display the connection of the classes to extract SAP model data in the extracted model.
"! It may later be replaced by cl_sap_abap, cl_sap_bw, ...
CLASS cl_sap DEFINITION
.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.
CLASS CL_SAP IMPLEMENTATION.
ENDCLASS.



CLASS cl_sap_package DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    TYPES packages_type TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING
        name TYPE clike.
    "! Call once to set the parent package
    METHODS set_parent_package
      IMPORTING
        this_package   TYPE clike
        parent_package TYPE clike.
    "! Returns all packages that are stored up to this time
    METHODS get_all_packages
      RETURNING VALUE(packages) TYPE packages_type.

  PRIVATE SECTION.
    DATA: g_famix_package TYPE REF TO cl_famix_package.
    DATA: g_added_packages TYPE STANDARD TABLE OF devclass.
ENDCLASS.
CLASS CL_SAP_PACKAGE IMPLEMENTATION.
  METHOD add.
    DATA name_typed TYPE devclass.
    g_famix_package->add( name = name ).
    name_typed = name.
    INSERT name_typed INTO TABLE g_added_packages.
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_package EXPORTING model = model.
  ENDMETHOD.
  METHOD get_all_packages.
    SORT g_added_packages.
    DELETE ADJACENT DUPLICATES FROM g_added_packages.
    packages = g_added_packages.
  ENDMETHOD.
  METHOD set_parent_package.
    g_famix_package->set_parent_package( element_id = 0
                                         element_type = 'FAMIX.Package'
                                         element_name_group = ''
                                         element_name = this_package
                                         parent_package = parent_package ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_sap_class DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Add global class
    "! @parameters modifiers | will be available in FAMIX in the attribute modifiers
    METHODS add
      IMPORTING name                          TYPE clike
                modifiers                     TYPE clike
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i.
    "! Specify the parent program for a local class
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_program
      IMPORTING
        element_id  TYPE i
        sap_program TYPE clike.
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.
    METHODS is_interface
      IMPORTING
        element_id TYPE i.
    "! Add local class of a program
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter program | the name of the program the local class is part of
    "! @parameters modifiers | will be available in FAMIX in the attribute modifiers
    METHODS add_local
      IMPORTING
        program   TYPE clike
        name      TYPE any
        modifiers TYPE clike
      RETURNING
        VALUE(id) TYPE i.

  PRIVATE SECTION.
    DATA: g_famix_class TYPE REF TO cl_famix_class.
ENDCLASS.
CLASS CL_SAP_CLASS IMPLEMENTATION.
  METHOD add.
    " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
    " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
    g_famix_class->add( EXPORTING name_group             = ''
                                  name                   = name
                                  modifiers              = modifiers
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id                     = id ).
  ENDMETHOD.
  METHOD add_local.
    g_famix_class->add( EXPORTING name_group = program
                                  name       = name
                                  modifiers  = modifiers
                        IMPORTING id = id ).
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_class EXPORTING model = model.
  ENDMETHOD.
  METHOD is_interface.
    g_famix_class->is_interface( element_id = element_id ).
  ENDMETHOD.
  METHOD set_parent_package.
    g_famix_class->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.
  METHOD set_parent_program.

    " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

    g_famix_class->set_container( EXPORTING element_id = element_id
                                            container_element = 'FAMIX.Module'
                                            parent_container  = sap_program ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_sap_db_table DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Add global Database table
    METHODS add
      IMPORTING name                          TYPE clike
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i
                VALUE(dummy_attribute_id)     TYPE i.
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.

  PRIVATE SECTION.
    CONSTANTS modifier_dbtable TYPE string VALUE 'DBTable' ##NO_TEXT.
    DATA: g_famix_class     TYPE REF TO cl_famix_class,
          g_famix_attribute TYPE REF TO cl_famix_attribute.
ENDCLASS.
CLASS CL_SAP_DB_TABLE IMPLEMENTATION.
  METHOD add.
    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
    g_famix_class->add( EXPORTING name_group             = ''
                                  name                   = name
                                  modifiers              = modifier_dbtable
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id = id ).
    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
    g_famix_attribute->add( EXPORTING name = name IMPORTING id = dummy_attribute_id ).
    g_famix_attribute->set_parent_type( EXPORTING element_id = dummy_attribute_id
                                                  parent_id  = id ).
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_class EXPORTING model = model.
    CREATE OBJECT g_famix_attribute EXPORTING model = model.
  ENDMETHOD.
  METHOD set_parent_package.
    g_famix_class->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.
ENDCLASS.



CLASS cl_sap_attribute DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS get_id
      IMPORTING
        class     TYPE clike
        attribute TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    METHODS add
      IMPORTING
        class     TYPE clike
        attribute TYPE clike.

  PRIVATE SECTION.
    DATA: g_famix_attribute TYPE REF TO cl_famix_attribute.
ENDCLASS.
CLASS CL_SAP_ATTRIBUTE IMPLEMENTATION.
  METHOD add.
    DATA last_id TYPE i.
    g_famix_attribute->add( EXPORTING name = attribute IMPORTING id = last_id ).
    g_famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                  parent_element = 'FAMIX.Class'
                                                  parent_name    = class ).
    g_famix_attribute->store_id( EXPORTING class     = class
                                           attribute = attribute ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_attribute EXPORTING model = model.
  ENDMETHOD.
  METHOD get_id.
    " SAP_2_FAMIX_13        Mapp attributes of classes to FAMIX.Attribute
    " SAP_2_FAMIX_14        Mapp attributes of interfaces to FAMIX.Attribute
    id = g_famix_attribute->get_id( class     = class
                                    attribute = attribute ).
  ENDMETHOD.
ENDCLASS.

"! Specifies a SAP method
CLASS cl_sap_method DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Returns the ID for a given method of a global class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
        class     TYPE clike
        method    TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    "! Add a method for a global SAP class or a global SAP instance
    METHODS add
      IMPORTING
        class     TYPE clike
        method    TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    METHODS add_local_method
      IMPORTING
        class_name  TYPE clike
        class_id    TYPE i
        method_name TYPE clike
      RETURNING
        VALUE(id)   TYPE i.

  PRIVATE SECTION.
    DATA: g_famix_method TYPE REF TO cl_famix_method.

ENDCLASS.
CLASS CL_SAP_METHOD IMPLEMENTATION.
  METHOD add.
    " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
    " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
    g_famix_method->add( EXPORTING name = method IMPORTING id = id ).

    " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
    " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( element_id = id
                                   signature = method ).

    g_famix_method->set_parent_type( EXPORTING element_id = id
                                               parent_element = 'FAMIX.Class'
                                               parent_name    = class ).

    g_famix_method->store_id( EXPORTING class  = class
                                        method = method ).

  ENDMETHOD.
  METHOD add_local_method.

    " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method
    g_famix_method->add( EXPORTING name_group = class_name " TBD Why name of class in name_group?
                                        name       = method_name
                                        IMPORTING id = id ).
    " SAP_2_FAMIX_43        Fill the attribute signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( element_id = id
                                   signature = method_name ).

    " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class
    g_famix_method->set_parent_type( EXPORTING element_id = id
                                               parent_element = 'FAMIX.Class'
                                               parent_id      =  class_id ).
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_method EXPORTING model = model.
  ENDMETHOD.
  METHOD get_id.
    id = g_famix_method->get_id( class  = class
                                 method = method ).
  ENDMETHOD.
ENDCLASS.


CLASS cl_sap_inheritance DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add RETURNING VALUE(id) TYPE i.
    METHODS set_sub_and_super_class
      IMPORTING
        element_id      TYPE i
        subclass_name   TYPE clike
        superclass_name TYPE clike.
    METHODS set_interface_for_class
      IMPORTING
        element_id     TYPE i
        interface_name TYPE clike
        class_name     TYPE clike.
    METHODS set_local_sub_and_super_class
      IMPORTING
        element_id      TYPE i
        program         TYPE clike
        subclass_name   TYPE any
        superclass_name TYPE any.

  PRIVATE SECTION.
    DATA: g_famix_inheritance TYPE REF TO cl_famix_inheritance.
ENDCLASS.
CLASS CL_SAP_INHERITANCE IMPLEMENTATION.
  METHOD add.
    id = g_famix_inheritance->add( ).
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_inheritance EXPORTING model = model.
  ENDMETHOD.
  METHOD set_interface_for_class.

    " SAP_2_FAMIX_40        Map all interface implementations of interfaces in selected packages by classes of selected packages by FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = ''
                                                            subclass_name         = interface_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = ''
                                                            superclass_name       = class_name ).
  ENDMETHOD.
  METHOD set_local_sub_and_super_class.

    " SAP_2_FAMIX_38        Map local inheritances of classes to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = program
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = program
                                                            superclass_name       = superclass_name ).
  ENDMETHOD.
  METHOD set_sub_and_super_class.

    " SAP_2_FAMIX_39     Map all inheritances between classes in selected packages to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( EXPORTING element_id = element_id
                                                            subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = ''
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = ''
                                                            superclass_name       = superclass_name ).
  ENDMETHOD.
ENDCLASS.


CLASS cl_sap_invocation DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add_invocation
      IMPORTING
        used_method  TYPE i
        using_method TYPE i.

  PRIVATE SECTION.
    DATA: g_famix_invocation TYPE REF TO cl_famix_invocation.
ENDCLASS.
CLASS CL_SAP_INVOCATION IMPLEMENTATION.
  METHOD add_invocation.
    " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
    IF g_famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method
                                                           candidates_id = used_method )
       EQ abap_true.

      DATA invocation_id TYPE i.
      invocation_id = g_famix_invocation->add( ).
      g_famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                 sender_id     = using_method
                                                                 candidates_id = used_method
                                                                 signature     = 'DUMMY' ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_invocation EXPORTING model = model.
  ENDMETHOD.
ENDCLASS.

CLASS cl_sap_access DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add_access
      IMPORTING
        used_attribute TYPE i
        using_method   TYPE i.

  PRIVATE SECTION.
    DATA: g_famix_access TYPE REF TO cl_famix_access.
ENDCLASS.
CLASS CL_SAP_ACCESS IMPLEMENTATION.
  METHOD add_access.
    " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

    IF g_famix_access->is_new_access( accessor_id = using_method
                                      variable_id = used_attribute )
       EQ abap_true.
      DATA last_id TYPE i.
      last_id = g_famix_access->add( ).
      g_famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id
                                                                accessor_id = using_method
                                                                variable_id = used_attribute ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_access EXPORTING model = model.
  ENDMETHOD.
ENDCLASS.

CLASS cl_sap_program DEFINITION INHERITING FROM cl_sap
.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING
        name      TYPE clike
      RETURNING
        VALUE(id) TYPE i.
    "! Call once to set the parent package of a program
    "! @parameter element_id | the ID of the element where the ID shall be added
    METHODS set_parent_package
      IMPORTING
        element_id     TYPE i
        parent_package TYPE clike.

  PRIVATE SECTION.
    DATA g_famix_module TYPE REF TO cl_famix_module.
ENDCLASS.
CLASS CL_SAP_PROGRAM IMPLEMENTATION.
  METHOD add.

    " SAP_2_FAMIX_5     Map program to FAMIX.Module
    g_famix_module->add( EXPORTING name = name IMPORTING id = id ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_module EXPORTING model = model.
  ENDMETHOD.
  METHOD set_parent_package.
    g_famix_module->set_parent_package( element_id = element_id parent_package = parent_package ).
  ENDMETHOD.
ENDCLASS.

******************************************** End Include Z_SAP_2_FAMIX ******************************

"! Analyze ABAP Statement of type K (Other ABAP key word)
CLASS cl_ep_analyze_other_keywrd DEFINITION
.
  PUBLIC SECTION.
    TYPES: BEGIN OF indexed_token_type,
         index TYPE i,
         str   TYPE string,
         row   TYPE token_row,
         col   TYPE token_col,
         type  TYPE token_type,
       END OF indexed_token_type.

TYPES sorted_tokens_type TYPE SORTED TABLE OF indexed_token_type WITH UNIQUE KEY index.
    METHODS constructor
      IMPORTING
        sorted_tokens TYPE sorted_tokens_type.
    METHODS analyze
      IMPORTING
        statement TYPE sstmnt.
    TYPES statement_type TYPE c LENGTH 2.
    CONSTANTS:

      start_class_definition      TYPE statement_type VALUE 'CD',
      start_class_implementation  TYPE statement_type VALUE 'CI',
      end_class                   TYPE statement_type VALUE 'CE',
      method_definition           TYPE statement_type VALUE 'MD',
      start_method_implementation TYPE statement_type VALUE 'MI',
      end_method_implementation   TYPE statement_type VALUE 'ME',
      attribute_definition        TYPE statement_type VALUE 'AD',
      start_public                TYPE statement_type VALUE 'PU',
      start_protected             TYPE statement_type VALUE 'PO',
      start_private               TYPE statement_type VALUE 'PR'.


    TYPES: BEGIN OF info_type,
             statement_type      TYPE statement_type,
             is_class_stmnt_info TYPE abap_bool,
             class_is_inheriting TYPE abap_bool,
             class_inherits_from TYPE string,
             is_static           TYPE abap_bool,
             name                TYPE string,
           END OF info_type.
    DATA: g_info TYPE info_type READ-ONLY.

  PRIVATE SECTION.
    DATA g_sorted_tokens TYPE sorted_tokens_type.
ENDCLASS.

"! Analyzes local objects of ABAP programs
"! Is not yet completely implemented
CLASS cl_program_analyzer DEFINITION
.
  PUBLIC SECTION.
    METHODS extract
      IMPORTING
        module_reference TYPE i
        program          TYPE char30
      CHANGING
        model            TYPE REF TO cl_model.

  PRIVATE SECTION.
    CONSTANTS modifiers_abaplocalclass TYPE string VALUE 'ABAPLocalClass' ##NO_TEXT.
    DATA g_parameter_list_tokens TYPE abap_bool.

ENDCLASS.

CLASS cl_extract_sap DEFINITION
.
  PUBLIC SECTION.
    TYPES:BEGIN OF db_table_type,
            db_table                       TYPE tabname,
            id_in_model                    TYPE i,
            id_of_dummy_attribute_in_model TYPE i,
          END OF db_table_type.
    TYPES: db_tables_type TYPE HASHED TABLE OF db_table_type WITH UNIQUE KEY db_table.
    TYPES:         ty_s_compsn                       TYPE RANGE OF tadir-obj_name.
    TYPES:         ty_s_pack                         TYPE RANGE OF tadir-devclass.
    METHODS constructor
      IMPORTING
        i_g_filter_using_package       TYPE abap_bool
        i_g_filter_using_name          TYPE abap_bool
        i_g_parameter_package_to_analz TYPE parentcl
        i_p_iprog                      TYPE abap_bool
        i_p_clas                       TYPE abap_bool
        i_p_wdyn                       TYPE abap_bool
        i_p_intf                       TYPE abap_bool
        i_p_prog                       TYPE abap_bool
        i_p_tables                     TYPE abap_bool
        i_s_compsn                     TYPE ty_s_compsn
        i_s_pack                       TYPE ty_s_pack
        i_g_param_usage_outpack_groupd TYPE abap_bool.
    METHODS extract
      EXPORTING
        mse_model           TYPE cl_model=>lines_type
        VALUE(nothing_done) TYPE abap_bool.

  PRIVATE SECTION.

    TYPES: BEGIN OF class_component_type,
             clsname TYPE seocompo-clsname,
             cmpname TYPE seocompo-cmpname,
             cmptype TYPE seocompo-cmptype,
           END OF class_component_type.

    TYPES component_type TYPE string.

    TYPES: BEGIN OF component_infos_type,
             component      TYPE component_type,
             component_name TYPE string,
             package        TYPE devclass,
           END OF component_infos_type.

    TYPES components_infos_type TYPE HASHED TABLE OF component_infos_type WITH UNIQUE KEY component component_name.

    TYPES: BEGIN OF map_tadir_component_type,
             object    TYPE trobjtype, " The SAP TADIR Name
             component TYPE component_type, " As called here
           END OF map_tadir_component_type.

    TYPES tadir_components_mapping_type TYPE HASHED TABLE OF map_tadir_component_type WITH UNIQUE KEY object
                                                                                WITH UNIQUE HASHED KEY comp COMPONENTS component.

    TYPES: BEGIN OF class_interface_type,
             obj_name TYPE seoclsname,
           END OF class_interface_type.

    TYPES: BEGIN OF program_type,
             program TYPE char30,
           END OF program_type.

    TYPES:BEGIN OF class_type,
            class TYPE seoclsname,
          END OF class_type.



    TYPES: BEGIN OF inheritance_type,
             clsname    TYPE seometarel-clsname,
             refclsname TYPE seometarel-refclsname,
             reltype    TYPE seometarel-reltype,
           END OF inheritance_type.

    "! Maps the component lists from SAP (table TADIR) to the component list used in this program
    DATA g_tadir_components_mapping TYPE tadir_components_mapping_type.
    DATA g_filter_using_package TYPE abap_bool."TBD fill
    DATA g_filter_using_name TYPE abap_bool."TBD fill
    DATA g_parameter_package_to_analyze TYPE parentcl.
    DATA p_iprog TYPE abap_bool.
    DATA p_clas TYPE abap_bool.
    DATA p_wdyn TYPE abap_bool.
    DATA p_intf TYPE abap_bool.
    DATA p_prog TYPE abap_bool.
    DATA p_tables TYPE abap_bool.
    DATA s_compsn TYPE RANGE OF tadir-obj_name.
    DATA s_pack TYPE RANGE OF tadir-devclass.
    DATA g_param_usage_outpack_groupd TYPE abap_bool."TBD fill

    CONSTANTS comptype_attribute TYPE seocmptype VALUE '0'.
    CONSTANTS comptype_method TYPE seocmptype VALUE '1'.
    CONSTANTS globclass_component_key TYPE string VALUE 'GlobClass' ##NO_TEXT.
    CONSTANTS webdynpro_component_comp_key TYPE string VALUE 'WebDynproComponent' ##NO_TEXT.
    CONSTANTS globintf_component_key TYPE string VALUE 'GlobIntf' ##NO_TEXT.
    CONSTANTS abapprogram_component_key TYPE string VALUE 'ABAPProgram' ##NO_TEXT.
    CONSTANTS databasetable_component_key TYPE string VALUE 'DataBaseTable' ##NO_TEXT.
    CONSTANTS tadir_clas TYPE string VALUE 'CLAS' ##NO_TEXT.
    "! WebDynpro component
    CONSTANTS tadir_wdyn TYPE string VALUE 'WDYN' ##NO_TEXT.
    CONSTANTS tadir_intf TYPE string VALUE 'INTF' ##NO_TEXT.
    CONSTANTS tadir_prog TYPE string VALUE 'PROG' ##NO_TEXT.
    CONSTANTS tadir_tabl TYPE string VALUE 'TABL' ##NO_TEXT.
    CONSTANTS modifier_abapglobalclass TYPE string VALUE 'ABAPGlobalClass' ##NO_TEXT.
    CONSTANTS modifier_abapglobalinterface TYPE string VALUE 'ABAPGlobalInterface' ##NO_TEXT.
    CONSTANTS modifier_webdynpro_component TYPE string VALUE 'ABAPWebDynproComponent'.





    METHODS _set_default_language
      IMPORTING
        model TYPE REF TO cl_model.

    TYPES: BEGIN OF package_type,
             devclass TYPE devclass,
           END OF package_type.
    TYPES:
      processed_packages_type TYPE HASHED TABLE OF package_type WITH UNIQUE KEY devclass.
    METHODS _determine_packages_to_analyze
      IMPORTING
        sap_package               TYPE REF TO cl_sap_package
        package_first             TYPE tdevc
      RETURNING
        VALUE(processed_packages) TYPE processed_packages_type.
    TYPES:
      classes_type  TYPE STANDARD TABLE OF class_interface_type WITH DEFAULT KEY,
      programs_type TYPE STANDARD TABLE OF program_type WITH DEFAULT KEY.
    METHODS _analyze_components
      IMPORTING
        components_infos TYPE components_infos_type
      EXPORTING
        VALUE(classes)   TYPE classes_type
        VALUE(programs)  TYPE programs_type
        VALUE(db_tables) TYPE db_tables_type.
    METHODS _read_all_programs
      IMPORTING
        sap_package      TYPE REF TO cl_sap_package
        sap_program      TYPE REF TO cl_sap_program
        components_infos TYPE components_infos_type
        programs         TYPE programs_type
      CHANGING
        model            TYPE REF TO cl_model.
    TYPES:existing_classes_type TYPE HASHED TABLE OF class_type WITH UNIQUE KEY class.
    METHODS _add_classes_to_model
      IMPORTING
        sap_package      TYPE REF TO cl_sap_package
        sap_class        TYPE REF TO cl_sap_class
        components_infos TYPE components_infos_type
        existing_classes TYPE existing_classes_type.
    METHODS _add_tables_to_model
      IMPORTING
        sap_package      TYPE REF TO cl_sap_package
        components_infos TYPE components_infos_type
      CHANGING
        db_tables        TYPE db_tables_type
        sap_db_table     TYPE REF TO cl_sap_db_table.
    METHODS _determine_inheritances_betwee
      IMPORTING
        sap_inheritance  TYPE REF TO cl_sap_inheritance
        existing_classes TYPE existing_classes_type.
    TYPES: class_components_type   TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.
    METHODS _determine_class_components
      IMPORTING
        existing_classes        TYPE existing_classes_type
      RETURNING
        VALUE(class_components) TYPE class_components_type.
    METHODS _add_to_class_components_to_mo
      IMPORTING
        class_components TYPE class_components_type
        sap_method       TYPE REF TO cl_sap_method
        sap_attribute    TYPE REF TO cl_sap_attribute.
    METHODS _determine_usage_of_methods
      IMPORTING
                sap_class                   TYPE REF TO cl_sap_class
                class_components            TYPE class_components_type
                sap_package                 TYPE REF TO cl_sap_package
                sap_method                  TYPE REF TO cl_sap_method
                sap_attribute               TYPE REF TO cl_sap_attribute
                sap_invocation              TYPE REF TO cl_sap_invocation
                sap_access                  TYPE REF TO cl_sap_access
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.

    METHODS _determine_usage_of_db_tables
      IMPORTING
                sap_class                   TYPE REF TO cl_sap_class
                class_components            TYPE class_components_type
                sap_package                 TYPE REF TO cl_sap_package
                sap_method                  TYPE REF TO cl_sap_method
                sap_attribute               TYPE REF TO cl_sap_attribute
                sap_invocation              TYPE REF TO cl_sap_invocation
                sap_access                  TYPE REF TO cl_sap_access
                db_tables                   TYPE db_tables_type
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.

    "! Determine usages for components
    "! If using components are not part of the model, they are either added or replaced by a dummy component
    "! Either provide class_component or db_table
    METHODS _determine_usages
      IMPORTING
                sap_class                   TYPE REF TO cl_sap_class
                class_component             TYPE class_component_type OPTIONAL
                db_table                    TYPE db_table_type OPTIONAL
                sap_package                 TYPE REF TO cl_sap_package
                sap_method                  TYPE REF TO cl_sap_method
                sap_invocation              TYPE REF TO cl_sap_invocation
                sap_access                  TYPE REF TO cl_sap_access
                used                        TYPE i
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.
    TYPES:
      classes_4_type        TYPE STANDARD TABLE OF class_interface_type WITH DEFAULT KEY.
    METHODS _read_all_classes
      IMPORTING
        classes                 TYPE classes_4_type
      RETURNING
        VALUE(existing_classes) TYPE existing_classes_type.
    "! Evaluate user selection and return initial list of objects to analyze
    "! @parameter nothing_selected | nothing is selected
    METHODS _select_requested_components
      IMPORTING
        sap_package           TYPE REF TO cl_sap_package
        package_to_analyze    TYPE parentcl
        select_by_top_package TYPE abap_bool
      EXPORTING
        components_infos      TYPE components_infos_type
        nothing_selected      TYPE abap_bool.
    METHODS _handle_used_by_class
      IMPORTING
        i_sap_class                   TYPE REF TO cl_sap_class
        i_class_component             TYPE class_component_type
        i_sap_package                 TYPE REF TO cl_sap_package
        i_sap_method                  TYPE REF TO cl_sap_method
        i_sap_invocation              TYPE REF TO cl_sap_invocation
        i_sap_access                  TYPE REF TO cl_sap_access
        i_used                        TYPE i
        i_class_component_is_supplied TYPE abap_bool
        ib_table_is_supplied          TYPE abap_bool
        i_using_class                 TYPE string
        i_using_method                TYPE string
        i_modifier_of_using_class     TYPE string
      RETURNING
        VALUE(r_new_components_infos) TYPE components_infos_type.
    METHODS _convert_compnt_to_famix_model
      IMPORTING
        component       TYPE string
      RETURNING
        VALUE(modifier) TYPE string.

ENDCLASS.
CLASS CL_EXTRACT_SAP IMPLEMENTATION.
  METHOD constructor.
    g_filter_using_package = i_g_filter_using_package.
    g_filter_using_name = i_g_filter_using_name.
    g_parameter_package_to_analyze = i_g_parameter_package_to_analz.
    p_iprog = i_p_iprog.
    p_clas = i_p_clas.
    p_wdyn = i_p_wdyn.
    p_intf = i_p_intf.
    p_prog = i_p_prog.
    p_tables = i_p_tables.
    s_compsn = i_s_compsn.
    s_pack = i_s_pack.
    g_param_usage_outpack_groupd = i_g_param_usage_outpack_groupd.
  ENDMETHOD.
  METHOD extract.

    TYPES:BEGIN OF package_type,
            devclass TYPE devclass,
          END OF package_type.

    DATA components_infos TYPE components_infos_type.
    DATA new_components_infos TYPE components_infos_type.
    DATA processed_components_infos TYPE components_infos_type.
    DATA classes TYPE STANDARD TABLE OF class_interface_type.
    DATA programs TYPE STANDARD TABLE OF program_type.
    DATA db_tables TYPE cl_extract_sap=>db_tables_type.
    DATA existing_classes TYPE HASHED TABLE OF class_type WITH UNIQUE KEY class.

    DATA class_components TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.

    " Do not use singleton pattern, but define each instance only one time at the start. Multiple instanciation allowed unless
    " specified for the class

    DATA model            TYPE REF TO cl_model.
    CREATE OBJECT model.
    DATA sap_package     TYPE REF TO cl_sap_package.
    CREATE OBJECT sap_package EXPORTING model = model.
    DATA sap_program     TYPE REF TO cl_sap_program.
    CREATE OBJECT sap_program EXPORTING model = model.
    DATA sap_class TYPE REF TO cl_sap_class.
    CREATE OBJECT sap_class EXPORTING model = model.
    DATA sap_inheritance TYPE REF TO cl_sap_inheritance.
    CREATE OBJECT sap_inheritance EXPORTING model = model.
    DATA sap_method TYPE REF TO cl_sap_method.
    CREATE OBJECT sap_method EXPORTING model = model.
    DATA sap_attribute   TYPE REF TO cl_sap_attribute.
    CREATE OBJECT sap_attribute EXPORTING model = model.
    DATA sap_invocation  TYPE REF TO cl_sap_invocation.
    CREATE OBJECT sap_invocation EXPORTING model = model.
    DATA sap_access      TYPE REF TO cl_sap_access.
    CREATE OBJECT sap_access EXPORTING model = model.
    DATA check_famix_model TYPE REF TO cl_check_famix_model.
    CREATE OBJECT check_famix_model.

    DATA sap_db_table TYPE REF TO cl_sap_db_table.
    CREATE OBJECT sap_db_table EXPORTING model = model.

    " Set TADIR mapping
    DATA ls_mapping TYPE map_tadir_component_type. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_mapping.
    ls_mapping-object = tadir_clas.
    ls_mapping-component = globclass_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_wdyn.
    ls_mapping-component = webdynpro_component_comp_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_intf.
    ls_mapping-component = globintf_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_prog.
    ls_mapping-component = abapprogram_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_tabl.
    ls_mapping-component = databasetable_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    _set_default_language( model ).

    DATA nothing_selected TYPE abap_bool.

    IF g_filter_using_package EQ abap_true.
      DATA select_by_top_package TYPE boolean.
      select_by_top_package = abap_true.
    ELSEIF g_filter_using_name EQ abap_true.
      select_by_top_package = abap_false.
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
    _select_requested_components( EXPORTING  sap_package          = sap_package
                                             select_by_top_package = select_by_top_package
                                             package_to_analyze    = g_parameter_package_to_analyze
                                   IMPORTING components_infos      = components_infos
                                             nothing_selected      = nothing_selected ).



    " Select requested components by complex filters





    IF nothing_selected EQ abap_true.
      nothing_done = abap_true.
      RETURN.
    ENDIF.

    WHILE lines( components_infos ) <> 0.

      _analyze_components( EXPORTING components_infos = components_infos
                           IMPORTING classes          = classes
                                     programs         = programs
                                     db_tables        = db_tables ).

      "TBD finalize issue8 add db tables to extractor

      _read_all_programs( EXPORTING sap_package    = sap_package
                                    sap_program      = sap_program
                                    components_infos = components_infos
                                    programs         = programs
                           CHANGING model = model ).

      existing_classes = _read_all_classes( classes ).

      _add_classes_to_model( sap_package     = sap_package
                             sap_class       = sap_class
                             components_infos  = components_infos
                             existing_classes  = existing_classes ).

      _add_tables_to_model( EXPORTING sap_package      = sap_package
                                      components_infos = components_infos
                             CHANGING db_tables        = db_tables
                                      sap_db_table     = sap_db_table ).

      _determine_inheritances_betwee( sap_inheritance = sap_inheritance
                                      existing_classes  = existing_classes ).

      class_components = _determine_class_components( existing_classes ).

      _add_to_class_components_to_mo( class_components = class_components
                                      sap_method       = sap_method
                                      sap_attribute    = sap_attribute ).

      new_components_infos = _determine_usage_of_methods( sap_class        = sap_class
                                                          class_components = class_components
                                                          sap_package      = sap_package
                                                          sap_method       = sap_method
                                                          sap_attribute    = sap_attribute
                                                          sap_invocation   = sap_invocation
                                                          sap_access       = sap_access ).

      " SAP_2_FAMIX_55      Determine Usages of database tables by class methods
      INSERT LINES OF _determine_usage_of_db_tables( sap_class        = sap_class
                                                           class_components = class_components
                                                           sap_package      = sap_package
                                                           sap_method       = sap_method
                                                           sap_attribute    = sap_attribute
                                                           sap_invocation   = sap_invocation
                                                           sap_access       = sap_access
                                                           db_tables        = db_tables ) INTO TABLE new_components_infos.

      " Determine package for new components

      " TBD Find more performant solution

      FIELD-SYMBOLS <component_infos> LIKE LINE OF new_components_infos.
      LOOP AT new_components_infos ASSIGNING <component_infos>.
        "READ
        DATA object TYPE trobjtype.
        DATA ls_tadir LIKE LINE OF g_tadir_components_mapping.
        READ TABLE g_tadir_components_mapping
              INTO ls_tadir
              WITH KEY component  = <component_infos>-component.
        ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing
        object = ls_tadir-object.

        SELECT SINGLE devclass FROM tadir
          INTO <component_infos>-package
         WHERE pgmid = 'R3TR'
           AND object = object
           AND obj_name = <component_infos>-component_name.

        IF sy-subrc <> 0. "OK
          " TBD
          " Report errors
          DELETE new_components_infos WHERE component = <component_infos>-component
                                        AND component_name = <component_infos>-component_name.
        ENDIF.

      ENDLOOP.

      INSERT LINES OF components_infos INTO TABLE processed_components_infos.

      CLEAR components_infos.

      " Add parent package for new global classes

      LOOP AT new_components_infos ASSIGNING <component_infos>.

        IF <component_infos>-component EQ globclass_component_key
         OR <component_infos>-component EQ globintf_component_key
         OR <component_infos>-component EQ webdynpro_component_comp_key.
          " GlobClass
          DATA: id_of_new_component TYPE i.

          sap_class->add(
            EXPORTING
              name                   = <component_infos>-component_name
              modifiers              = _convert_compnt_to_famix_model( <component_infos>-component )
            IMPORTING
              id                     = id_of_new_component ).

          sap_package->add( name = <component_infos>-package ).

          sap_class->set_parent_package( EXPORTING element_id     = id_of_new_component
                                                   parent_package = <component_infos>-package ).
        ELSE.
          FORMAT COLOR COL_NEGATIVE.

          WRITE: / 'For new element of type ', <component_infos>-component, 'no determination of parent package specified'.

          FORMAT COLOR COL_BACKGROUND.
        ENDIF.

      ENDLOOP.

      " SAP_2_FAMIX_48      Allow to select all using objects
      " Fullfilled by adding new_components_infos to components_infos and repeating the analysis in the while loop

      FIELD-SYMBOLS <component_infos_2> LIKE LINE OF new_components_infos.
      LOOP AT new_components_infos ASSIGNING <component_infos_2>.

        READ TABLE processed_components_infos TRANSPORTING NO FIELDS WITH TABLE KEY component = <component_infos_2>-component component_name = <component_infos_2>-component_name.

        IF sy-subrc <> 0. "OK

          INSERT <component_infos_2> INTO TABLE components_infos.

        ENDIF.

      ENDLOOP.

    ENDWHILE.



    " Add parent packages for packages

    DATA packages TYPE sap_package->packages_type.
    packages = sap_package->get_all_packages( ).
    IF packages IS NOT INITIAL.
      TYPES: BEGIN OF devclass_type,
               devclass TYPE tdevc-devclass,
             END OF devclass_type.
      DATA packages_with_type_devclass TYPE STANDARD TABLE OF devclass_type WITH KEY devclass.


      DATA package LIKE LINE OF packages.
      LOOP AT packages INTO package.
        DATA ls_package_with_type_devclass LIKE LINE OF packages_with_type_devclass.
        CLEAR ls_package_with_type_devclass.
        ls_package_with_type_devclass-devclass = package.
        APPEND ls_package_with_type_devclass TO packages_with_type_devclass.

      ENDLOOP.
      TYPES: BEGIN OF packages_info_type,
               devclass TYPE tdevc-devclass,
               parentcl TYPE tdevc-parentcl,
             END OF  packages_info_type.
      DATA package_info TYPE packages_info_type.
      DATA packages_info TYPE STANDARD TABLE OF packages_info_type WITH DEFAULT KEY.

      SELECT  devclass parentcl FROM tdevc INTO TABLE packages_info FOR ALL ENTRIES IN packages_with_type_devclass WHERE devclass = packages_with_type_devclass-devclass.
      LOOP AT packages_info INTO package_info.
        IF package_info-parentcl IS NOT INITIAL.
          sap_package->add( name = package_info-parentcl ). " So that parent class can always be assigned
          sap_package->add( name = package_info-devclass ).
          sap_package->set_parent_package( this_package = package_info-devclass
                                           parent_package = package_info-parentcl ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    check_famix_model->check( model = model ).

    model->make_mse( IMPORTING mse_model = mse_model ).

  ENDMETHOD.
  METHOD _add_classes_to_model.

    " Add to model
    DATA existing_class LIKE LINE OF existing_classes.
    LOOP AT existing_classes INTO existing_class.



      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = globclass_component_key component_name = existing_class-class.
      IF sy-subrc <> 0. "OK
        " It may be an interface
        READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = globintf_component_key component_name = existing_class-class.

        IF sy-subrc <> 0. "OK
          " It may be a Web Dynpro component
          READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = webdynpro_component_comp_key component_name = existing_class-class.
          ASSERT sy-subrc EQ 0. "OK
        ENDIF.
      ENDIF.

      sap_package->add( EXPORTING name = <component_infos>-package ).

      DATA last_id TYPE i.

      IF <component_infos>-component EQ globintf_component_key.
        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_abapglobalinterface
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        sap_class->is_interface( element_id = last_id ).
      ELSEIF <component_infos>-component EQ globclass_component_key.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_abapglobalclass
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
      ELSEIF <component_infos>-component EQ webdynpro_component_comp_key.
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_webdynpro_component
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
      ELSE.
        ASSERT 1 = 2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD _add_tables_to_model.

    FIELD-SYMBOLS: <db_table> LIKE LINE OF db_tables.
    LOOP AT db_tables ASSIGNING <db_table>.

      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = databasetable_component_key component_name = <db_table>-db_table.
      ASSERT sy-subrc EQ 0.
      sap_package->add( EXPORTING name = <component_infos>-package ).

      sap_db_table->add( EXPORTING name = <db_table>-db_table IMPORTING id                 = <db_table>-id_in_model
                                                                        dummy_attribute_id = <db_table>-id_of_dummy_attribute_in_model ).

      sap_db_table->set_parent_package( element_id     = <db_table>-id_in_model
                                        parent_package = <component_infos>-package ).

    ENDLOOP.

  ENDMETHOD.
  METHOD _add_to_class_components_to_mo.

    " Add to class components to model

    DATA class_component LIKE LINE OF class_components.
    LOOP AT class_components INTO class_component.

      CASE class_component-cmptype.
        WHEN comptype_attribute. "Attribute

          DATA existing_id TYPE i.
          existing_id =  sap_attribute->get_id( class     = class_component-clsname
                                                attribute = class_component-cmpname ).
          IF existing_id EQ 0. " not found

            sap_attribute->add( EXPORTING class     = class_component-clsname
                                          attribute = class_component-cmpname ).
*            famix_attribute->set_parent_type(
*              EXPORTING
*                parent_element = 'FAMIX.Class'
*                parent_name    = CONV string( class_component-clsname ) ).
*            famix_attribute->store_id( EXPORTING class     = CONV string( class_component-clsname )
*                                                 attribute = CONV string( class_component-cmpname ) ).

          ENDIF.

        WHEN comptype_method. "Method

          existing_id = sap_method->get_id( class  = class_component-clsname
                                            method = class_component-cmpname ).

          IF existing_id EQ 0. " not found

            sap_method->add( class  = class_component-clsname
                             method = class_component-cmpname ).

          ENDIF.
        WHEN 2. "Event
        WHEN 3. "Type
        WHEN OTHERS.
          " TBD Warn

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD _analyze_components.

    " Loop over all packages to find classes and programms

    " SAP_2_FAMIX_1     Extract classes from Dictionary
    " SAP_2_FAMIX_2     Extract interfaces as FAMIX.Class with attribute isinterface

    DATA class LIKE LINE OF classes.

    FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.

    LOOP AT components_infos ASSIGNING <component_infos>.
      MOVE-CORRESPONDING <component_infos> TO class.
      INSERT class INTO TABLE classes.

      IF <component_infos>-component EQ globclass_component_key
      OR <component_infos>-component EQ globintf_component_key
      OR <component_infos>-component EQ webdynpro_component_comp_key.

        class-obj_name = <component_infos>-component_name.
        INSERT class INTO TABLE classes.

      ELSEIF <component_infos>-component EQ abapprogram_component_key.
        DATA ls_program LIKE LINE OF programs. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
        CLEAR ls_program.
        ls_program-program = <component_infos>-component_name.
        INSERT ls_program INTO TABLE programs.
      ELSEIF <component_infos>-component EQ databasetable_component_key.
        DATA ls_db_table LIKE LINE OF db_tables.
        CLEAR ls_db_table.
        ls_db_table-db_table = <component_infos>-component_name.
        INSERT ls_db_table INTO TABLE db_tables.
      ELSE.
        ASSERT 1 = 2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD _convert_compnt_to_famix_model.

    " convert component to FAMIX modifier

    CASE component.
      WHEN globclass_component_key.
        modifier = modifier_abapglobalclass.
      WHEN globintf_component_key.
        modifier = modifier_abapglobalinterface.
      WHEN webdynpro_component_comp_key.
        modifier = modifier_webdynpro_component.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

  ENDMETHOD.
  METHOD _determine_class_components.

    " Determine class components

    " SAP_2_FAMIX_9         Extract methods of classes
    " SAP_2_FAMIX_10        Extract methods of interfaces
    " SAP_2_FAMIX_11        Extract attributes of classes
    " SAP_2_FAMIX_12        Extract attributes of interfaces

    IF existing_classes IS NOT INITIAL.
      "
      SELECT clsname cmpname cmptype FROM seocompo INTO TABLE class_components
        FOR ALL ENTRIES IN existing_classes
        WHERE
          clsname = existing_classes-class.

    ENDIF.

  ENDMETHOD.
  METHOD _determine_inheritances_betwee.

    " Determine inheritances between selected classes

    DATA: inheritances TYPE STANDARD TABLE OF  inheritance_type.

    IF existing_classes IS NOT INITIAL.
      SELECT clsname refclsname reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE inheritances
        FOR ALL ENTRIES IN existing_classes WHERE clsname = existing_classes-class
                                               AND version = 1.
    ENDIF.

    " Delete all inheritances where superclass is not in selected packages
    DATA inheritance LIKE LINE OF inheritances.
    LOOP AT inheritances INTO inheritance.
      READ TABLE existing_classes TRANSPORTING NO FIELDS WITH TABLE KEY class = inheritance-refclsname.
      IF sy-subrc <> 0. "OK
        DELETE inheritances.
      ENDIF.
    ENDLOOP.

    " Add inheritances to model
    DATA inheritance_2 LIKE LINE OF inheritances.
    LOOP AT inheritances INTO inheritance_2.
      CASE inheritance_2-reltype.
        WHEN 2.
          " Inheritance
          DATA inheritance_id TYPE i.
          inheritance_id = sap_inheritance->add( ).
          sap_inheritance->set_sub_and_super_class( EXPORTING element_id = inheritance_id
                                                              subclass_name         = inheritance_2-clsname
                                                              superclass_name       = inheritance_2-refclsname ).
        WHEN 1.
          " Interface implementation

          inheritance_id = sap_inheritance->add( ).
          sap_inheritance->set_interface_for_class( EXPORTING element_id = inheritance_id
                                                              interface_name         = inheritance_2-clsname
                                                              class_name       = inheritance_2-refclsname ).

        WHEN 0.
          " Interface composition     (i COMPRISING i_ref)
          " TBD
        WHEN 5.
          " Enhancement            ( c enhances c_ref)
          " TBD
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD _determine_packages_to_analyze.

    " Determine packages to analyze

    "! A temporal helper table used to find all packages (development classes) in the selection
    DATA temp_packages_to_search TYPE STANDARD TABLE OF package_type.

    sap_package->add( name = package_first-devclass ).

    DATA ls_processed_package LIKE LINE OF processed_packages. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_processed_package.
    ls_processed_package-devclass = package_first-devclass.
    INSERT ls_processed_package INTO TABLE processed_packages.

    DATA ls_temp_package_to_search LIKE LINE OF temp_packages_to_search. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_temp_package_to_search.
    ls_temp_package_to_search-devclass = g_parameter_package_to_analyze.
    INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.
    WHILE temp_packages_to_search IS NOT INITIAL.
      IF temp_packages_to_search IS NOT INITIAL.
        TYPES: BEGIN OF abap_731_package_type,
                 devclass TYPE tdevc-devclass,
                 parentcl TYPE tdevc-parentcl,
               END OF abap_731_package_type.
        DATA: packages TYPE STANDARD TABLE OF abap_731_package_type WITH DEFAULT KEY.
        SELECT devclass  parentcl FROM tdevc INTO TABLE packages
         FOR ALL ENTRIES IN temp_packages_to_search
          WHERE parentcl = temp_packages_to_search-devclass.
      ENDIF.

      CLEAR temp_packages_to_search.

      DATA package LIKE LINE OF packages.
      LOOP AT packages INTO package.

        CLEAR ls_processed_package.
        ls_processed_package-devclass = package-devclass.
        INSERT ls_processed_package INTO TABLE processed_packages.
        IF sy-subrc EQ 0. "OK
          " New package
          " Search again
          CLEAR ls_temp_package_to_search.
          ls_temp_package_to_search-devclass = package-devclass.
          INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.
          sap_package->add( name = package-devclass ).
          IF package-parentcl IS NOT INITIAL.
            " Assume that this is not a top package but that there exists a parent package
            sap_package->set_parent_package( this_package = package-devclass
                                             parent_package = package-parentcl ).
          ENDIF.
        ENDIF.

      ENDLOOP.

      SORT temp_packages_to_search.
      DELETE ADJACENT DUPLICATES FROM temp_packages_to_search.

    ENDWHILE.

  ENDMETHOD.
  METHOD _determine_usages.

    DATA where_used_name TYPE eu_lname.
    DATA class_component_is_supplied TYPE abap_bool.
    DATA db_table_is_supplied TYPE abap_bool.
    IF class_component IS SUPPLIED.
      class_component_is_supplied = abap_true.
      CASE class_component-cmptype.
        WHEN comptype_method.

          " SAP_2_FAMIX_17      Determine usage of class methods by programs and classes
          " SAP_2_FAMIX_18      Determine usage of interface methods by programs and classes

          where_used_name = class_component-clsname && |\\ME:| && class_component-cmpname.
          DATA where_used_components TYPE STANDARD TABLE OF wbcrossgt.
          SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'ME' AND name = where_used_name.
        WHEN comptype_attribute.

          " SAP_2_FAMIX_19      Determine usage of class attributes by programs and classes
          " SAP_2_FAMIX_20      Determine usage of interface attributes by programs and classes

          where_used_name = class_component-clsname && |\\DA:| && class_component-cmpname.
          SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'DA' AND name = where_used_name.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
    ELSEIF db_table IS SUPPLIED.
      db_table_is_supplied = abap_true.
      where_used_name = db_table-db_table.
      SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'TY' AND name = where_used_name.

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

    FIELD-SYMBOLS <where_used_component> LIKE LINE OF where_used_components.
    LOOP AT where_used_components ASSIGNING <where_used_component>.

      DATA: using_class TYPE string.
      DATA: using_method TYPE string.
      DATA: modifier_of_using_class TYPE string.

      DATA ls_mtdkey TYPE seocpdkey.
      CALL FUNCTION 'SEO_METHOD_GET_NAME_BY_INCLUDE'
        EXPORTING
          progname = <where_used_component>-include
        IMPORTING
          mtdkey   = ls_mtdkey.
      IF ls_mtdkey IS NOT INITIAL.
*        FIND FIRST OCCURRENCE OF '~' IN ls_mtdkey-cpdname.
*        if sy-subrc eq 0.
        " This is an interface. Reverse the usage direction
        " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method


*        else.
        using_class = ls_mtdkey-clsname.

        " Used by method

        IF ls_mtdkey-cpdname IS INITIAL.
          using_method = 'DUMMY'.
        ELSE.
          using_method = ls_mtdkey-cpdname.
        ENDIF.

        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'

        modifier_of_using_class = modifier_abapglobalclass.

*        ENDIF.

        INSERT LINES OF _handle_used_by_class(
             i_sap_class                   = sap_class
             i_class_component             = class_component
             i_sap_package                 = sap_package
             i_sap_method                  = sap_method
             i_sap_invocation              = sap_invocation
             i_sap_access                  = sap_access
             i_used                        = used
             i_class_component_is_supplied = class_component_is_supplied
             ib_table_is_supplied          = db_table_is_supplied
             i_using_class                 = using_class
             i_using_method                = using_method
             i_modifier_of_using_class     = modifier_of_using_class ) INTO TABLE new_components_infos.

      ELSE.
        "Check for usage in Web Dynpro ABAP
        DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.
        SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = <where_used_component>-include AND srtf2 = 0.
        IF sy-subrc EQ 0.

          using_class = ls_wd_sourcemap-component_name.
          using_method = ls_wd_sourcemap-controller_name.
          modifier_of_using_class = modifier_webdynpro_component.

          INSERT LINES OF _handle_used_by_class(
                i_sap_class                   = sap_class
                i_class_component             = class_component
                i_sap_package                 = sap_package
                i_sap_method                  = sap_method
                i_sap_invocation              = sap_invocation
                i_sap_access                  = sap_access
                i_used                        = used
                i_class_component_is_supplied = class_component_is_supplied
                ib_table_is_supplied          = db_table_is_supplied
                i_using_class                 = using_class
                i_using_method                = using_method
                i_modifier_of_using_class     = modifier_of_using_class ) INTO TABLE new_components_infos.

        ELSE.

          " TBD Implement other usages
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD _determine_usage_of_db_tables.
    DATA db_table LIKE LINE OF db_tables.

    LOOP AT db_tables INTO db_table.

      INSERT LINES OF _determine_usages( sap_class        = sap_class
                                         db_table         = db_table
                                         sap_package      = sap_package
                                         sap_method       = sap_method
                                         sap_invocation   = sap_invocation
                                         sap_access       = sap_access
                                         used          = db_table-id_of_dummy_attribute_in_model ) INTO TABLE new_components_infos.

    ENDLOOP.

  ENDMETHOD.
  METHOD _determine_usage_of_methods.

    DATA class_component TYPE class_component_type.

    " Determine usage of methods

    LOOP AT class_components INTO class_component WHERE cmptype = comptype_attribute  " Methods
                                                     OR cmptype = comptype_method. "Attributes

      CASE class_component-cmptype.
        WHEN comptype_method.
          DATA used_id TYPE i.
          used_id = sap_method->get_id( class  = class_component-clsname
                                        method = class_component-cmpname ).

        WHEN comptype_attribute.
          used_id = sap_attribute->get_id( class     = class_component-clsname
                                            attribute = class_component-cmpname ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      INSERT LINES OF _determine_usages( sap_class        = sap_class
                                         class_component  = class_component
                                         sap_package      = sap_package
                                         sap_method       = sap_method
                                         sap_invocation   = sap_invocation
                                         sap_access       = sap_access
                                         used          = used_id ) INTO TABLE new_components_infos.

    ENDLOOP.

  ENDMETHOD.
  METHOD _handle_used_by_class.

    DATA tadir_object TYPE string.
    CASE i_modifier_of_using_class.
      WHEN modifier_abapglobalclass.
        tadir_object = tadir_clas.
      WHEN modifier_webdynpro_component.
        tadir_object = tadir_wdyn.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    " Handle used by class
    DATA using_method_id TYPE i.
    using_method_id = i_sap_method->get_id( class  = i_using_class
                                          method = i_using_method ).
    IF using_method_id EQ 0.

      IF g_param_usage_outpack_groupd EQ abap_false.

        " Method does not exist, create the class
        " SAP_2_FAMIX_21      If a component is used by a class that is not selected, add this class to the model
        " SAP_2_FAMIX_22      Do not assign classes that included due to usage to a package


        DATA exists_already_with_id TYPE i.
        i_sap_class->add( EXPORTING name      = i_using_class
                                  modifiers = i_modifier_of_using_class
                        IMPORTING exists_already_with_id = exists_already_with_id ).

        IF exists_already_with_id IS INITIAL.

          " SAP_2_FAMIX_47      If no dummy class is specified in case a using class is not in the initial selection, analyze this classes also

          DATA ls_new_components_info LIKE LINE OF r_new_components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion

          DATA ls_tadir_comp_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
          READ TABLE g_tadir_components_mapping INTO ls_tadir_comp_map WITH TABLE KEY object = tadir_object.
          ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing
          CLEAR ls_new_components_info.
          ls_new_components_info-component_name = i_using_class.
          ls_new_components_info-component   = ls_tadir_comp_map-component .
          INSERT ls_new_components_info INTO TABLE r_new_components_infos.

        ENDIF.

      ELSE.
        " SAP_2_FAMIX_35        Add usage to a single dummy class "OTHER_SAP_CLASS" if required by a parameter
        DATA id TYPE i.
        i_sap_class->add( EXPORTING name                   = 'OTHER_SAP_CLASS'
                                  modifiers              = modifier_abapglobalclass
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id                     = id ).
        i_sap_package->add( name = 'OTHER_SAP_PACKAGE' ).
        i_sap_class->set_parent_package( element_id     = id
                                       parent_package = 'OTHER_SAP_PACKAGE' ).

      ENDIF.

      " Now there is a class, but no duplicate class

      IF g_param_usage_outpack_groupd EQ abap_false.
        using_method_id = i_sap_method->get_id( class  = i_using_class
                                              method = i_using_method ).
      ELSE.
        using_method_id = i_sap_method->get_id( class  = 'OTHER_SAP_CLASS'
                                              method = 'OTHER_SAP_METHOD' ).
      ENDIF.


      IF using_method_id EQ 0.
        IF g_param_usage_outpack_groupd EQ abap_false.
          " Now also the method is to be created
          " SAP_2_FAMIX_23       If a component is used by a class that is not selected, add the using methods to the model

          using_method_id = i_sap_method->add( class  = i_using_class
                                             method = i_using_method ).

        ELSE.

          " SAP_2_FAMIX_36        Add a usage to a single dummy method "OTHER_SAP_METHOD" if required by a parameter

          using_method_id = i_sap_method->add( class  = 'OTHER_SAP_CLASS'
                                             method = 'OTHER_SAP_METHOD'  ).

        ENDIF.
      ENDIF.

    ENDIF.

    IF i_class_component_is_supplied EQ abap_true.
      CASE i_class_component-cmptype.
        WHEN comptype_method.

          i_sap_invocation->add_invocation( used_method  = i_used
                                          using_method = using_method_id ).

        WHEN comptype_attribute.

          i_sap_access->add_access( used_attribute = i_used
                                  using_method   = using_method_id ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
    ELSEIF ib_table_is_supplied EQ abap_true.

      " SAP_2_FAMIX_57      Model usages of database tables by a an access to the dummy attribute of the modelled FAMIX Class
      i_sap_access->add_access( used_attribute  = i_used
                              using_method = using_method_id ).

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.
  METHOD _read_all_classes.

    " Read all classes

    " Determine existing classes
    IF classes IS NOT INITIAL.
      SELECT clsname AS class FROM seoclass INTO TABLE existing_classes FOR ALL ENTRIES IN classes
        WHERE
          clsname = classes-obj_name.

      SELECT component_name AS class FROM wdy_component APPENDING TABLE existing_classes FOR ALL ENTRIES IN classes
        WHERE
          component_name = classes-obj_name
          AND version = 'A'.

    ENDIF.

  ENDMETHOD.
  METHOD _read_all_programs.

    " Read all programs

    " SAP_2_FAMIX_4     Extract programs

    FIELD-SYMBOLS <program> LIKE LINE OF programs.
    LOOP AT programs ASSIGNING <program>.

      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos>
            WITH TABLE KEY component = abapprogram_component_key
                           component_name = <program>-program.
      ASSERT sy-subrc EQ 0. "OK

      sap_package->add( name  = <component_infos>-package ).

      DATA module_reference TYPE i.
      module_reference = sap_program->add( name = <program>-program ).

      sap_program->set_parent_package( element_id = module_reference
                                       parent_package = <component_infos>-package ).

      IF p_iprog EQ abap_true.

        DATA program_analyzer TYPE REF TO cl_program_analyzer.
        CREATE OBJECT program_analyzer.

        program_analyzer->extract( EXPORTING module_reference = module_reference
                                             program          = <program>-program
                                    CHANGING model            = model ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD _select_requested_components.

    DATA first_package TYPE tdevc.
    DATA processed_packages TYPE cl_extract_sap=>processed_packages_type.
    DATA object TYPE trobjtype.

    IF select_by_top_package EQ abap_true.

      " Select components in package and sub package
      " SAP_2_FAMIX_3     Select all components in a package and the sub packages of this package

      SELECT SINGLE devclass parentcl FROM tdevc INTO first_package WHERE devclass = package_to_analyze.
      IF sy-subrc <> 0. "OK
        WRITE: 'Package does not exist: ', package_to_analyze.
        nothing_selected  = abap_true.
      ENDIF.

      processed_packages = _determine_packages_to_analyze( sap_package = sap_package
                                                           package_first = first_package ).

    ENDIF.

    IF   select_by_top_package EQ abap_false
      OR processed_packages IS NOT INITIAL.
      DO 5 TIMES.
        CASE sy-index.
          WHEN 1.
            IF p_clas EQ abap_true.
              object = tadir_clas.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 2.
            IF p_wdyn EQ abap_true.
              object = tadir_wdyn.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 3.
            IF p_intf EQ abap_true.
              object = tadir_intf.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 4.
            IF p_prog EQ abap_true.
              object = tadir_prog.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 5.
            IF p_tables EQ abap_true.
              " SAP_2_FAMIX_53        Extract database tables
              object = tadir_tabl.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        IF select_by_top_package EQ abap_true.
          DATA: BEGIN OF tadir_component,
                  obj_name TYPE tadir-obj_name,
                  object   TYPE tadir-object,
                  devclass TYPE tadir-devclass,
                END OF tadir_component.
          SELECT obj_name object devclass FROM tadir INTO tadir_component FOR ALL ENTRIES IN processed_packages
            WHERE pgmid = 'R3TR'
              AND object = object
              AND devclass = processed_packages-devclass.

            DATA ls_component_info LIKE LINE OF components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
            DATA ls_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion

            READ TABLE g_tadir_components_mapping INTO ls_map WITH TABLE KEY object = tadir_component-object.
            ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing

            CLEAR ls_component_info.
            ls_component_info-component = ls_map-component.
            ls_component_info-component_name = tadir_component-obj_name.
            ls_component_info-package = tadir_component-devclass.
            INSERT ls_component_info INTO TABLE components_infos.

          ENDSELECT.
        ELSE.
          SELECT obj_name object devclass FROM tadir INTO tadir_component
            WHERE pgmid = 'R3TR'
              AND object = object
              AND obj_name IN s_compsn
              AND devclass IN s_pack.

            DATA ls_map_2 LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
            READ TABLE g_tadir_components_mapping INTO ls_map_2 WITH TABLE KEY object = tadir_component-object.
            ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing

            CLEAR ls_component_info.
            ls_component_info-component = ls_map_2-component.
            ls_component_info-component_name = tadir_component-obj_name.
            ls_component_info-package = tadir_component-devclass.
            INSERT ls_component_info INTO TABLE components_infos.

          ENDSELECT.
        ENDIF.
      ENDDO.
    ENDIF.

    " Select only real tables
    DATA: tabclass TYPE dd02l-tabclass.
    LOOP AT components_infos INTO ls_component_info WHERE component = databasetable_component_key.

      SELECT SINGLE tabclass FROM dd02l INTO tabclass WHERE tabname = ls_component_info-component_name
                                                        AND as4local = 'A'
                                                        AND as4vers = ''.
      IF sy-subrc <> 0.
        "TBD report error
      ELSE.
        IF tabclass EQ 'INTTAB' OR tabclass EQ 'VIEW'.
          DELETE components_infos WHERE component = ls_component_info-component AND component_name = ls_component_info-component_name.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lines( components_infos ) EQ 0.
      WRITE: 'Nothing selected '.
      nothing_selected  = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD _set_default_language.

    " Set default language

    DATA famix_custom_source_language TYPE REF TO cl_famix_custom_source_lng.
    CREATE OBJECT famix_custom_source_language EXPORTING model = model.

    famix_custom_source_language->add( name = 'ABAP' ).

    " Do not assign any entities to ABAP, because otherwise this will not be the default language anymore
    " So do not do this for ABAP, but maybe for another language
    " famix_package->set_declared_source_language( EXPORTING source_language_element = 'FAMIX.CustomSourceLanguage'
    "                                                        source_language_name    = 'ABAP' ).

  ENDMETHOD.
ENDCLASS.

CLASS CL_EP_ANALYZE_OTHER_KEYWRD IMPLEMENTATION.
  METHOD analyze.
    ASSERT statement-type EQ 'K'.
    CLEAR g_info.

    " First Run, what is the keyword
    FIELD-SYMBOLS <token> LIKE LINE OF g_sorted_tokens.
    READ TABLE g_sorted_tokens ASSIGNING <token> WITH TABLE KEY index = statement-from.
    IF sy-subrc <> 0. "OK
      " TBD Error handling
      " In the moment ignore
      RETURN.
    ENDIF.

    CASE <token>-str.
      WHEN 'CLASS'.
        g_info-is_class_stmnt_info = abap_true.

      WHEN 'ENDCLASS'.
        g_info-statement_type = end_class.
      WHEN 'PUBLIC'.
        g_info-statement_type = start_public.
      WHEN 'PROTECTED'.
        g_info-statement_type = start_protected.
      WHEN 'PRIVATE'.
        g_info-statement_type = start_private.
      WHEN 'METHODS'.
        " info-is_method_stmnt = abap_true.
        g_info-statement_type = method_definition.
      WHEN 'CLASS-METHODS'.
        g_info-statement_type = method_definition.
        g_info-is_static = abap_true.
      WHEN 'METHOD'.
        g_info-statement_type = start_method_implementation.
      WHEN 'ENDMETHOD'.
        g_info-statement_type = end_method_implementation.

      WHEN 'DATA'.
        g_info-statement_type = attribute_definition.
      WHEN 'CLASS-DATA'.
        g_info-statement_type = attribute_definition.
        g_info-is_static = abap_true.
      WHEN OTHERS.
        " TBD
        " Add further, in the moment ignore
        RETURN.
    ENDCASE.

    " Second Run, what is the name
    IF g_info-is_class_stmnt_info EQ abap_true
    OR g_info-statement_type EQ method_definition
    OR g_info-statement_type EQ start_method_implementation
    OR g_info-statement_type EQ attribute_definition.

      DATA position_of_name TYPE i.
      position_of_name =  statement-from + 1.
      READ TABLE g_sorted_tokens ASSIGNING <token> WITH TABLE KEY index = position_of_name.
      IF sy-subrc <> 0. "OK
        " TBD Error handling
        " In the moment ignore
        RETURN.
      ENDIF.

      g_info-name = <token>-str.

      " Third run, further keywords
      IF g_info-is_class_stmnt_info EQ abap_true.
        LOOP AT g_sorted_tokens ASSIGNING <token> WHERE index > position_of_name
                                                       AND index <= statement-to.
          CASE <token>-str.
            WHEN 'DEFINITION'.
              g_info-statement_type = start_class_definition.
            WHEN 'IMPLEMENTATION'.
              g_info-statement_type = start_class_implementation.
            WHEN 'INHERITING'.
              g_info-class_is_inheriting = abap_true.
              DATA superclass_is_at TYPE i.
              superclass_is_at  = sy-tabix + 2.
              FIELD-SYMBOLS <ls_superclass_token> LIKE LINE OF g_sorted_tokens.
              READ TABLE g_sorted_tokens ASSIGNING <ls_superclass_token> WITH TABLE KEY index = superclass_is_at.
              IF sy-subrc EQ 0. "OK
                g_info-class_inherits_from = <ls_superclass_token>-str.
              ELSE.
                " TBD Error handling
                " In the moment ignore
                RETURN.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    g_sorted_tokens = sorted_tokens.

  ENDMETHOD.
ENDCLASS.

CLASS CL_PROGRAM_ANALYZER IMPLEMENTATION.
  METHOD extract.
    DATA source TYPE TABLE OF string.
    READ REPORT program INTO source.

    DATA: tokens TYPE STANDARD TABLE OF stokes.

    DATA: sorted_tokens TYPE cl_ep_analyze_other_keywrd=>sorted_tokens_type.

    DATA statements TYPE STANDARD TABLE OF sstmnt.

    SCAN ABAP-SOURCE source TOKENS INTO tokens STATEMENTS INTO statements.

    FIELD-SYMBOLS <ls_token_2> LIKE LINE OF tokens.
    LOOP AT tokens ASSIGNING <ls_token_2>.
      DATA ls_token LIKE LINE OF sorted_tokens. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
      CLEAR ls_token.
      ls_token-index = sy-tabix.
      ls_token-str   = <ls_token_2>-str.
      ls_token-row   = <ls_token_2>-row.
      ls_token-col   = <ls_token_2>-col.
      ls_token-type  = <ls_token_2>-type.
      INSERT ls_token INTO TABLE sorted_tokens.
    ENDLOOP.

    SORT statements BY from.

    IF g_parameter_list_tokens EQ abap_true.
      WRITE: /.
      WRITE: / program.

    ENDIF.

    TYPES section_type TYPE c LENGTH 1.

    CONSTANTS: "! Is in public section
      public    TYPE section_type VALUE '1',
      "! Is in protected section
      protected TYPE section_type VALUE '2',
      "! Is in private section
      private   TYPE section_type VALUE '3',
      "! Not in a section
      none      TYPE section_type VALUE ' '.

    TYPES: BEGIN OF codecontext_type,
             in_section               TYPE section_type,
             in_class_definition      TYPE abap_bool,
             implementation_of_class  TYPE string,
             implementation_of_method TYPE string,
           END OF codecontext_type.

    "! Context of statement in the code
    DATA context TYPE codecontext_type.

    TYPES: BEGIN OF class_with_model_id_type,
             classname   TYPE string,
             id_in_model TYPE i,
           END OF class_with_model_id_type.

    DATA: classes_with_model_id      TYPE HASHED TABLE OF class_with_model_id_type WITH UNIQUE KEY classname,
          actual_class_with_model_id TYPE class_with_model_id_type.

    TYPES: BEGIN OF method_type,
             classname          TYPE string,
             class_id_in_model  TYPE i,
             methodname         TYPE string,
             method_id_in_model TYPE i,
             in_section         TYPE section_type,
             instanciable       TYPE abap_bool,
           END OF method_type.

    DATA: methods       TYPE STANDARD TABLE OF method_type,
          actual_method TYPE method_type.

    TYPES: BEGIN OF inheritance_type,
             subclass   TYPE string,
             superclass TYPE string,
           END OF inheritance_type.

    DATA: inheritances TYPE STANDARD TABLE OF inheritance_type.

    DATA token_number TYPE i.

    "! Instance that analyzes other ABAP Keywords
    DATA aok TYPE REF TO cl_ep_analyze_other_keywrd. " So that ABAP Doc Comment is possible
    CREATE OBJECT aok EXPORTING sorted_tokens = sorted_tokens.

    FIELD-SYMBOLS <statement> LIKE LINE OF statements.
    LOOP AT statements ASSIGNING <statement>.

      token_number = 0.
      CASE <statement>-type.
        WHEN 'K'.

          aok->analyze( statement = <statement> ).
          CASE aok->g_info-statement_type.
            WHEN aok->start_class_definition.
              " SAP_2_FAMIX_28        Determine local classes in programs
              context-in_class_definition = abap_true.
              actual_class_with_model_id-classname = aok->g_info-name.
              INSERT actual_class_with_model_id INTO TABLE classes_with_model_id.
              IF aok->g_info-class_is_inheriting EQ abap_true.
                " SAP_2_FAMIX_37        Determine local inheritances of classes
                DATA ls_inheritance_2 LIKE LINE OF inheritances. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
                CLEAR ls_inheritance_2.
                ls_inheritance_2-subclass = actual_class_with_model_id-classname.
                ls_inheritance_2-superclass = aok->g_info-class_inherits_from.
                INSERT ls_inheritance_2 INTO TABLE inheritances.
              ENDIF.
            WHEN aok->start_public.
              context-in_section = public.
            WHEN aok->start_protected.
              context-in_section = protected.
            WHEN aok->start_private.
              context-in_section = private.
            WHEN aok->end_class.
              context-in_section = none.
              context-in_class_definition = abap_false.
              CLEAR context-implementation_of_class.
            WHEN aok->method_definition.
              " SAP_2_FAMIX_29      Determine local class methods in programs
              IF aok->g_info-is_static EQ abap_true.
                CLEAR actual_method.
                actual_method-classname = actual_class_with_model_id-classname.
                actual_method-in_section = context-in_section.
              ELSE.
                CLEAR actual_method.
                actual_method-classname = actual_class_with_model_id-classname.
                actual_method-in_section = context-in_section.
                actual_method-instanciable = abap_true.
              ENDIF.
              actual_method-methodname = aok->g_info-name.
            WHEN aok->start_class_implementation.
              context-implementation_of_class = aok->g_info-name.
            WHEN aok->start_method_implementation.
              context-implementation_of_method = aok->g_info-name.
              IF g_parameter_list_tokens EQ abap_true.
                FORMAT COLOR COL_GROUP.
              ENDIF.
            WHEN aok->end_method_implementation.
              CLEAR context-implementation_of_method.
              IF g_parameter_list_tokens EQ abap_true.
                FORMAT COLOR COL_BACKGROUND.
              ENDIF.
            WHEN OTHERS.

          ENDCASE.

        WHEN OTHERS.

      ENDCASE.

      IF g_parameter_list_tokens EQ abap_true.
        WRITE: / <statement>-type.
        FIELD-SYMBOLS <token> LIKE LINE OF sorted_tokens.
        LOOP AT sorted_tokens ASSIGNING <token> WHERE
            index >= <statement>-from
        AND index <= <statement>-to.
          WRITE: '|', <token>-type, <token>-str.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    " Add local classes to model

    DATA sap_class TYPE REF TO cl_sap_class.
    CREATE OBJECT sap_class EXPORTING model = model.

    FIELD-SYMBOLS <class> LIKE LINE OF classes_with_model_id.
    LOOP AT classes_with_model_id ASSIGNING <class>.
      " SAP_2_FAMIX_30        Map local classes of programs to FAMIX.Class
      " SAP_2_FAMIX_61      Mark the FAMIX Class with the attribute modifiers = 'ABAPLocalClass'
      <class>-id_in_model = sap_class->add_local( program   = program
                                                  name      = <class>-classname
                                                  modifiers = modifiers_abaplocalclass ).

      sap_class->set_parent_program( element_id = <class>-id_in_model
                                     sap_program = program ).


    ENDLOOP.

    " Add local methods to model

    DATA sap_method TYPE REF TO cl_sap_method.
    CREATE OBJECT sap_method EXPORTING model = model.

    FIELD-SYMBOLS <method> LIKE LINE OF methods.
    LOOP AT methods ASSIGNING <method>.
      FIELD-SYMBOLS <class_2> LIKE LINE OF classes_with_model_id.
      READ TABLE classes_with_model_id ASSIGNING <class_2> WITH TABLE KEY classname = <method>-classname.
      <method>-class_id_in_model = <class_2>-id_in_model.

      <method>-method_id_in_model = sap_method->add_local_method( class_name  = <class_2>-classname
                                                                  class_id    = <class_2>-id_in_model
                                                                  method_name = <method>-methodname ).

*      " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method
*
*      <method>-method_id_in_model = famix_method->add( EXPORTING name_group = <class_2>-classname " Why classname in name_group?
*                                                                 name       = <method>-methodname ).
*      " SAP_2_FAMIX_43        Fill the attribute signature of FAMIX.METHOD with the name of the method
*      famix_method->set_signature( signature = <method>-methodname ).
*
*      " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class
*
*
*      famix_method->set_parent_type( EXPORTING parent_element = 'FAMIX.Class'
*                                               parent_id      =  <class_2>-id_in_model ).
    ENDLOOP.

    " Add local inheritances to model

    DATA sap_inheritance TYPE REF TO cl_sap_inheritance.
    CREATE OBJECT sap_inheritance EXPORTING model = model.
    DATA inheritance LIKE LINE OF inheritances.
    LOOP AT inheritances INTO inheritance.
      DATA last_used_id TYPE i.
      last_used_id = sap_inheritance->add( ).
      sap_inheritance->set_local_sub_and_super_class( EXPORTING element_id = last_used_id
                                                                program = program
                                                                subclass_name   = inheritance-subclass
                                                                superclass_name = inheritance-superclass ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: mse_model TYPE cl_model=>lines_type.
*"! Extract from SAP
  DATA g_parameter_extract_from_sap TYPE abap_bool.
  g_parameter_extract_from_sap = p_sap.
  IF g_parameter_extract_from_sap EQ abap_false.
    cl_make_demo_model=>make( IMPORTING mse_model = mse_model ).
  ELSE.
    DATA sap_extractor TYPE REF TO cl_extract_sap.

    DATA: ls_compson_line LIKE LINE OF s_compsn.
    DATA: ls_compsn TYPE sap_extractor->ty_s_compsn.
    LOOP AT s_compsn INTO ls_compson_line.
      APPEND ls_compson_line TO ls_compsn.
    ENDLOOP.

    DATA: ls_pack_line LIKE LINE OF s_pack.
    DATA: ls_pack TYPE sap_extractor->ty_s_pack.
    LOOP AT s_pack INTO ls_pack_line.
      APPEND ls_pack_line TO ls_pack.
    ENDLOOP.

    CREATE OBJECT sap_extractor
      EXPORTING
        i_g_filter_using_package       = g_filter_using_package
        i_g_filter_using_name          = g_filter_using_name
        i_g_parameter_package_to_analz = g_parameter_package_to_analyze
        i_p_iprog                      = p_iprog
        i_p_clas                       = p_clas
        i_p_wdyn                       = p_wdyn
        i_p_intf                       = p_intf
        i_p_prog                       = p_prog
        i_p_tables                     = p_tables
        i_s_compsn                     = ls_compsn
        i_s_pack                       = ls_pack
        i_g_param_usage_outpack_groupd = g_param_usage_outpack_groupd.

    DATA nothing_done TYPE boolean.
    sap_extractor->extract( IMPORTING mse_model    = mse_model
                                      nothing_done = nothing_done ).
  ENDIF.

  IF nothing_done EQ abap_true.
    RETURN.
  ENDIF.

  DATA model_outputer TYPE REF TO cl_output_model.
  CREATE OBJECT model_outputer.
  model_outputer->make( mse_model = mse_model g_parameter_download_file = p_down ).
