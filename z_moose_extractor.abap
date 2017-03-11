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
"! Generated 11.03.2017
"! Contains commit a82e81c9285a353cf0c2a8661808e99ad08ca653
"!
"! This is version 0.2.0. It will be much better covered with unit tests and end-to-end tests than the first version. It is currently incomplete.
"! Includes fix for #44 ignore SAP interfaces in Where-Used
"! Select where-used now not only for a single table
"! Fix syntax error for ABAP 7.02
"!
REPORT z2mse_moose_extractor2.
TABLES tadir. "So that select-options work

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE TEXT-001.


SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE TEXT-002.

*PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_wdyn AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_intf AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_tables AS CHECKBOX DEFAULT 'X'. "Analyze database tables
*PARAMETERS: p_prog AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_iprog AS CHECKBOX DEFAULT ' '. "Internal parts of reports
*
*
*PARAMETERS: rb_fpack RADIOBUTTON GROUP rbsl DEFAULT 'X'.
*"! Filter using package
*DATA g_filter_using_package TYPE abap_bool.
*g_filter_using_package = rb_fpack.
*
*PARAMETERS: p_pack TYPE parentcl DEFAULT ''.
*"! Package to be analyzed
*DATA g_parameter_package_to_analyze TYPE parentcl.
*g_parameter_package_to_analyze = p_pack.
*
*PARAMETERS: rb_fname RADIOBUTTON GROUP rbsl.
*"! Filter using name
*DATA g_filter_using_name TYPE abap_bool.
*g_filter_using_name = rb_fname.

SELECT-OPTIONS s_pack FOR tadir-devclass.
SELECT-OPTIONS s_spack FOR tadir-devclass.
PARAMETERS p_sub AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_up TYPE i DEFAULT -1.
"Exclude interfaces in sap name space when found via where used analysis
PARAMETERS p_ex AS CHECKBOX DEFAULT 'X'.

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
    TYPES: lines_type TYPE STANDARD TABLE OF line_type WITH DEFAULT KEY.

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
      INSERT ls_attribute INTO TABLE g_attributes.
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
      INSERT ls_attribute INTO TABLE g_attributes.
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
      INSERT ls_attribute INTO TABLE g_attributes.
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
        assert ( <attribute>-value_type eq string_value ) or ( <attribute>-value_type eq reference_value ) or ( <attribute>-value_type eq boolean_value ).
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


CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM CL_famix_entity
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



CLASS cl_famix_behavioural_entty DEFINITION INHERITING FROM CL_famix_container_entity ABSTRACT
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



CLASS cl_famix_method DEFINITION INHERITING FROM CL_famix_behavioural_entty
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
      IMPORTING class_name_group  TYPE clike OPTIONAL
                class             TYPE clike
                method_name_group TYPE clike OPTIONAL
                method            TYPE clike.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class_name_group | the name group of the class of the method
    "! @parameter class | the class of the method
    "! @parameter method_name_group | the name group of the method name
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING class_name_group  TYPE clike OPTIONAL
                class             TYPE clike
                method_name_group TYPE clike OPTIONAL
                method            TYPE clike
      RETURNING VALUE(id)         TYPE i.

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



CLASS cl_famix_access DEFINITION INHERITING FROM CL_famix_association
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



CLASS cl_famix_invocation DEFINITION INHERITING FROM CL_famix_association
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



CLASS cl_famix_inheritance DEFINITION INHERITING FROM CL_famix_association
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

" Obsolete:



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

******************************************** End Include Z_FAMIX_ABAP *****************************

"! Selects packages in case objects to be analyzed are selected by packages.
"! Will be tested by transferring test data to the constructor.
"! Has Unit Tests.
CLASS cl_extr_packages DEFINITION
.

  PUBLIC SECTION.

    TYPES:
      ty_s_pack                         TYPE RANGE OF tadir-devclass.

    TYPES: BEGIN OF ty_package,
             package       TYPE devclass,
             parentpackage TYPE parentcl,
           END OF ty_package.
    TYPES ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    TYPES:
      BEGIN OF ty_tdevc_test,
        devclass TYPE devclass,
        parentcl TYPE parentcl,
      END OF ty_tdevc_test.
    TYPES ty_t_tdevc_test TYPE HASHED TABLE OF ty_tdevc_test WITH UNIQUE KEY devclass.

    DATA g_selected_packages TYPE ty_packages READ-ONLY.

    "! @parameter tdevc_test | provide test data for table TDEVC during unit tests.
    METHODS constructor
      IMPORTING
        !tdevc_test TYPE ty_t_tdevc_test OPTIONAL.

    "! Select packages according to filter transfered by report
    "! @parameter top_packages | Select packages
    "! @parameter sub_packages_filter | Optional: Include sub packages only if they are filtered by this filter
    "! @parameter including_sub_packages | Default false: Search sub packages
    METHODS select_packages
      IMPORTING
        !top_packages           TYPE ty_s_pack
        !sub_packages_filter    TYPE ty_s_pack OPTIONAL
        !including_sub_packages TYPE abap_bool DEFAULT abap_false.


    "! Add all selected components to the model. Should be called only once
    METHODS add_selected_packages_to_model
      IMPORTING
        famix_package TYPE REF TO cl_famix_package.

PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_package_store,
             package           TYPE devclass,
             parentpackage     TYPE parentcl,
             subclass_searched TYPE abap_bool,
             is_to_be_returned TYPE abap_bool,
           END OF ty_package_store.
    TYPES ty_packages_store TYPE HASHED TABLE OF ty_package_store WITH UNIQUE KEY package.
    "! Filled during tests
    DATA g_tdevc_test TYPE ty_t_tdevc_test.
    DATA g_is_test TYPE abap_bool.
    METHODS _select_top_packages
      IMPORTING
        i_top_packages    TYPE cl_extr_packages=>ty_s_pack
      RETURNING
        VALUE(r_packages) TYPE cl_extr_packages=>ty_packages.
    TYPES:
      ty_packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    METHODS _select_sub_packages
      IMPORTING
        i_packages_to_search_sub TYPE ty_packages_to_search_sub
      RETURNING
        VALUE(r_packages)        TYPE cl_extr_packages=>ty_packages.
ENDCLASS.

"! Extract informations on ABAP classes
"! Will be tested by transferring test data to the constructor.
"! Has Unit Tests.
CLASS cl_extr_classes DEFINITION
  FINAL
.
  PUBLIC SECTION.
    "! use as replacement for ty_component_long
    "! Required because in case of interfaces the name of the interface is together with a ~ part of the name
    TYPES ty_component_long TYPE c LENGTH 61.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.
    TYPES: BEGIN OF ty_seoclass_test,
             clsname TYPE seoclsname,
           END OF ty_seoclass_test.
    TYPES ty_t_seoclass_test TYPE HASHED TABLE OF ty_seoclass_test WITH UNIQUE KEY clsname.

    TYPES: BEGIN OF ty_class,
             clsname  TYPE seoclsname,
             clstype  TYPE seoclstype,
             devclass TYPE tadir-devclass,
             exists   TYPE abap_bool,
           END OF ty_class.
    TYPES: ty_classes TYPE HASHED TABLE OF ty_class WITH UNIQUE KEY clsname.

    TYPES: BEGIN OF ty_seocompo_test,
             clsname TYPE seoclsname,
             cmpname TYPE seocmpname,
             cmptype TYPE seocmptype,
           END OF ty_seocompo_test.
    TYPES ty_t_seocompo_test TYPE HASHED TABLE OF ty_seocompo_test WITH UNIQUE KEY clsname cmpname.

    TYPES: BEGIN OF ty_seometarel_test,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
             version    TYPE seoversion,
             state      TYPE seostate,
             reltype    TYPE seoreltype,
           END OF ty_seometarel_test.
    TYPES ty_t_seometarel_test TYPE HASHED TABLE OF ty_seometarel_test WITH UNIQUE KEY clsname refclsname version.

    TYPES: BEGIN OF ty_class_component,
             clsname TYPE seoclsname,
             cmpname TYPE ty_component_long,
             cmptype TYPE seocmptype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE SORTED TABLE OF ty_class_component WITH UNIQUE KEY clsname cmpname cmptype.
    TYPES ty_class_components_hashed TYPE HASHED TABLE OF ty_class_component WITH UNIQUE KEY clsname cmpname cmptype.

    CONSTANTS: class_type     TYPE seoclstype VALUE 0,
               interface_type TYPE seoclstype VALUE 1,
               attribute_type TYPE seocmptype VALUE 0,
               method_type    TYPE seocmptype VALUE 1,
               event_type     TYPE seocmptype VALUE 2,
               tadir_clas     TYPE tadir-object VALUE 'CLAS' ##NO_TEXT,
               tadir_intf     TYPE tadir-object VALUE 'INTF' ##NO_TEXT.
    METHODS constructor
      IMPORTING
        !tadir_test              TYPE ty_t_tadir_test OPTIONAL
        seoclass_test            TYPE ty_t_seoclass_test OPTIONAL
        seocompo_test            TYPE ty_t_seocompo_test OPTIONAL
        seometarel_test          TYPE ty_t_seometarel_test OPTIONAL
        i_exclude_found_sap_intf TYPE abap_bool.
    "! Call once to select all classes that are in a list of packages
    METHODS select_classes_by_packages
      IMPORTING
        packages TYPE cl_extr_packages=>ty_packages.
    "! Add classes by a list of components
    METHODS select_classes_by_components
      IMPORTING
        components TYPE ty_class_components_hashed.

    "! Add all selected components to the model. Should be called only once
    METHODS add_to_model
      IMPORTING
        famix_package    TYPE REF TO cl_famix_package
        famix_class      TYPE REF TO cl_famix_class
        famix_method     TYPE REF TO cl_famix_method
        famix_attribute  TYPE REF TO cl_famix_attribute
        famix_invocation TYPE REF TO cl_famix_invocation
        famix_access     TYPE REF TO cl_famix_access.
    "! Returns components. Returns these Components only once
    METHODS get_comp_to_do_where_used
      RETURNING VALUE(components) TYPE cl_extr_classes=>ty_class_components.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_tadir_obj_name,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir_obj_name.

    TYPES: ty_tadir_obj_names TYPE HASHED TABLE OF ty_tadir_obj_name WITH UNIQUE KEY obj_name.

    "! Filled during tests
    DATA g_tadir_test TYPE cl_extr_classes=>ty_t_tadir_test.
    "! Filled during tests
    DATA g_seoclass_test TYPE cl_extr_classes=>ty_t_seoclass_test.
    "! Filled during tests
    DATA g_seocompo_test TYPE cl_extr_classes=>ty_t_seocompo_test.
    "! Filled during tests
    DATA g_seometarel_test TYPE cl_extr_classes=>ty_t_seometarel_test.
    "! A list of all primarily selected and existing classes or interfaces
    DATA g_selected_classes TYPE cl_extr_classes=>ty_classes.
    "! A list of all components of primarily selected and existing classes or interfaces. Only if not yet transfered to where used analysis
    DATA g_selected_components_new TYPE cl_extr_classes=>ty_class_components_hashed.
    "! A list of all components of primarily selected and existing classes or interfaces
    DATA g_selected_components TYPE cl_extr_classes=>ty_class_components_hashed.
    DATA g_is_test TYPE abap_bool.
    " Exclude found interfaces in SAP namespace in the where-used analysis
    DATA g_exclude_found_sap_intf TYPE abap_bool.
    "! Checks whether a class exists. There can be TADIR entries for not existing classes.
    METHODS _check_existence_and_add_intf
      CHANGING classes TYPE cl_extr_classes=>ty_classes.
    METHODS _read_class_details
      IMPORTING classes           TYPE cl_extr_classes=>ty_classes
      RETURNING VALUE(components) TYPE cl_extr_classes=>ty_class_components.
    METHODS _select_from_tadir
      IMPORTING
                i_packages              TYPE cl_extr_packages=>ty_packages
      RETURNING VALUE(selected_classes) TYPE cl_extr_classes=>ty_classes.
    METHODS _select_from_tadir_by_comp
      IMPORTING
                obj_names               TYPE ty_tadir_obj_names
      RETURNING VALUE(selected_classes) TYPE cl_extr_classes=>ty_classes.

    METHODS _add_classes_to_model
      IMPORTING
        famix_package    TYPE REF TO cl_famix_package
        famix_class      TYPE REF TO cl_famix_class
        famix_method     TYPE REF TO cl_famix_method
        famix_attribute  TYPE REF TO cl_famix_attribute
        famix_invocation TYPE REF TO cl_famix_invocation
        famix_access     TYPE REF TO cl_famix_access
        classes          TYPE cl_extr_classes=>ty_classes
        components       TYPE cl_extr_classes=>ty_class_components_hashed.
    METHODS add_and_sort_to_classes_table
      IMPORTING
        i_tadirvalues      TYPE cl_extr_classes=>ty_t_tadir_test
      CHANGING
        c_selected_classes TYPE cl_extr_classes=>ty_classes.
    "! Exclude certain elements where no where used analysis shall be made
    METHODS _exclude_classes_from_where_us
      CHANGING
        c_components TYPE cl_extr_classes=>ty_class_components.
ENDCLASS.

"! Extract informations on SAP database tables
CLASS cl_extr_tables DEFINITION
  FINAL
.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test .
    TYPES:
      ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name .
    TYPES:
      BEGIN OF ty_dd02l_test,
        tabname TYPE tabname,
      END OF ty_dd02l_test .
    TYPES:
      ty_t_dd02l_test TYPE HASHED TABLE OF ty_dd02l_test WITH UNIQUE KEY tabname .
    TYPES:
      BEGIN OF ty_table_public,
        tabname TYPE tabname,
      END OF ty_table_public .
    TYPES:
      ty_tables_public TYPE HASHED TABLE OF ty_table_public WITH UNIQUE KEY tabname .

    METHODS constructor
      IMPORTING
        !tadir_test TYPE ty_t_tadir_test OPTIONAL
        !dd02l_test TYPE ty_t_dd02l_test OPTIONAL .
    "! Call once to select all tables that are in a list of packages
    METHODS select_tables_by_packages
      IMPORTING
        !packages TYPE cl_extr_packages=>ty_packages .
    "! Returns tables. Returns these tables only once
    METHODS get_tables_to_do_where_used
      RETURNING
        VALUE(tables) TYPE ty_tables_public .
    METHODS add_to_model
      IMPORTING
        !famix_package   TYPE REF TO cl_famix_package
        !famix_class     TYPE REF TO cl_famix_class
        !famix_attribute TYPE REF TO cl_famix_attribute .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_table,
             tabname  TYPE tabname,
             devclass TYPE tadir-devclass,
             exists   TYPE abap_bool,
           END OF ty_table.
    TYPES: ty_tables TYPE HASHED TABLE OF ty_table WITH UNIQUE KEY tabname.
    CONSTANTS: tadir_table TYPE tadir-object VALUE 'TABL' ##NO_TEXT.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_tadir_test TYPE ty_t_tadir_test.
    "! Filled during tests
    DATA g_dd02l_test TYPE ty_t_dd02l_test.
    "! A list of all primarily selected and existing tables. Only if not yet transfered to where used analysis
    DATA g_selected_tables_new TYPE cl_extr_tables=>ty_tables.
    "! A list of all primarily selected and existing tables
    DATA g_selected_tables TYPE cl_extr_tables=>ty_tables.
    METHODS _select_from_tadir
      IMPORTING
        i_packages             TYPE cl_extr_packages=>ty_packages
      RETURNING
        VALUE(selected_tables) TYPE cl_extr_tables=>ty_tables.
    METHODS add_and_sort_to_tables_table
      IMPORTING
        i_tadirvalues     TYPE cl_extr_tables=>ty_t_tadir_test
      CHANGING
        c_selected_tables TYPE cl_extr_tables=>ty_tables.
    "! Checks whether a class exists. There can be TADIR entries for not existing classes.
    METHODS _check_existence
      CHANGING db_tables TYPE cl_extr_tables=>ty_tables.

ENDCLASS.

CLASS cl_extr_web_dynpro DEFINITION
.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test .
    TYPES:
      ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.

    METHODS constructor
      IMPORTING
        !tadir_test TYPE ty_t_tadir_test OPTIONAL.


    TYPES: BEGIN OF ty_web_dynpro_component,
             component_name  TYPE wdy_component_name,
             controller_name TYPE wdy_controller_name,
           END OF ty_web_dynpro_component.
    TYPES ty_web_dynpro_components TYPE SORTED TABLE OF ty_web_dynpro_component WITH UNIQUE KEY component_name controller_name.
    TYPES ty_web_dynpro_components_hash TYPE HASHED TABLE OF ty_web_dynpro_component WITH UNIQUE KEY component_name controller_name.

    "! Add Web Dynpro Components by a list of Web Dynpro Components
    METHODS select_classes_by_components
      IMPORTING
        components TYPE ty_web_dynpro_components_hash.

    "! Add all selected components to the model. Should be called only once
    METHODS add_to_model
      IMPORTING
        famix_class  TYPE REF TO cl_famix_class
        famix_method TYPE REF TO cl_famix_method.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_tadir_test TYPE ty_t_tadir_test.
    "! A list of all components of primarily selected and existing classes or interfaces. Only if not yet transfered to where used analysis
    DATA g_selctd_web_dynpro_compts TYPE ty_web_dynpro_components_hash.
ENDCLASS.

CLASS cl_extr_where_used DEFINITION
.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_wbcrossgt_test,
        otype    TYPE char2,
        name     TYPE eu_lname,
        include  TYPE programm,
        direct   TYPE sgrade,
        indirect TYPE sgrade,
      END OF ty_wbcrossgt_test ,
      ty_t_wbcrossgt_test TYPE SORTED TABLE OF ty_wbcrossgt_test WITH UNIQUE KEY otype name include.

    TYPES:
      BEGIN OF ty_include_to_component,
        include            TYPE programm,
        is_class_component TYPE abap_bool,
        clsname            TYPE seoclsname,
        clstype            TYPE seoclstype,
        cmpname            TYPE seocmpname,
        cmptype            TYPE seocmptype,
        is_webdynpro       TYPE abap_bool,
        component_name     TYPE wdy_component_name,
        controller_name    TYPE wdy_controller_name,
      END OF ty_include_to_component .
    TYPES:
      ty_includes_to_components TYPE HASHED TABLE OF ty_include_to_component WITH UNIQUE KEY include .
    METHODS constructor
      IMPORTING
        wbcrossgt_test           TYPE ty_t_wbcrossgt_test
        includes_to_components   TYPE ty_includes_to_components.
    "! Returns all components that are found in the last where-used analysis. Returns this components only once
    METHODS get_components_where_used
      EXPORTING
        VALUE(components)            TYPE cl_extr_classes=>ty_class_components_hashed
        VALUE(web_dynpro_components) TYPE cl_extr_web_dynpro=>ty_web_dynpro_components_hash.
  PROTECTED SECTION.
    "! Filled during tests
    DATA g_wbcrossgt_test TYPE ty_t_wbcrossgt_test.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_includes_to_components_test TYPE ty_includes_to_components.
    "! All class components that where found during where used
    DATA g_class_components_where_used TYPE cl_extr_classes=>ty_class_components_hashed.
    "! All Web Dynpro ABAP components that where found during where used
    DATA g_web_dynpro_cmpnts_where_used TYPE cl_extr_web_dynpro=>ty_web_dynpro_components_hash.

    TYPES: BEGIN OF ty_where_used_name,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
           END OF ty_where_used_name.
    TYPES: ty_where_used_names TYPE HASHED TABLE OF ty_where_used_name WITH UNIQUE KEY otype where_used_name.

    METHODS _select_where_used_table
      IMPORTING
        i_names_to_components    TYPE cl_extr_where_used=>ty_where_used_names
      RETURNING
        VALUE(r_found_wbcrossgt) TYPE ty_t_wbcrossgt_test.
    METHODS _determine_mapping_include_to
      IMPORTING
        i_found_wbcrossgt              TYPE ty_t_wbcrossgt_test
      RETURNING
        VALUE(r_includes_2_components) TYPE ty_includes_to_components.

  PRIVATE SECTION.
ENDCLASS.

CLASS cl_extr_where_used_classes DEFINITION
  INHERITING FROM cl_extr_where_used
  FINAL
.

  PUBLIC SECTION.


    TYPES:
      BEGIN OF ty_comp_used_by_element,
        clsname            TYPE seoclsname,
        cmpname            TYPE seocmpname,
        cmptype            TYPE seocmptype,
        is_class_component TYPE abap_bool,
        used_by_clsname    TYPE seoclsname,
        used_by_cmpname    TYPE seocmpname,
        used_by_cmptype    TYPE seocmptype,
        is_webdynpro       TYPE abap_bool,
        component_name     TYPE wdy_component_name,
        controller_name    TYPE wdy_controller_name,
      END OF ty_comp_used_by_element .
    TYPES:
      ty_comps_used_by_elements TYPE STANDARD TABLE OF ty_comp_used_by_element WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !wbcrossgt_test         TYPE ty_t_wbcrossgt_test OPTIONAL
        !includes_to_components TYPE ty_includes_to_components OPTIONAL.
    "! Receives a list of components. Does a where-used analysis. Stores components that are using the received components internally
    METHODS used_by_class_component
      IMPORTING
        !class_components TYPE cl_extr_classes=>ty_class_components .

    "! Add all selected components to the model. Should be called only once
    METHODS add_usage_to_model
      IMPORTING
        !famix_method     TYPE REF TO cl_famix_method
        !famix_attribute  TYPE REF TO cl_famix_attribute
        !famix_invocation TYPE REF TO cl_famix_invocation
        !famix_access     TYPE REF TO cl_famix_access.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_name_to_component,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
             clsname         TYPE seoclsname,
             cmpname         TYPE seocmpname,
             cmptype         TYPE seocmptype,
           END OF ty_name_to_component.
    TYPES: ty_names_to_components TYPE HASHED TABLE OF ty_name_to_component WITH UNIQUE KEY otype where_used_name.



    "! all class components from initial search
    DATA g_class_components_initial TYPE cl_extr_classes=>ty_class_components_hashed.

    DATA g_comps_used_by_comps TYPE ty_comps_used_by_elements.
    METHODS _fill_comps_used_by_elements
      IMPORTING
        i_names_to_components   TYPE ty_names_to_components
        i_found_wbcrossgt       TYPE ty_t_wbcrossgt_test
        i_includes_2_components TYPE ty_includes_to_components.


    METHODS _get_mapping_to_name
      IMPORTING
        i_class_components           TYPE cl_extr_classes=>ty_class_components
      RETURNING
        VALUE(r_names_to_components) TYPE ty_names_to_components.
ENDCLASS.

"! Extract elements that use certain tables
CLASS cl_extr_where_used_tables DEFINITION
  INHERITING FROM cl_extr_where_used
.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !wbcrossgt_test         TYPE ty_t_wbcrossgt_test OPTIONAL
        !includes_to_components TYPE ty_includes_to_components OPTIONAL.
    "! Receives a list of tables. Does a where-used analysis. Stores elements that are using the received elements internally
    METHODS used_by_table
      IMPORTING
        tables TYPE cl_extr_tables=>ty_tables_public.


    TYPES:
      BEGIN OF ty_table_used_by_element,
        table              TYPE tabname,
        is_class_component TYPE abap_bool,
        used_by_clsname    TYPE seoclsname,
        used_by_cmpname    TYPE seocmpname,
        used_by_cmptype    TYPE seocmptype,
        is_webdynpro       TYPE abap_bool,
        component_name     TYPE wdy_component_name,
        controller_name    TYPE wdy_controller_name,
      END OF ty_table_used_by_element .
    TYPES:
      ty_tables_used_by_elements TYPE STANDARD TABLE OF ty_table_used_by_element WITH DEFAULT KEY.

    "! Add all selected components to the model. Should be called only once
    METHODS add_usage_to_model
      IMPORTING
        !famix_method     TYPE REF TO cl_famix_method
        !famix_attribute  TYPE REF TO cl_famix_attribute
        !famix_invocation TYPE REF TO cl_famix_invocation
        !famix_access     TYPE REF TO cl_famix_access.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "! all tables from initial search
    DATA g_tables_initial TYPE cl_extr_tables=>ty_tables_public.

    TYPES: BEGIN OF ty_name_to_table,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
             table           TYPE tabname,
           END OF ty_name_to_table.
    TYPES: ty_names_to_tables TYPE HASHED TABLE OF ty_name_to_table WITH UNIQUE KEY otype where_used_name.
    METHODS _get_mapping_to_name
      IMPORTING
        tables                   TYPE cl_extr_tables=>ty_tables_public
      RETURNING
        VALUE(r_names_to_tables) TYPE cl_extr_where_used_tables=>ty_names_to_tables.

    DATA g_tables_used_by_elements TYPE ty_tables_used_by_elements.
    METHODS _fill_tables_used_by_elements
      IMPORTING
        i_names_to_components   TYPE ty_names_to_tables
        i_found_wbcrossgt       TYPE ty_t_wbcrossgt_test
        i_includes_2_components TYPE ty_includes_to_components.
ENDCLASS.

CLASS cl_extract_sap2 DEFINITION
  FINAL
.

  PUBLIC SECTION.

    TYPES: ty_s_pack TYPE RANGE OF tadir-devclass .
    CONSTANTS modifier_abapglobalclass TYPE string VALUE 'ABAPGlobalClass' ##NO_TEXT.
    CONSTANTS modifier_abapglobalinterface TYPE string VALUE 'ABAPGlobalInterface' ##NO_TEXT.
    CONSTANTS modifier_webdynpro_component TYPE string VALUE 'ABAPWebDynproComponent'.
    CONSTANTS modifier_dbtable TYPE string VALUE 'DBTable' ##NO_TEXT.
    METHODS constructor .
    "! Main start to do the extraction
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    "! @parameter i_exclude_found_sap_intf | exclude found interfaces in SAP namespace in the where-used analysis
    METHODS extract
      IMPORTING
        !i_top_packages        TYPE ty_s_pack
        !i_sub_packages_filter TYPE ty_s_pack
        !i_search_sub_packages TYPE abap_bool
        i_search_up            TYPE i
        i_exclude_found_sap_intf TYPE abap_bool
      EXPORTING
        !mse_model             TYPE cl_model=>lines_type
        VALUE(nothing_done)    TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA model            TYPE REF TO cl_model.
    DATA famix_package     TYPE REF TO cl_famix_package.
    DATA famix_class     TYPE REF TO cl_famix_class.
    DATA famix_method     TYPE REF TO cl_famix_method.
    DATA famix_attribute     TYPE REF TO cl_famix_attribute.
    DATA famix_invocation     TYPE REF TO cl_famix_invocation.
    DATA famix_access     TYPE REF TO cl_famix_access.
    METHODS _initial_selections_by_filter
      IMPORTING
        i_top_packages        TYPE cl_extract_sap2=>ty_s_pack
        i_sub_packages_filter TYPE cl_extract_sap2=>ty_s_pack
        i_search_sub_packages TYPE abap_bool
        i_extract_packages    TYPE REF TO cl_extr_packages
        i_extract_classes     TYPE REF TO cl_extr_classes
        i_extract_tables      TYPE REF TO cl_extr_tables.
    "! @parameter i_search_up | how often is a upward searched in the where-used-information to be repeated. Search infinite if < 0
    METHODS _get_using_elements
      IMPORTING
        i_extract_classes            TYPE REF TO cl_extr_classes
        i_extract_tables             TYPE REF TO cl_extr_tables
        i_extract_web_dynpro         TYPE REF TO cl_extr_web_dynpro
        i_extract_where_used_classes TYPE REF TO cl_extr_where_used_classes
        i_extract_where_used_tables  TYPE REF TO cl_extr_where_used_tables
        i_search_up                  TYPE i.
    METHODS _add_all_to_model_and_make_mse
      IMPORTING
        i_extract_packages           TYPE REF TO cl_extr_packages
        i_extract_classes            TYPE REF TO cl_extr_classes
        i_extract_web_dynpro         TYPE REF TO cl_extr_web_dynpro
        i_extract_where_used_classes TYPE REF TO cl_extr_where_used_classes
        i_extract_where_used_tables  TYPE REF TO cl_extr_where_used_tables
        i_extract_tables             TYPE REF TO cl_extr_tables
      RETURNING
        VALUE(r_mse_model)           TYPE cl_model=>lines_type.
ENDCLASS.

CLASS CL_EXTR_PACKAGES IMPLEMENTATION.
  METHOD constructor.
    IF tdevc_test IS SUPPLIED.
      g_tdevc_test = tdevc_test.
      g_is_test = abap_true.
    ENDIF.
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
  METHOD add_selected_packages_to_model.

    DATA: selected_package  TYPE cl_extr_packages=>ty_package.

    LOOP AT g_selected_packages INTO selected_package.

      famix_package->add( name = selected_package-package ).

      IF selected_package-parentpackage IS NOT INITIAL.

        famix_package->add( name = selected_package-parentpackage ).
        famix_package->set_parent_package( element_id = 0
                                     element_type = 'FAMIX.Package'
                                     element_name_group = ''
                                     element_name = selected_package-package
                                     parent_package = selected_package-parentpackage ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_CLASSES IMPLEMENTATION.
  METHOD add_and_sort_to_classes_table.

    DATA tadirline TYPE cl_extr_classes=>ty_tadir_test.

    "Add and sort to classes table
    DATA class TYPE ty_class.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR class.

      class-clsname = tadirline-obj_name.
      CASE tadirline-object.
        WHEN tadir_clas.
          class-clstype = class_type.
        WHEN tadir_intf.
          class-clstype = interface_type.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      class-devclass = tadirline-devclass.

      INSERT class INTO TABLE c_selected_classes.

    ENDLOOP.

    SORT c_selected_classes BY clsname.

  ENDMETHOD.
  METHOD constructor.
    IF tadir_test IS SUPPLIED
    OR seoclass_test IS SUPPLIED
    OR seocompo_test IS SUPPLIED
    OR seometarel_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_seoclass_test = seoclass_test.
      g_seocompo_test = seocompo_test.
      g_seometarel_test = seometarel_test.
      g_is_test = abap_true.
    ENDIF.

    g_exclude_found_sap_intf = i_exclude_found_sap_intf.

  ENDMETHOD.
  METHOD get_comp_to_do_where_used.

    DATA line LIKE LINE OF g_selected_components_new.

    LOOP AT g_selected_components_new INTO line.
      READ TABLE g_selected_components TRANSPORTING NO FIELDS WITH TABLE KEY clsname = line-clsname cmpname = line-cmpname cmptype = line-cmptype.
      IF sy-subrc <> 0.
        INSERT line INTO TABLE components.
      ENDIF.
    ENDLOOP.

    _exclude_classes_from_where_us( CHANGING c_components = components ).

    LOOP AT g_selected_components_new INTO line.
      INSERT line INTO TABLE g_selected_components.
    ENDLOOP.
    CLEAR g_selected_components_new.

  ENDMETHOD.
  METHOD select_classes_by_components.

    DATA component LIKE LINE OF components.

    DATA: obj_name  TYPE ty_tadir_obj_name,
          obj_names TYPE ty_tadir_obj_names.

    LOOP AT components INTO component.
      READ TABLE g_selected_classes TRANSPORTING NO FIELDS WITH TABLE KEY clsname = component-clsname.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.
      obj_name = component-clsname.
      INSERT obj_name INTO TABLE obj_names.
    ENDLOOP.
    DATA: l_add_classes TYPE ty_classes,
          l_added_class TYPE ty_class.
    l_add_classes = me->_select_from_tadir_by_comp( obj_names = obj_names ).

    _check_existence_and_add_intf( CHANGING classes = l_add_classes ).

    LOOP AT l_add_classes INTO l_added_class.
      INSERT l_added_class INTO TABLE g_selected_classes.
    ENDLOOP.
*    g_selected_components_new = _read_class_details(  l_add_classes ).
    DATA: tmp_components TYPE ty_class_components_hashed,
          tmp_component  TYPE ty_class_component.
    tmp_components = _read_class_details(  l_add_classes ).
    LOOP AT tmp_components INTO tmp_component.
      INSERT tmp_component INTO TABLE g_selected_components_new.
    ENDLOOP.

  ENDMETHOD.
  METHOD select_classes_by_packages.

    g_selected_classes = _select_from_tadir( packages ).
    _check_existence_and_add_intf( CHANGING classes = g_selected_classes ).
    g_selected_components_new = _read_class_details(  g_selected_classes ).

  ENDMETHOD.
  METHOD _read_class_details.

    TYPES: BEGIN OF ty_interface,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
           END OF ty_interface.
    TYPES: ty_interfaces TYPE STANDARD TABLE OF ty_interface.
    DATA interface TYPE ty_interface.
    DATA interfaces TYPE ty_interfaces.

    "! The new components.
    "! In case of interfaces add methods and attributes of interface with notation
    "! interface name ~ attribute/methodname
    DATA new_comps_including_intf TYPE cl_extr_classes=>ty_class_components.
    DATA new_comp_including_intf TYPE cl_extr_classes=>ty_class_component.

    DATA new_comps_intf TYPE cl_extr_classes=>ty_class_components.
    DATA new_comp_intf TYPE cl_extr_classes=>ty_class_component.

    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.

        SELECT clsname cmpname cmptype
          FROM seocompo
          INTO CORRESPONDING FIELDS OF TABLE new_comps_including_intf
          FOR ALL ENTRIES IN classes
          WHERE cmptype <> 3 AND clsname = classes-clsname.

        SELECT clsname refclsname FROM seometarel INTO TABLE interfaces
          FOR ALL ENTRIES IN classes
          WHERE clsname = classes-clsname
            AND version = 1 "Active
            AND state = 1 "Implemented
            AND reltype = 1. "Interface implementation

        IF interfaces IS NOT INITIAL.

          SELECT clsname cmpname cmptype
            FROM seocompo
            INTO CORRESPONDING FIELDS OF TABLE new_comps_intf
            FOR ALL ENTRIES IN interfaces
            WHERE cmptype <> 3 AND clsname = interfaces-refclsname.

        ENDIF.

      ENDIF.
    ELSE.
      DATA: seocompo           TYPE ty_seocompo_test,
            selected_component TYPE ty_class_component,
            seometarel         TYPE ty_seometarel_test.
      LOOP AT g_seocompo_test INTO seocompo.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seocompo-clsname.
        IF sy-subrc EQ 0.
          CLEAR selected_component.
          selected_component-clsname = seocompo-clsname.
          selected_component-cmpname = seocompo-cmpname.
          selected_component-cmptype = seocompo-cmptype.
          INSERT selected_component INTO TABLE new_comps_including_intf.
        ENDIF.

      ENDLOOP.

      LOOP AT g_seometarel_test INTO seometarel
        WHERE version = 1
          AND state = 1
          AND reltype = 1.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seometarel-clsname.
        IF sy-subrc EQ 0.
          CLEAR interface.
          interface-clsname = seometarel-clsname.
          interface-refclsname = seometarel-refclsname.
          INSERT interface INTO TABLE interfaces.
        ENDIF.

      ENDLOOP.

      IF interfaces IS NOT INITIAL.

        LOOP AT g_seocompo_test INTO seocompo.

          READ TABLE interfaces TRANSPORTING NO FIELDS WITH KEY refclsname = seocompo-clsname.
          IF sy-subrc EQ 0.
            CLEAR selected_component.
            selected_component-clsname = seocompo-clsname.
            selected_component-cmpname = seocompo-cmpname.
            selected_component-cmptype = seocompo-cmptype.
            INSERT selected_component INTO TABLE new_comps_intf.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ENDIF.

    LOOP AT new_comps_including_intf INTO new_comp_including_intf.
      INSERT new_comp_including_intf INTO TABLE components.
    ENDLOOP.

    " Add interface attributes and methods
    DATA component  TYPE ty_class_component.
    LOOP AT interfaces INTO interface.
      LOOP AT new_comps_intf INTO new_comp_intf WHERE clsname = interface-refclsname.
        CLEAR component.
        component-clsname = interface-clsname.
        CONCATENATE interface-refclsname '~' new_comp_intf-cmpname INTO component-cmpname.
        component-cmptype = new_comp_intf-cmptype.
        INSERT component INTO TABLE components.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD _select_from_tadir.

    " Select from TADIR
    CLEAR selected_classes.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF i_packages IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
          pgmid = 'R3TR' AND
          devclass = i_packages-package AND
          ( object = tadir_clas OR
            object = tadir_intf ).

      ENDIF.
    ELSE.
      DATA package LIKE LINE OF i_packages.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT i_packages INTO package.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          ( object = tadir_clas OR object = tadir_intf ) AND
          devclass = package-package.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_classes_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_classes = selected_classes ).

  ENDMETHOD.
  METHOD _select_from_tadir_by_comp.

    " Select from TADIR
    CLEAR selected_classes.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF obj_names  IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN obj_names   WHERE
          pgmid = 'R3TR' AND
          obj_name = obj_names-obj_name AND
          ( object = tadir_clas OR
            object = tadir_intf ).

      ENDIF.
    ELSE.
      DATA obj_name LIKE LINE OF obj_names.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT obj_names INTO obj_name.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          ( object = tadir_clas OR object = tadir_intf ) AND
          obj_name = obj_name-obj_name.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_classes_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_classes = selected_classes ).

  ENDMETHOD.
  METHOD _add_classes_to_model.

    DATA: class LIKE LINE OF g_selected_classes.
    DATA last_id TYPE i.
    LOOP AT classes INTO class.
      famix_package->add( name = class-devclass ).

      IF class-clstype EQ class_type.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
        famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                      name                   = class-clsname
                                      modifiers              = cl_extract_sap2=>modifier_abapglobalclass
                            IMPORTING id         = last_id ).
*        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
*                                  name       = class-clsname
*                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalclass
*                        IMPORTING id         = last_id ).
        famix_class->set_parent_package( element_id     = last_id
                                       parent_package = class-devclass ).
      ELSEIF class-clstype EQ interface_type.
        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
        " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
        famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                      name                   = class-clsname
                                      modifiers              = cl_extract_sap2=>modifier_abapglobalinterface
                            IMPORTING id         = last_id ).
*        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
*                                  name       = class-clsname
*                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalinterface
*                        IMPORTING id         = last_id ).
        famix_class->set_parent_package( element_id     = last_id
                                       parent_package = class-devclass ).
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        famix_class->is_interface( element_id = last_id ).
      ELSE.
        CONTINUE.
      ENDIF.

      DATA component TYPE ty_class_component.

      LOOP AT components INTO component WHERE clsname = class-clsname.

        CASE component-cmptype.
          WHEN attribute_type.


*    DATA last_id TYPE i.!
            famix_attribute->add( EXPORTING name = component-cmpname IMPORTING id = last_id ).
            famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                          parent_element = 'FAMIX.Class'
                                                          parent_name_group = 'ABAP_CLASS'
                                                          parent_name    = class-clsname ).

            famix_attribute->store_id( EXPORTING class     = class-clsname
                                                 attribute = component-cmpname ).

*            sap_attribute->add( EXPORTING class     = class-clsname
*                                          attribute = component-cmpname ).
          WHEN method_type OR event_type.
            " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
            " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
            famix_method->add( EXPORTING name = component-cmpname IMPORTING id = last_id ).

            " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
            " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
            famix_method->set_signature( element_id = last_id
                                           signature = component-cmpname ).

            famix_method->set_parent_type( EXPORTING element_id = last_id
                                                       parent_element = 'FAMIX.Class'
                                                       parent_name_group = 'ABAP_CLASS'
                                                       parent_name    = class-clsname ).


            famix_method->store_id( EXPORTING class  = class-clsname
                                                method = component-cmpname ).


*            sap_method->add( EXPORTING class  = class-clsname
*                                       method = component-cmpname ).
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

      ENDLOOP.

    ENDLOOP.

    "! TBD Add implementation of interfaces here

    LOOP AT components INTO component.
      DATA: interface TYPE string,
            element   TYPE string.
      SPLIT component-cmpname AT '~' INTO interface element.

      IF element IS NOT INITIAL.

        CASE component-cmptype.
          WHEN attribute_type.

          WHEN method_type.

            DATA: class_comp_id     TYPE i,
                  interface_comp_id TYPE i.

            class_comp_id = famix_method->get_id(
*                class_name_group  =
                 class             = component-clsname
*                method_name_group =
                 method            = component-cmpname
*              RECEIVING
*                id                =
             ).

            interface_comp_id = famix_method->get_id(
*                        class_name_group  =
                        class             = interface
*                        method_name_group =
                        method            = element
                    ).

            DATA invocation_id TYPE i.
            invocation_id = famix_invocation->add( ).
            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                       sender_id     = interface_comp_id
                                                                       candidates_id = class_comp_id
                                                                       signature     = 'DUMMY' ).

          WHEN OTHERS.
            "! TBD is this error handling OK?
            ASSERT 1 = 2.
        ENDCASE.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD add_to_model.

    DATA line LIKE LINE OF g_selected_components_new.

    LOOP AT g_selected_components_new INTO line.
      INSERT line INTO TABLE g_selected_components.
    ENDLOOP.

    me->_add_classes_to_model( EXPORTING famix_package    = famix_package
                                          famix_class      = famix_class
                                          famix_method    = famix_method
                                          famix_attribute  = famix_attribute
                                          famix_invocation = famix_invocation
                                          famix_access = famix_access
                                          classes       = g_selected_classes
                                          components    = g_selected_components ).

*    me->_add_classes_to_model( EXPORTING famix_package    = famix_package
*                                          famix_class      = famix_class
*                                          famix_method    = famix_method
*                                          famix_attribute  = famix_attribute
*                                          classes       = g_add_classes
*                                          components    = g_add_components ).

  ENDMETHOD.
  METHOD _check_existence_and_add_intf.

    TYPES: BEGIN OF ty_existing,
             clsname TYPE seoclsname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <class> LIKE LINE OF classes.

*    " Build helper table with correct type for select
*    LOOP AT g_selected_tadir INTO selected_tadir.
*      line-clsname = selected_tadir-obj_name.
*      INSERT line INTO TABLE check_table.
*    ENDLOOP.

    " Select from database
    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.
        SELECT clsname FROM seoclass INTO TABLE table FOR ALL ENTRIES IN classes WHERE clsname = classes-clsname.
      ENDIF.

    ELSE.

      DATA seoclass_line TYPE ty_seoclass_test.

      LOOP AT g_seoclass_test INTO seoclass_line.
        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seoclass_line-clsname.
        IF sy-subrc EQ 0.
          line-clsname = seoclass_line-clsname.
          INSERT line INTO TABLE table.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " Mark as existing
    LOOP AT table INTO line.

      READ TABLE classes ASSIGNING <class> WITH KEY clsname = line-clsname.
      IF sy-subrc EQ 0.
        <class>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE classes WHERE exists = abap_false.

    " Add interfaces

    TYPES: BEGIN OF ty_interface,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
           END OF ty_interface.
    TYPES: ty_interfaces TYPE STANDARD TABLE OF ty_interface.
    DATA interface TYPE ty_interface.
    DATA interfaces TYPE ty_interfaces.

    " Select from database
    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.

        SELECT clsname refclsname FROM seometarel INTO TABLE interfaces
          FOR ALL ENTRIES IN classes
          WHERE clsname = classes-clsname
            AND version = 1 "Active
            AND state = 1 "Implemented
            AND reltype = 1. "Interface implementation

      ENDIF.

    ELSE.

      DATA seometarel         TYPE ty_seometarel_test.

      "! TBD this is duplicate code, remove by single access to database

      LOOP AT g_seometarel_test INTO seometarel
          WHERE version = 1
            AND state = 1
            AND reltype = 1.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seometarel-clsname.
        IF sy-subrc EQ 0.
          CLEAR interface.
          interface-clsname = seometarel-clsname.
          interface-refclsname = seometarel-refclsname.
          INSERT interface INTO TABLE interfaces.
        ENDIF.

      ENDLOOP.

    ENDIF.

    " Find interfaces that are not yet selected
    LOOP AT interfaces INTO interface.

      READ TABLE g_selected_classes TRANSPORTING NO FIELDS WITH TABLE KEY clsname = interface-refclsname.
      IF sy-subrc <> 0.
        "TBD Add to classes
        DATA class  TYPE ty_class.
        CLEAR class.
        class-clsname = interface-refclsname.
        class-clstype = interface_type.
        "! TBD fill package correct
        class-devclass = ''.
        class-exists = abap_true.
        INSERT class INTO TABLE classes.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.
  METHOD _exclude_classes_from_where_us.

    DATA class TYPE cl_extr_classes=>ty_class.

    " Exclude classes from where used analysis
    " SAP_2_FAMIX_65 : The SAP Interface will not be returned to do a where used analysis
    DATA component  TYPE ty_class_component.

    IF g_exclude_found_sap_intf EQ abap_true.

      LOOP AT c_components INTO component.
        READ TABLE g_selected_classes INTO class WITH TABLE KEY  clsname  = component-clsname.
        ASSERT sy-subrc EQ 0.
        IF class-clstype EQ interface_type.
          IF component-clsname CP 'Y*'
          OR component-clsname CP 'Z*'
          OR component-clsname CP '/*' .
            " OK
          ELSE.

            DELETE c_components.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_TABLES IMPLEMENTATION.
  METHOD add_and_sort_to_tables_table.

    DATA tadirline TYPE cl_extr_classes=>ty_tadir_test.

    "Add and sort to classes table
    DATA table_line TYPE ty_table.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR table_line.

      table_line-tabname = tadirline-obj_name.
      table_line-devclass = tadirline-devclass.

      INSERT table_line INTO TABLE c_selected_tables .

    ENDLOOP.

    SORT c_selected_tables BY tabname.

  ENDMETHOD.
  METHOD add_to_model.

    DATA: table LIKE LINE OF g_selected_tables.
    DATA last_id TYPE i.
    LOOP AT g_selected_tables INTO table.
      famix_package->add( name = table-devclass ).

      " SAP_2_FAMIX_54        Map database tables to FAMIX Class
      " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
      famix_class->add( EXPORTING name_group             = 'ABAP_TABLE'
                                  name                   = table-tabname
                                  modifiers              = cl_extract_sap2=>modifier_dbtable
                          IMPORTING id         = last_id ).
      famix_class->set_parent_package( EXPORTING element_id         = last_id
                                                 parent_package     = table-devclass ).
      DATA dummy_attribute_id TYPE i.
      " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
      famix_attribute->add( EXPORTING name                   = table-tabname
                            IMPORTING id                     = dummy_attribute_id ).

      famix_attribute->set_parent_type( EXPORTING element_id         = dummy_attribute_id
                                                  parent_id          = last_id ).

      famix_attribute->store_id( EXPORTING class     = table-tabname
                                           attribute = table-tabname ).

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.
    IF tadir_test IS SUPPLIED OR
       dd02l_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_dd02l_test = dd02l_test.
      g_is_test = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD select_tables_by_packages.

    g_selected_tables = _select_from_tadir( packages ).
    _check_existence( CHANGING db_tables = g_selected_tables ).
    g_selected_tables_new = g_selected_tables.

  ENDMETHOD.
  METHOD _check_existence.

    TYPES: BEGIN OF ty_existing,
             tabname TYPE tabname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <db_table> LIKE LINE OF db_tables.

*    " Build helper table with correct type for select
*    LOOP AT g_selected_tadir INTO selected_tadir.
*      line-clsname = selected_tadir-obj_name.
*      INSERT line INTO TABLE check_table.
*    ENDLOOP.

    " Select from database
    IF g_is_test EQ abap_false.

      IF db_tables IS NOT INITIAL.
        SELECT tabname FROM dd02l INTO TABLE table FOR ALL ENTRIES IN db_tables WHERE tabname = db_tables-tabname.
      ENDIF.

    ELSE.

      DATA dd02l_line TYPE ty_dd02l_test.

      LOOP AT g_dd02l_test INTO dd02l_line.
        READ TABLE db_tables TRANSPORTING NO FIELDS WITH KEY tabname = dd02l_line-tabname.
        IF sy-subrc EQ 0.
          line-tabname = dd02l_line-tabname.
          INSERT line INTO TABLE table.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " Mark as existing
    LOOP AT table INTO line.

      READ TABLE db_tables ASSIGNING <db_table> WITH KEY tabname = line-tabname.
      IF sy-subrc EQ 0.
        <db_table>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE db_tables WHERE exists = abap_false.

  ENDMETHOD.
  METHOD _select_from_tadir.

    " Select from TADIR
    CLEAR selected_tables.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF i_packages IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
          pgmid = 'R3TR' AND
          devclass = i_packages-package AND
          object = tadir_table.

      ENDIF.
    ELSE.
      DATA package LIKE LINE OF i_packages.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT i_packages INTO package.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          object = tadir_table AND
          devclass = package-package.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_tables_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_tables = selected_tables ).

  ENDMETHOD.
  METHOD get_tables_to_do_where_used.
    DATA: line        LIKE LINE OF g_selected_tables_new,
          line_public TYPE ty_table_public.
    LOOP AT g_selected_tables_new INTO line.
      CLEAR line_public.
      line_public-tabname = line-tabname.
      INSERT line_public INTO TABLE tables.
    ENDLOOP.

    CLEAR g_selected_tables_new.

  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_WEB_DYNPRO IMPLEMENTATION.
  METHOD constructor.
    IF tadir_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD select_classes_by_components.
    DATA component TYPE ty_web_dynpro_component.
    LOOP AT components INTO component.
      INSERT component INTO TABLE g_selctd_web_dynpro_compts.
    ENDLOOP.
  ENDMETHOD.
  METHOD add_to_model.

    DATA wdy_comp TYPE ty_web_dynpro_component.
    DATA class_id TYPE i.
    DATA method_id TYPE i.


    LOOP AT g_selctd_web_dynpro_compts INTO wdy_comp.
      famix_class->add( EXPORTING name_group             = 'WEB_DYNPRO'
                                name                   = wdy_comp-component_name
                                modifiers              = 'ABAPWebDynproComponent'
                        IMPORTING id         = class_id ).
      famix_method->add( EXPORTING name = wdy_comp-controller_name IMPORTING id = method_id ).

      famix_method->set_signature( element_id = method_id
                                     signature = wdy_comp-controller_name ).
      famix_method->set_parent_type(
        EXPORTING
          element_id         = method_id
          parent_element     = 'FAMIX.Class'
          parent_id          = class_id ).

      "! TBD Really required, this appears to be not exact, no namegroup, ...
      famix_method->store_id( EXPORTING class  = wdy_comp-component_name
                                        method = wdy_comp-controller_name ).


    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_WHERE_USED IMPLEMENTATION.
  METHOD constructor.

    g_wbcrossgt_test = wbcrossgt_test.

    g_includes_to_components_test = includes_to_components.

    IF g_wbcrossgt_test IS NOT INITIAL
    OR g_includes_to_components_test IS NOT INITIAL.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD _select_where_used_table.

    DATA name TYPE ty_where_used_name.

    " Select where used table
    IF g_is_test EQ abap_false.
      IF i_names_to_components IS NOT INITIAL.
        SELECT otype name include direct indirect
          FROM wbcrossgt
          INTO TABLE r_found_wbcrossgt
          FOR ALL ENTRIES IN i_names_to_components
          WHERE otype = i_names_to_components-otype
            AND name = i_names_to_components-where_used_name.
      ENDIF.
    ELSE.

      DATA found_wbcrossgt_line TYPE ty_wbcrossgt_test.

      LOOP AT i_names_to_components INTO name.
        LOOP AT g_wbcrossgt_test INTO found_wbcrossgt_line WHERE otype = name-otype
                                                             AND name = name-where_used_name.

          INSERT found_wbcrossgt_line INTO TABLE r_found_wbcrossgt.

        ENDLOOP.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD _determine_mapping_include_to.

    DATA found_wbcrossgt_line TYPE cl_extr_where_used_classes=>ty_wbcrossgt_test.

    " Determine mapping include to component

    DATA: includes_2_components TYPE ty_includes_to_components,
          include_2_component   TYPE ty_include_to_component.

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.
      include_2_component-include = found_wbcrossgt_line-include.
      INSERT include_2_component INTO TABLE r_includes_2_components.
    ENDLOOP.

    FIELD-SYMBOLS: <include_2_component> TYPE ty_include_to_component.

    LOOP AT r_includes_2_components ASSIGNING <include_2_component>.

      " Analyze where used table
      DATA: pgmid    TYPE pgmid,
            object   TYPE trobjtype,
            obj_name TYPE trobj_name.

      IF g_is_test EQ abap_false.

        cl_oo_include_naming=>get_trkey_by_include(
          EXPORTING
            progname        = <include_2_component>-include
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
        IF sy-subrc <> 0.
          "! TBD handle

          "Check for usage in Web Dynpro ABAP
          DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.


            SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = <include_2_component>-include AND srtf2 = 0.


          IF sy-subrc EQ 0.
            <include_2_component>-is_webdynpro = abap_true.
            <include_2_component>-component_name = ls_wd_sourcemap-component_name.
            <include_2_component>-controller_name = ls_wd_sourcemap-controller_name.
          ENDIF.

        ELSE.
          <include_2_component>-clsname = obj_name+0(30).
          "! <include_2_component>-clstype = ?
          <include_2_component>-cmpname = obj_name+30(30).
          CASE object.
            WHEN 'METH'.
              <include_2_component>-cmptype = cl_extr_classes=>method_type.
            WHEN OTHERS.
              "! TBD handle
          ENDCASE.
          <include_2_component>-is_class_component = abap_true.
        ENDIF.

      ELSE.

        DATA include_to_component_test TYPE ty_include_to_component.

        READ TABLE g_includes_to_components_test INTO include_to_component_test WITH TABLE KEY include = <include_2_component>-include.
        IF sy-subrc EQ 0.
          <include_2_component>-clsname = include_to_component_test-clsname.
          <include_2_component>-cmpname = include_to_component_test-cmpname.
          <include_2_component>-cmptype = include_to_component_test-cmptype.
          <include_2_component>-is_class_component = abap_true.
        ENDIF.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.
  METHOD get_components_where_used.
    components = g_class_components_where_used.
    CLEAR g_class_components_where_used.
    web_dynpro_components = g_web_dynpro_cmpnts_where_used.
    CLEAR g_web_dynpro_cmpnts_where_used.
  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_WHERE_USED_CLASSES IMPLEMENTATION.
  METHOD add_usage_to_model.

    DATA comp_used_by_comp TYPE ty_comp_used_by_element.

    LOOP AT g_comps_used_by_comps INTO comp_used_by_comp.
      IF comp_used_by_comp-is_class_component EQ abap_true.

        IF comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>attribute_type.
          CONTINUE.
        ENDIF.

        ASSERT comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>method_type OR
               comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>event_type.

        DATA using_method_id TYPE i.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-used_by_clsname
                                                method = comp_used_by_comp-used_by_cmpname ).

      ELSEIF comp_used_by_comp-is_webdynpro EQ abap_true.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-component_name
                                                method = comp_used_by_comp-controller_name ).

      ELSE.
        ASSERT 1 = 2.
      ENDIF.

      IF using_method_id IS INITIAL AND comp_used_by_comp-is_class_component EQ abap_true.
        "! TBD report or handle this better.
        "! Should this not have been solved already because while extracting classe SEOMETAREL is already used
        " This happened because interface methods are not in table SEOCOMPO
        " The information is in table SEOMETAREL

        "! TBD THis is duplicate coding!
        "!

        " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
        " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
        famix_method->add( EXPORTING name = comp_used_by_comp-used_by_cmpname IMPORTING id = using_method_id ).

        " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
        " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
        famix_method->set_signature( element_id = using_method_id
                                       signature = comp_used_by_comp-used_by_cmpname ).

        famix_method->set_parent_type( EXPORTING element_id = using_method_id
                                                   parent_element = 'FAMIX.Class'
                                                   parent_name_group = 'ABAP_CLASS'
                                                   parent_name    = comp_used_by_comp-used_by_clsname ).


        famix_method->store_id( EXPORTING class  = comp_used_by_comp-used_by_clsname
                                            method = comp_used_by_comp-used_by_cmpname ).


*        using_method_id = sap_method->add( class  = comp_used_by_comp-used_by_clsname
*                                           method = comp_used_by_comp-used_by_cmpname ).
      ENDIF.

      DATA used_id TYPE i.
      CASE comp_used_by_comp-cmptype.
        WHEN cl_extr_classes=>attribute_type.

          used_id = famix_attribute->get_id( class     = comp_used_by_comp-clsname
                                             attribute = comp_used_by_comp-cmpname ).


          " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
          " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

          IF famix_access->is_new_access( accessor_id = using_method_id
                                            variable_id = used_id )
             EQ abap_true.
            DATA last_id2 TYPE i.
            last_id2 = famix_access->add( ).
            famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id2
                                                                      accessor_id = using_method_id
                                                                      variable_id = used_id ).
          ENDIF.

*          sap_access->add_access( used_attribute = used_id
*                                  using_method   = using_method_id ).


        WHEN cl_extr_classes=>method_type OR cl_extr_classes=>event_type.

          used_id = famix_method->get_id( class_name_group = ''
                                          class            = comp_used_by_comp-clsname
                                          method           = comp_used_by_comp-cmpname ).

          " This is an interface. Reverse the usage direction
          " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method

          DATA: temp TYPE seocmpname.
          temp = comp_used_by_comp-clsname && |~| && comp_used_by_comp-cmpname.

          IF comp_used_by_comp-used_by_cmpname EQ temp AND comp_used_by_comp-is_class_component EQ abap_true.

            "! TBD This will not be used in future, because the implementation of an interface will be handled in CL_EXTR_CLASSES

            CONTINUE.

            DATA: inv_used_id  TYPE i,
                  inv_using_id TYPE i.
            " Reverse direction
            inv_used_id = using_method_id.
            inv_using_id = used_id.
*            sap_invocation->add_invocation( used_method  = using_method_id
*                                            using_method = used_id ).

          ELSE.
            inv_used_id = used_id.
            inv_using_id = using_method_id.

          ENDIF.
          " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
          " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
          IF famix_invocation->is_new_invocation_to_candidate( sender_id     = inv_using_id
                                                                 candidates_id = inv_used_id )
             EQ abap_true.

            DATA invocation_id TYPE i.
            invocation_id = famix_invocation->add( ).
            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                       sender_id     = inv_using_id
                                                                       candidates_id = inv_used_id
                                                                       signature     = 'DUMMY' ).
          ENDIF.


*            sap_invocation->add_invocation( used_method  = inv_used_id
*                                            using_method = inv_using_id ).


        WHEN OTHERS.
          ASSERT 1 = 2.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( wbcrossgt_test         = wbcrossgt_test
                        includes_to_components = includes_to_components ).
  ENDMETHOD.
  METHOD used_by_class_component.

    DATA name_to_component TYPE ty_name_to_component.
    DATA names_to_components TYPE ty_names_to_components.

    DATA where_used_name TYPE eu_lname.
    DATA class_component TYPE cl_extr_classes=>ty_class_component.

    g_class_components_initial = class_components.

    names_to_components = _get_mapping_to_name( class_components ).

    DATA found_wbcrossgt TYPE ty_t_wbcrossgt_test.
    DATA: names TYPE ty_where_used_names,
          name  TYPE ty_where_used_name.

    LOOP AT names_to_components INTO name_to_component.
      CLEAR name.
      name-otype = name_to_component-otype.
      name-where_used_name = name_to_component-where_used_name.
      INSERT name INTO TABLE names.
    ENDLOOP.

    found_wbcrossgt = _select_where_used_table( names ).

    DATA includes_2_components TYPE ty_includes_to_components.

    includes_2_components = _determine_mapping_include_to( found_wbcrossgt ).

    _fill_comps_used_by_elements(
          i_names_to_components   = names_to_components
          i_found_wbcrossgt       = found_wbcrossgt
          i_includes_2_components = includes_2_components ).

    SORT g_comps_used_by_comps.

  ENDMETHOD.
  METHOD _get_mapping_to_name.

    DATA n2c TYPE cl_extr_where_used_classes=>ty_name_to_component.
    DATA comp TYPE cl_extr_classes=>ty_class_component.

    " get mapping to name
    LOOP AT i_class_components INTO comp.
      CLEAR n2c.
      CASE comp-cmptype.
        WHEN cl_extr_classes=>method_type.
          n2c-otype = 'ME'.
        WHEN cl_extr_classes=>attribute_type.
          n2c-otype = 'DA'.
        WHEN cl_extr_classes=>event_type.
          n2c-otype = 'EV'.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
      n2c-where_used_name = comp-clsname && |\\| && n2c-otype && |:| && comp-cmpname.
      n2c-clsname = comp-clsname.
      n2c-cmpname = comp-cmpname.
      n2c-cmptype = comp-cmptype.
      INSERT n2c INTO TABLE r_names_to_components.
    ENDLOOP.

  ENDMETHOD.
  METHOD _fill_comps_used_by_elements.

    DATA ntc TYPE cl_extr_where_used_classes=>ty_name_to_component.
    DATA found_wbcrossgt_line TYPE cl_extr_where_used_classes=>ty_wbcrossgt_test.
    DATA include_2_component TYPE cl_extr_where_used_classes=>ty_include_to_component.

    " fill comps used by comps

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.

      DATA cubc TYPE ty_comp_used_by_element.

      CLEAR cubc.
      READ TABLE i_names_to_components INTO ntc WITH TABLE KEY otype           = found_wbcrossgt_line-otype
                                                                           where_used_name = found_wbcrossgt_line-name.
      cubc-clsname = ntc-clsname.
      cubc-cmpname = ntc-cmpname.
      cubc-cmptype = ntc-cmptype.

      READ TABLE i_includes_2_components INTO include_2_component WITH TABLE KEY include = found_wbcrossgt_line-include.
      IF include_2_component-is_class_component EQ abap_true.
        cubc-is_class_component = abap_true.
        cubc-used_by_clsname = include_2_component-clsname.
        cubc-used_by_cmpname = include_2_component-cmpname.
        cubc-used_by_cmptype = include_2_component-cmptype.

        INSERT cubc INTO TABLE g_comps_used_by_comps.

        READ TABLE g_class_components_initial TRANSPORTING NO FIELDS WITH TABLE KEY clsname = cubc-used_by_clsname
                                                                                    cmpname = cubc-used_by_cmpname
                                                                                    cmptype = cubc-used_by_cmptype.
        IF sy-subrc <> 0.
          DATA cwu LIKE LINE OF g_class_components_initial.
          cwu-clsname = cubc-used_by_clsname.
          cwu-cmpname = cubc-used_by_cmpname.
          cwu-cmptype = cubc-used_by_cmptype.
          INSERT cwu INTO TABLE g_class_components_where_used.
        ENDIF.

      ELSEIF include_2_component-is_webdynpro EQ abap_true.

        cubc-is_webdynpro = abap_true.
        cubc-component_name = include_2_component-component_name.
        cubc-controller_name = include_2_component-controller_name.

        INSERT cubc INTO TABLE g_comps_used_by_comps.

        DATA wdcwu LIKE LINE OF g_web_dynpro_cmpnts_where_used.
        wdcwu-component_name = include_2_component-component_name.
        wdcwu-controller_name = include_2_component-controller_name.
        INSERT wdcwu INTO TABLE g_web_dynpro_cmpnts_where_used.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTR_WHERE_USED_TABLES IMPLEMENTATION.
  METHOD constructor.

    super->constructor( wbcrossgt_test         = wbcrossgt_test
                        includes_to_components = includes_to_components ).

  ENDMETHOD.
  METHOD used_by_table.

    g_tables_initial = tables.

    DATA: names_to_tables TYPE ty_names_to_tables,
          name_to_table   TYPE ty_name_to_table.

    names_to_tables = _get_mapping_to_name( tables = tables ).

    DATA found_wbcrossgt TYPE ty_t_wbcrossgt_test.
    DATA: names TYPE ty_where_used_names,
          name  TYPE ty_where_used_name.

    LOOP AT names_to_tables INTO name_to_table.
      CLEAR name.
      name-otype = name_to_table-otype.
      name-where_used_name = name_to_table-where_used_name.
      INSERT name INTO TABLE names.
    ENDLOOP.

    found_wbcrossgt = _select_where_used_table( names ).

    DATA includes_2_components TYPE ty_includes_to_components.

    includes_2_components = _determine_mapping_include_to( found_wbcrossgt ).

    _fill_tables_used_by_elements( EXPORTING i_names_to_components   = names_to_tables
                                          i_found_wbcrossgt       = found_wbcrossgt
                                          i_includes_2_components = includes_2_components ).

    SORT g_tables_used_by_elements.

  ENDMETHOD.
  METHOD _get_mapping_to_name.

    DATA n2t TYPE cl_extr_where_used_tables=>ty_name_to_table.
    DATA table TYPE cl_extr_tables=>ty_table_public.

    LOOP AT tables INTO table.
      CLEAR n2t.

      n2t-otype = 'TY'.
      "! TYPE is also used for classes, ... What happen if a table has the same name as a class?
      n2t-where_used_name = table-tabname.
      n2t-table = table-tabname.
      INSERT n2t INTO TABLE r_names_to_tables.
    ENDLOOP.

  ENDMETHOD.
  METHOD add_usage_to_model.

    DATA comp_used_by_comp TYPE ty_table_used_by_element.

    LOOP AT g_tables_used_by_elements INTO comp_used_by_comp.

      IF comp_used_by_comp-is_class_component EQ abap_true.

        IF comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>attribute_type.
          CONTINUE.
        ENDIF.

        ASSERT comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>method_type OR
               comp_used_by_comp-used_by_cmptype EQ cl_extr_classes=>event_type.

        DATA using_method_id TYPE i.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-used_by_clsname
                                                method = comp_used_by_comp-used_by_cmpname ).
      ELSEIF comp_used_by_comp-is_webdynpro EQ abap_true.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-component_name
                                                method = comp_used_by_comp-controller_name ).
      ELSE.
        ASSERT 1 = 2.
      ENDIF.


*      IF using_method_id IS INITIAL.
*        "! TBD report or handle this better.
*        " This happened because interface methods are not in table SEOCOMPO
*        " The information is in table SEOMETAREL
*
*        "! TBD THis is duplicate coding!
*        "!
*
*        " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
*        " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
*        famix_method->add( EXPORTING name = comp_used_by_comp-used_by_cmpname IMPORTING id = using_method_id ).
*
*        " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
*        " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
*        famix_method->set_signature( element_id = using_method_id
*                                       signature = comp_used_by_comp-used_by_cmpname ).
*
*        famix_method->set_parent_type( EXPORTING element_id = using_method_id
*                                                   parent_element = 'FAMIX.Class'
*                                                   parent_name_group = 'ABAP_CLASS'
*                                                   parent_name    = comp_used_by_comp-used_by_clsname ).
*
*
*        famix_method->store_id( EXPORTING class  = comp_used_by_comp-used_by_clsname
*                                            method = comp_used_by_comp-used_by_cmpname ).
*
*
**        using_method_id = sap_method->add( class  = comp_used_by_comp-used_by_clsname
**                                           method = comp_used_by_comp-used_by_cmpname ).
*      ENDIF.

      DATA used_id TYPE i.
*      CASE comp_used_by_comp-cmptype.
*        WHEN cl_extr_classes=>attribute_type.

      used_id = famix_attribute->get_id( class     = comp_used_by_comp-table
                                         attribute = comp_used_by_comp-table ).


      " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
      " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

      IF famix_access->is_new_access( accessor_id = using_method_id
                                        variable_id = used_id )
         EQ abap_true.
        DATA last_id2 TYPE i.
        last_id2 = famix_access->add( ).
        famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id2
                                                                  accessor_id = using_method_id
                                                                  variable_id = used_id ).
      ENDIF.

*          sap_access->add_access( used_attribute = used_id
*                                  using_method   = using_method_id ).


*        WHEN cl_extr_classes=>method_type OR cl_extr_classes=>event_type.
*
*          used_id = famix_method->get_id( class_name_group = ''
*                                          class            = comp_used_by_comp-clsname
*                                          method           = comp_used_by_comp-cmpname ).
*
*          " This is an interface. Reverse the usage direction
*          " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method
*
*          DATA: temp TYPE seocmpname.
*          temp = comp_used_by_comp-clsname && |~| && comp_used_by_comp-cmpname.
*
*          IF comp_used_by_comp-used_by_cmpname EQ temp.
*
*            "! TBD This will not be used in future, because the implementation of an interface will be handled in CL_EXTR_CLASSES
*
*            CONTINUE.
*
*            DATA: inv_used_id  TYPE i,
*                  inv_using_id TYPE i.
*            " Reverse direction
*            inv_used_id = using_method_id.
*            inv_using_id = used_id.
**            sap_invocation->add_invocation( used_method  = using_method_id
**                                            using_method = used_id ).
*
*          ELSE.
*            inv_used_id = used_id.
*            inv_using_id = using_method_id.
*
*          ENDIF.
*          " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
*          " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
*          IF famix_invocation->is_new_invocation_to_candidate( sender_id     = inv_using_id
*                                                                 candidates_id = inv_used_id )
*             EQ abap_true.
*
*            DATA invocation_id TYPE i.
*            invocation_id = famix_invocation->add( ).
*            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
*                                                                       sender_id     = inv_using_id
*                                                                       candidates_id = inv_used_id
*                                                                       signature     = 'DUMMY' ).
*          ENDIF.
*
*
**            sap_invocation->add_invocation( used_method  = inv_used_id
**                                            using_method = inv_using_id ).
*

*        WHEN OTHERS.
*          ASSERT 1 = 2.
*
*      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD _fill_tables_used_by_elements.

    DATA ntc TYPE ty_name_to_table.
    DATA found_wbcrossgt_line TYPE ty_wbcrossgt_test.
    DATA include_2_component TYPE ty_include_to_component.

    " fill comps used by comps

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.

      DATA tbube TYPE ty_table_used_by_element.

      CLEAR tbube.
      READ TABLE i_names_to_components INTO ntc WITH TABLE KEY otype           = found_wbcrossgt_line-otype
                                                               where_used_name = found_wbcrossgt_line-name.
      tbube-table = ntc-table.

      READ TABLE i_includes_2_components INTO include_2_component WITH TABLE KEY include = found_wbcrossgt_line-include.
      IF include_2_component-is_class_component EQ abap_true.
        tbube-is_class_component = abap_true.
        tbube-used_by_clsname = include_2_component-clsname.
        tbube-used_by_cmpname = include_2_component-cmpname.
        tbube-used_by_cmptype = include_2_component-cmptype.

        INSERT tbube INTO TABLE g_tables_used_by_elements.

        DATA cwu LIKE LINE OF g_class_components_where_used.
        cwu-clsname = tbube-used_by_clsname.
        cwu-cmpname = tbube-used_by_cmpname.
        cwu-cmptype = tbube-used_by_cmptype.
        INSERT cwu INTO TABLE g_class_components_where_used.

      ELSEIF include_2_component-is_webdynpro EQ abap_true.

        tbube-is_webdynpro = abap_true.
        tbube-component_name = include_2_component-component_name.
        tbube-controller_name = include_2_component-controller_name.

        INSERT tbube INTO TABLE g_tables_used_by_elements.

        DATA wdcwu LIKE LINE OF g_web_dynpro_cmpnts_where_used.
        wdcwu-component_name = include_2_component-component_name.
        wdcwu-controller_name = include_2_component-controller_name.
        INSERT wdcwu INTO TABLE g_web_dynpro_cmpnts_where_used.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

CLASS CL_EXTRACT_SAP2 IMPLEMENTATION.
  METHOD constructor.

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

  ENDMETHOD.
  METHOD extract.
    DATA: extract_packages TYPE REF TO cl_extr_packages.

      CREATE OBJECT extract_packages.

    DATA extract_classes TYPE REF TO cl_extr_classes.

      CREATE OBJECT extract_classes EXPORTING i_exclude_found_sap_intf = i_exclude_found_sap_intf.

    DATA extract_tables TYPE REF TO cl_extr_tables.

      CREATE OBJECT extract_tables.

    DATA extract_web_dynpro TYPE REF TO cl_extr_web_dynpro.

      CREATE OBJECT extract_web_dynpro.

    DATA extract_where_used_classes TYPE REF TO cl_extr_where_used_classes.

      CREATE OBJECT extract_where_used_classes.

    DATA extract_where_used_tables TYPE REF TO cl_extr_where_used_tables.

      CREATE OBJECT extract_where_used_tables.

    _initial_selections_by_filter( i_top_packages        = i_top_packages
                                   i_sub_packages_filter = i_sub_packages_filter
                                   i_search_sub_packages = i_search_sub_packages
                                   i_extract_packages    = extract_packages
                                   i_extract_classes     = extract_classes
                                   i_extract_tables      = extract_tables ).

    _get_using_elements( i_extract_classes            = extract_classes
                         i_extract_tables             = extract_tables
                         i_extract_web_dynpro         = extract_web_dynpro
                         i_extract_where_used_classes = extract_where_used_classes
                         i_extract_where_used_tables  = extract_where_used_tables
                         i_search_up                  = i_search_up ).

    mse_model = _add_all_to_model_and_make_mse( i_extract_packages           = extract_packages
                                                i_extract_classes            = extract_classes
                                                i_extract_tables             = extract_tables
                                                i_extract_web_dynpro         = extract_web_dynpro
                                                i_extract_where_used_classes = extract_where_used_classes
                                                i_extract_where_used_tables  = extract_where_used_tables ).

  ENDMETHOD.
  METHOD _add_all_to_model_and_make_mse.

    i_extract_packages->add_selected_packages_to_model( famix_package = famix_package ).

    i_extract_classes->add_to_model( EXPORTING famix_package = famix_package
                                               famix_class     = famix_class
                                               famix_method    = famix_method
                                               famix_attribute = famix_attribute
                                               famix_invocation = famix_invocation
                                               famix_access     = famix_access  ).

    i_extract_tables->add_to_model( EXPORTING famix_package   = famix_package
                                              famix_class     = famix_class
                                              famix_attribute = famix_attribute ).

    i_extract_web_dynpro->add_to_model( EXPORTING famix_class  = famix_class
                                                  famix_method = famix_method ).

    i_extract_where_used_classes->add_usage_to_model( EXPORTING famix_method    = famix_method
                                                                famix_attribute = famix_attribute
                                                                famix_invocation = famix_invocation
                                                                famix_access     = famix_access ).

    i_extract_where_used_tables->add_usage_to_model( EXPORTING famix_method    = famix_method
                                                               famix_attribute = famix_attribute
                                                               famix_invocation = famix_invocation
                                                               famix_access     = famix_access ).

    model->make_mse( IMPORTING mse_model = r_mse_model ).

  ENDMETHOD.
  METHOD _initial_selections_by_filter.

    " Initial selections due to filter of report

    i_extract_packages->select_packages( EXPORTING top_packages           = i_top_packages
                                                 sub_packages_filter    = i_sub_packages_filter
                                                 including_sub_packages = i_search_sub_packages  ).

    i_extract_classes->select_classes_by_packages( packages = i_extract_packages->g_selected_packages ).

    i_extract_tables->select_tables_by_packages( packages = i_extract_packages->g_selected_packages ).

  ENDMETHOD.
  METHOD _get_using_elements.

    DATA: classes_to_do_where_used_up TYPE cl_extr_classes=>ty_class_components,
          tables_to_do_where_used_up  TYPE cl_extr_tables=>ty_tables_public,
          repeat                      TYPE abap_bool,
          counter                     TYPE i.

    counter = i_search_up.

    IF i_search_up EQ 0.

      repeat = abap_false.

    ELSE.

      repeat = abap_true.

    ENDIF.

    WHILE repeat EQ abap_true.

      classes_to_do_where_used_up = i_extract_classes->get_comp_to_do_where_used( ).

      tables_to_do_where_used_up = i_extract_tables->get_tables_to_do_where_used( ).

      IF ( classes_to_do_where_used_up IS INITIAL AND
           tables_to_do_where_used_up IS INITIAL )
        OR counter EQ 0.
        EXIT.
      ENDIF.

      i_extract_where_used_classes->used_by_class_component( class_components = classes_to_do_where_used_up ).

      DATA class_components TYPE cl_extr_classes=>ty_class_components_hashed.
      DATA web_dynpro_components TYPE cl_extr_web_dynpro=>ty_web_dynpro_components_hash.

      i_extract_where_used_classes->get_components_where_used( IMPORTING components            = class_components
                                                                         web_dynpro_components = web_dynpro_components ).

      i_extract_classes->select_classes_by_components( components = class_components ).

      i_extract_web_dynpro->select_classes_by_components( components = web_dynpro_components ).

      i_extract_where_used_tables->used_by_table( tables = tables_to_do_where_used_up ).

      i_extract_where_used_tables->get_components_where_used( IMPORTING components            = class_components
                                                                        web_dynpro_components = web_dynpro_components ).

      i_extract_classes->select_classes_by_components( components = class_components ).

      i_extract_web_dynpro->select_classes_by_components( components = web_dynpro_components ).

      IF counter > 0.
        SUBTRACT 1 FROM counter.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA: mse_model TYPE cl_model=>lines_type.

  DATA sap_extractor TYPE REF TO cl_extract_sap2.

  DATA: ls_pack_line LIKE LINE OF s_pack.
  DATA: ls_pack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_pack INTO ls_pack_line.
    APPEND ls_pack_line TO ls_pack.
  ENDLOOP.

  DATA: ls_spack_line LIKE LINE OF s_pack.
  DATA: ls_spack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_spack INTO ls_spack_line.
    APPEND ls_spack_line TO ls_spack.
  ENDLOOP.

  CREATE OBJECT sap_extractor.

  DATA nothing_done TYPE boolean.
  sap_extractor->extract( EXPORTING i_top_packages           = ls_pack
                                    i_sub_packages_filter    = ls_spack
                                    i_search_sub_packages    = p_sub
                                    i_search_up              = p_up
                                    i_exclude_found_sap_intf = p_ex
                          IMPORTING mse_model             = mse_model
                                    nothing_done          = nothing_done ).

  IF nothing_done EQ abap_true.
    RETURN.
  ENDIF.

  DATA model_outputer TYPE REF TO cl_output_model.
  CREATE OBJECT model_outputer.
  model_outputer->make( mse_model = mse_model g_parameter_download_file = p_down ).
