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
"! 15.02.2016 23:01 issue20 Rainer Winkler
"!
REPORT yrw1_moose_extractor.
TABLES tadir. "So that select-options work

"! To not compare sy-subrc to zero, but more readable to ok
CONSTANTS ok TYPE i VALUE 0.

"! To compare with ID that are returned from methods where the value 0 denotes not found
CONSTANTS not_found TYPE i VALUE 0.
"! Redefines abap_bool to simplify coding (Not always reading abap_...)
TYPES bool TYPE abap_bool.
CONSTANTS:
  "! Redefines abap_true to simplify coding (Not always reading abap_...)
  true  TYPE bool VALUE abap_true,
  "! Redefines abap_false to simplify coding (Not always reading abap_...)
  false TYPE bool VALUE abap_false.

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE text-001.

PARAMETERS: p_sap AS CHECKBOX DEFAULT 'X'.
"! Extract from SAP
DATA g_parameter_extract_from_sap TYPE bool.
g_parameter_extract_from_sap = p_sap.

SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE text-002.

PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_intf AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_prog AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_iprog AS CHECKBOX DEFAULT ' '. "Internal parts of reports


PARAMETERS: rb_fpack RADIOBUTTON GROUP rbsl DEFAULT 'X'.
"! Filter using package
DATA g_filter_using_package TYPE bool.
g_filter_using_package = rb_fpack.

PARAMETERS: p_pack TYPE parentcl DEFAULT ''.
"! Package to be analyzed
DATA g_parameter_package_to_analyze TYPE parentcl.
g_parameter_package_to_analyze = p_pack.

PARAMETERS: rb_fname RADIOBUTTON GROUP rbsl.
"! Filter using name
DATA g_filter_using_name TYPE bool.
g_filter_using_name = rb_fname.

SELECT-OPTIONS s_pack FOR tadir-devclass.

SELECT-OPTIONS s_compsn FOR tadir-obj_name.

SELECTION-SCREEN END OF BLOCK block_selct_sap_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_using_comp WITH FRAME TITLE text-003.

PARAMETERS: p_dm AS CHECKBOX DEFAULT ' '.
"! Usages outside package grouped
"! If false, a recursive search for using components is performed until no further using components are found
DATA g_param_usage_outpack_groupd TYPE bool.
g_param_usage_outpack_groupd = p_dm.

SELECTION-SCREEN END OF BLOCK block_using_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_infos WITH FRAME TITLE text-004.

PARAMETERS: p_list AS CHECKBOX DEFAULT ' '.
"! List Tokens of selected programs
DATA g_parameter_list_tokens TYPE bool.
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

CLASS cl_famix_entity DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
  PROTECTED SECTION.
    DATA g_elementname TYPE string.
    DATA g_model TYPE REF TO cl_model.
    DATA g_last_used_id TYPE i.
ENDCLASS.

CLASS cl_famix_entity IMPLEMENTATION.

  METHOD constructor.
    g_model = model.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! Declare source language
    "! @parameter source_language_element | the FAMIX element of the source language
    "! @parameter source_language_name | the name of the source language
    METHODS set_declared_source_language
      IMPORTING
        source_language_element TYPE string
        source_language_name    TYPE string.
ENDCLASS.

CLASS cl_famix_sourced_entity IMPLEMENTATION.

  METHOD set_declared_source_language.
    g_model->add_reference( EXPORTING attribute_name    = 'declaredSourceLanguage'
                                      elementname       = source_language_element
                                      name_of_reference = source_language_name ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_named_entity DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.

    "! Call once to create a new named entity
    "! @parameter exists_already_with_id | contains the id if entry already existed
    "! @parameter id | the id in model either if just created or already existing
    METHODS add
      IMPORTING name_group                    TYPE string OPTIONAL
                name                          TYPE string
      EXPORTING VALUE(exists_already_with_id) TYPE i
      RETURNING VALUE(id)                     TYPE i.
    "! Call once to set the parent package
    "! @parameter parent_package | the name of an element of type FAMIX.Package
    METHODS set_parent_package IMPORTING parent_package TYPE string.

  PROTECTED SECTION.

ENDCLASS.

CLASS cl_famix_named_entity IMPLEMENTATION.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

  METHOD set_parent_package.
    g_model->add_reference( elementname       = 'FAMIX.Package'
                            name_of_reference = parent_package
                            attribute_name    = 'parentPackage' ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_parameter DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! Set the parent behavioural entity, either a method or a function
    "! @parameter parent_id | id of parent entity
    METHODS set_parent_behavioural_entity
      IMPORTING
        parent_id TYPE i.
ENDCLASS.

CLASS cl_famix_parameter IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Parameter'.
  ENDMETHOD.

  METHOD set_parent_behavioural_entity.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'parentBehaviouralEntity'
                                            reference_id   = parent_id ).
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = false
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_attribute DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    METHODS store_id
      IMPORTING
        class     TYPE string
        attribute TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                class     TYPE string
                attribute TYPE string
      RETURNING VALUE(id) TYPE i.
    METHODS add REDEFINITION.

    "! set the parent type, for instance the class the method is contained in
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | the name of the parent element
    METHODS set_parent_type
      IMPORTING
        parent_element TYPE string
        parent_name    TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF attribute_id_type,
             class     TYPE string,
             attribute TYPE string,
             id        TYPE i,
           END OF attribute_id_type.
    DATA: g_attribute_ids TYPE HASHED TABLE OF attribute_id_type WITH UNIQUE KEY class attribute.
ENDCLASS.

CLASS cl_famix_attribute IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Attribute'.
  ENDMETHOD.
  METHOD set_parent_type.
    g_model->add_reference( EXPORTING attribute_name    = 'parentType'
                                      elementname       = parent_element
                                      name_of_reference = parent_name ).
  ENDMETHOD.
  METHOD add.
    id = g_model->add_entity( elementname = g_elementname
                              is_named_entity = true
                              can_be_referenced_by_name = false
                              name = name ).
    g_last_used_id = id.
  ENDMETHOD.


  METHOD store_id.
    g_attribute_ids = VALUE #( BASE g_attribute_ids ( id        = g_last_used_id
                                                    class     = class
                                                    attribute = attribute ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE g_attribute_ids ASSIGNING FIELD-SYMBOL(<attribute_id>) WITH TABLE KEY class = class attribute = attribute.
    IF sy-subrc EQ ok.
      id = <attribute_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_container_entity DEFINITION INHERITING FROM cl_famix_named_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the container an element is in
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container | the name of the Container
    METHODS set_container IMPORTING container_element TYPE string
                                    parent_container  TYPE string.
    "! Set the container an element is in using the reference
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container_id | the id of the Container
    METHODS set_container_by_id IMPORTING container_element   TYPE string
                                          parent_container_id TYPE i.
  PROTECTED SECTION.

ENDCLASS.

CLASS cl_famix_container_entity IMPLEMENTATION.

  METHOD set_container.
    g_model->add_reference( EXPORTING elementname       = container_element
                                      name_of_reference = parent_container
                                      attribute_name    = 'container' ).
  ENDMETHOD.

  METHOD set_container_by_id.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'container'
                                            reference_id   = parent_container_id ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_behavioural_entity DEFINITION INHERITING FROM cl_famix_container_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the signature of a method
    "! This might not be relevant for ABAP, but is contained here for completeness
    "! @parameter signature | The signature like myMethod( myParameters, ...)
    METHODS set_signature IMPORTING signature TYPE string.

ENDCLASS.

CLASS cl_famix_behavioural_entity IMPLEMENTATION.

  METHOD set_signature.
    g_model->add_string( EXPORTING attribute_name = 'signature'
                                   string         = signature ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_namespace DEFINITION INHERITING FROM cl_famix_container_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

ENDCLASS.

CLASS cl_famix_namespace IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Namespace'.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_package DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_package IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Package'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_module DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_module IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Module'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_method DEFINITION INHERITING FROM cl_famix_behavioural_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! set the parent type, for instance the class the method is contained in
    "! Provide either parent_name or parent_id
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | optional the name of the parent element
    "! @parameter parent_id | optional the id of the parent element
    METHODS set_parent_type
      IMPORTING
        parent_element TYPE string
        parent_name    TYPE string OPTIONAL
        parent_id      TYPE i OPTIONAL.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    METHODS store_id
      IMPORTING
        class  TYPE string
        method TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                class     TYPE string
                method    TYPE string
      RETURNING VALUE(id) TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_method_id,
             class  TYPE string,
             method TYPE string,
             id     TYPE i,
           END OF ty_method_id.
    DATA: g_method_ids TYPE HASHED TABLE OF ty_method_id WITH UNIQUE KEY class method.
ENDCLASS.

CLASS cl_famix_method IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Method'.
  ENDMETHOD.

  METHOD set_parent_type.
    IF parent_name IS SUPPLIED.
      g_model->add_reference( EXPORTING attribute_name    = 'parentType'
                                        elementname       = parent_element
                                        name_of_reference = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING attribute_name = 'parentType'
                                              reference_id   = parent_id ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( elementname               = g_elementname
                              is_named_entity           = true
                              can_be_referenced_by_name = false
                              name = name ).
    g_last_used_id = id.
  ENDMETHOD.

  METHOD store_id.
    g_method_ids = VALUE #( BASE g_method_ids ( id    = g_last_used_id
                                                class = class method = method ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE g_method_ids ASSIGNING FIELD-SYMBOL(<method_id>) WITH TABLE KEY class = class
                                                                                  method = method.
    IF sy-subrc EQ ok.
      id = <method_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_class DEFINITION INHERITING FROM cl_famix_container_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Set if it is an interface
    METHODS is_interface.
ENDCLASS.

CLASS cl_famix_class IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Class'.
  ENDMETHOD.

  METHOD is_interface.
    g_model->add_boolean( EXPORTING attribute_name = 'isInterface'
                                    is_true        = true ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_association DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.
ENDCLASS.

CLASS cl_famix_association IMPLEMENTATION.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname               = g_elementname
                                        is_named_entity           = false
                                        can_be_referenced_by_name = false ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_access DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Checks that accessor and variable of an access are a new access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS is_new_access
      IMPORTING
                accessor_id   TYPE i
                variable_id   TYPE i
      RETURNING VALUE(is_new) TYPE bool.
    "! defines accessor and variable of an access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS set_accessor_variable_relation
      IMPORTING
        accessor_id TYPE i
        variable_id TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_accessor_variable_id,
             accessor_id TYPE i,
             variable_id TYPE i,
           END OF  ty_accessor_variable_id.
    DATA: g_accessor_variable_ids TYPE HASHED TABLE OF ty_accessor_variable_id WITH UNIQUE KEY accessor_id variable_id.
ENDCLASS.

CLASS cl_famix_access IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Access'.
  ENDMETHOD.

  METHOD set_accessor_variable_relation.
    g_accessor_variable_ids = VALUE #( BASE g_accessor_variable_ids ( accessor_id = accessor_id variable_id = variable_id ) ).
    g_model->add_reference_by_id( EXPORTING attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( EXPORTING attribute_name = 'variable'
                                            reference_id   = variable_id ).
  ENDMETHOD.

  METHOD is_new_access.
    READ TABLE g_accessor_variable_ids TRANSPORTING NO FIELDS WITH TABLE KEY accessor_id = accessor_id variable_id = variable_id.
    IF sy-subrc <> ok.
      is_new = true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_invocation DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS is_new_invocation_to_candidate
      IMPORTING
                sender_id     TYPE i
                candidates_id TYPE i
      RETURNING VALUE(is_new) TYPE bool.

    "! defines an invocation
    "! this also models standard call by functions or methods to components other than attributes
    "! Us this method to reference the receiver using his id
    "! @parameter sender_id | the id of the sender or calling method or function
    "! @parameter candidates_id | the id of the candidate, this is the used method or function of type BehaviouralEntity in case of method or function usage
    "! @parameter receiver_id | optional the id of the receiver or called method or function
    "! @parameter signature | optional a signature
    "! @parameter receiver_source_code | optional a receiver source code
    METHODS set_invocation_by_reference
      IMPORTING
        sender_id            TYPE i
        candidates_id        TYPE i OPTIONAL
        receiver_id          TYPE i OPTIONAL
        signature            TYPE string OPTIONAL
        receiver_source_code TYPE string OPTIONAL.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sender_candidate,
             sender_id     TYPE i,
             candidates_id TYPE i,
           END OF ty_sender_candidate.

    DATA g_sender_candidates TYPE HASHED TABLE OF ty_sender_candidate WITH UNIQUE KEY sender_id candidates_id.

ENDCLASS.

CLASS cl_famix_invocation IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Invocation'.
  ENDMETHOD.

  METHOD is_new_invocation_to_candidate.
    READ TABLE g_sender_candidates TRANSPORTING NO FIELDS WITH TABLE KEY sender_id = sender_id candidates_id = candidates_id.
    IF sy-subrc <> ok.
      is_new = true.
    ENDIF.
  ENDMETHOD.

  METHOD set_invocation_by_reference.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'sender'
                                            reference_id   = sender_id ).
    IF candidates_id IS SUPPLIED.
      g_sender_candidates = VALUE #( BASE g_sender_candidates ( sender_id = sender_id candidates_id = candidates_id ) ).
      g_model->add_reference_by_id( EXPORTING attribute_name = 'candidates'
                                              reference_id   = candidates_id ).
    ENDIF.

    IF receiver_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING attribute_name = 'receiver'
                                              reference_id   = receiver_id ).
    ENDIF.
    IF signature IS SUPPLIED.
      g_model->add_string( EXPORTING attribute_name = 'signature'
                                     string         = signature ).
    ENDIF.
    IF receiver_source_code IS SUPPLIED.
      g_model->add_string( EXPORTING attribute_name = 'receiverSourceCode'
                                     string         = receiver_source_code ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_inheritance DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter subclass_element | the FAMIX element of the subclass Type
    "! @parameter subclass_name_group | the name group of the subclass
    "! @parameter subclass_name | the name of the subclass
    "! @parameter superclass_element | the FAMIX element of the subclass Type
    "! @parameter superclass_name_group | the name group
    "! @parameter superclass_name | the name of the subclass of the superclass
    METHODS set_sub_and_super_class
      IMPORTING
        subclass_element      TYPE string
        subclass_name_group   TYPE string
        subclass_name         TYPE string
        superclass_element    TYPE string
        superclass_name_group TYPE string
        superclass_name       TYPE string.

ENDCLASS.

CLASS cl_famix_inheritance IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Inheritance'.
  ENDMETHOD.
  METHOD set_sub_and_super_class.
    g_model->add_reference( EXPORTING attribute_name          = 'subclass'
                                      elementname             = subclass_element
                                      name_group_of_reference = subclass_name_group
                                      name_of_reference       = subclass_name ).
    g_model->add_reference( EXPORTING attribute_name          = 'superclass'
                                      elementname             = superclass_element
                                      name_group_of_reference = superclass_name_group
                                      name_of_reference       = superclass_name ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_reference DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter target_id | the FAMIX id of the target element
    "! @parameter source_id | the FAMIX id of the source element
    METHODS set_target_source
      IMPORTING
        target_id TYPE i
        source_id TYPE i.
ENDCLASS.

CLASS cl_famix_reference IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Reference'.
  ENDMETHOD.

  METHOD set_target_source.

    g_model->add_reference_by_id( EXPORTING attribute_name    = 'target'
                                            reference_id      = target_id ).
    g_model->add_reference_by_id( EXPORTING attribute_name    = 'source'
                                            reference_id       = source_id ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_custom_source_lang DEFINITION INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING name                          TYPE string
                EXPORTING VALUE(exists_already_with_id) TYPE i
                RETURNING VALUE(id)                     TYPE i.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
ENDCLASS.

CLASS cl_famix_custom_source_lang IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_make_demo_model DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS make
      EXPORTING
        mse_model TYPE cl_model=>lines_type.
ENDCLASS.

CLASS cl_make_demo_model IMPLEMENTATION.

  METHOD make.
    DATA(model) = NEW cl_model( ).

    DATA(famix_namespace) = NEW cl_famix_namespace( model ).
    DATA(famix_package) = NEW cl_famix_package( model ).
    DATA(famix_class) = NEW cl_famix_class( model ).
    DATA(famix_method) = NEW cl_famix_method( model ).
    DATA(famix_attribute) = NEW cl_famix_attribute( model ).
    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).

    famix_namespace->add( name = 'aNamespace' ).
    famix_package->add( name = 'aPackage' ).
    famix_package->add( name = 'anotherPackage' ).
    famix_package->set_parent_package( parent_package = 'aPackage' ).
    famix_class->add( name = 'ClassA' ).
    famix_class->set_container( EXPORTING container_element = 'FAMIX.Namespace'
                                          parent_container  = 'aNamespace').
    famix_class->set_parent_package( parent_package = 'aPackage' ).

    famix_method->add( name = 'methodA1' ).
    famix_method->set_signature( signature = 'methodA1()' ).
    famix_method->set_parent_type( parent_element = 'FAMIX.Class'
                                   parent_name    = 'ClassA' ).
    famix_attribute->add( name = 'attributeA1').
    famix_attribute->set_parent_type( parent_element = 'FAMIX.Class'
                                      parent_name    = 'ClassA' ).
    famix_class->add( name = 'ClassB').
    famix_class->set_container( container_element = 'FAMIX.Namespace'
                                parent_container  = 'aNamespace' ).
    famix_class->set_parent_package( parent_package = 'anotherPackage' ).

    famix_inheritance->add( ).
    famix_inheritance->set_sub_and_super_class( EXPORTING subclass_element   = 'FAMIX.Class'
                                                          subclass_name_group = ''
                                                          subclass_name      = 'ClassB'
                                                          superclass_element = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name    = 'ClassA' ).

    model->make_mse( IMPORTING mse_model = mse_model ).
  ENDMETHOD.

ENDCLASS.
******************************************** End Include Z_FAMIX_ABAP *****************************

" include z_sap_2_famix
******************************************** Begin Include Z_SAP_2_FAMIX ****************************

CLASS cl_sap_package DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING
        name TYPE string.
    "! Call once to set the parent package
    METHODS set_parent_package
      IMPORTING
        parent_package TYPE string.
  PRIVATE SECTION.
    DATA: g_famix_package TYPE REF TO cl_famix_package.
ENDCLASS.

CLASS cl_sap_package IMPLEMENTATION.

  METHOD constructor.
    g_famix_package = NEW cl_famix_package( model = model ).
  ENDMETHOD.

  METHOD add.
    g_famix_package->add( name = name ).
  ENDMETHOD.

  METHOD set_parent_package.
    g_famix_package->set_parent_package( parent_package = parent_package ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_sap_class DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING name_group                    TYPE string OPTIONAL
                name                          TYPE string
      EXPORTING VALUE(exists_already_with_id) TYPE i
      RETURNING VALUE(id)                     TYPE i.
    "! Specify the parent program for a local class
    METHODS set_parent_program
      IMPORTING
        sap_program TYPE string.
    METHODS set_parent_package
      IMPORTING
        parent_package TYPE string.
    METHODS is_interface.
  PRIVATE SECTION.
    DATA: g_famix_class TYPE REF TO cl_famix_class.
ENDCLASS.

CLASS cl_sap_class IMPLEMENTATION.
  METHOD constructor.
    g_famix_class = NEW cl_famix_class( model = model ).
  ENDMETHOD.

  METHOD add.
    id = g_famix_class->add( EXPORTING name_group             = name_group
                                       name                   = name
                             IMPORTING exists_already_with_id = exists_already_with_id ).
  ENDMETHOD.

  METHOD set_parent_program.

    " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

    g_famix_class->set_container( EXPORTING container_element = 'FAMIX.Module'
                                            parent_container  = sap_program ).
  ENDMETHOD.

  METHOD set_parent_package.
    g_famix_class->set_parent_package( parent_package = parent_package ).
  ENDMETHOD.


  METHOD is_interface.
    g_famix_class->is_interface( ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_sap_program DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add
      IMPORTING
        name      TYPE string
      RETURNING
        VALUE(id) TYPE i.
    "! Call once to set the parent package of a program
    METHODS set_parent_package
      IMPORTING
        parent_package TYPE string.
  PRIVATE SECTION.
    DATA g_famix_module TYPE REF TO cl_famix_module.
ENDCLASS.

CLASS cl_sap_program IMPLEMENTATION.

  METHOD constructor.
    g_famix_module = NEW cl_famix_module( model = model ).
  ENDMETHOD.


  METHOD add.

    " SAP_2_FAMIX_5     Map program to FAMIX.Module
    id = g_famix_module->add( name = name ).

  ENDMETHOD.


  METHOD set_parent_package.
    g_famix_module->set_parent_package( parent_package = parent_package ).
  ENDMETHOD.

ENDCLASS.



******************************************** End Include Z_SAP_2_FAMIX ******************************

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
         program TYPE string,
       END OF program_type.

TYPES:BEGIN OF class_type,
        class TYPE seoclsname,
      END OF class_type.

TYPES: BEGIN OF inheritance_type,
         clsname    TYPE seometarel-clsname,
         refclsname TYPE seometarel-refclsname,
         reltype    TYPE seometarel-reltype,
       END OF inheritance_type.

CLASS cl_extract_sap DEFINITION.
  PUBLIC SECTION.
    METHODS extract
      EXPORTING
                mse_model           TYPE cl_model=>lines_type
      RETURNING VALUE(nothing_done) TYPE bool.
  PRIVATE SECTION.

    "! Maps the component lists from SAP (table TADIR) to the component list used in this program
    DATA g_tadir_components_mapping TYPE tadir_components_mapping_type.

    CONSTANTS comptype_attribute TYPE seocmptype VALUE '0'.
    CONSTANTS comptype_method TYPE seocmptype VALUE '1'.



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
        VALUE(programs)  TYPE programs_type.
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
    METHODS _determine_inheritances_betwee
      IMPORTING
        famix_inheritance TYPE REF TO cl_famix_inheritance
        existing_classes  TYPE existing_classes_type.
    TYPES: class_components_type   TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.
    METHODS _determine_class_components
      IMPORTING
        existing_classes        TYPE existing_classes_type
      RETURNING
        VALUE(class_components) TYPE class_components_type.
    METHODS _add_to_class_components_to_mo
      IMPORTING
        class_components TYPE class_components_type
        famix_method     TYPE REF TO cl_famix_method
        famix_attribute  TYPE REF TO cl_famix_attribute.
    METHODS _determine_usage_of_methods
      IMPORTING
                sap_class                   TYPE REF TO cl_sap_class
                class_components            TYPE class_components_type
                famix_method                TYPE REF TO cl_famix_method
                famix_attribute             TYPE REF TO cl_famix_attribute
                famix_invocation            TYPE REF TO cl_famix_invocation
                famix_access                TYPE REF TO cl_famix_access
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.
    "! Determine usages for components
    "! If using components are not part of the model, they are either added or replaced by a dummy component
    METHODS _determine_usages
      IMPORTING
                famix_class                 TYPE REF TO cl_sap_class
                class_component             TYPE class_component_type
                famix_method                TYPE REF TO cl_famix_method
                famix_invocation            TYPE REF TO cl_famix_invocation
                famix_access                TYPE REF TO cl_famix_access
                used_id                     TYPE i
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
        select_by_top_package TYPE bool
      EXPORTING
        components_infos      TYPE components_infos_type
        nothing_selected      TYPE bool.

ENDCLASS.

TYPES: BEGIN OF indexed_token_type,
         index TYPE i,
         str   TYPE string,
         row   TYPE token_row,
         col   TYPE token_col,
         type  TYPE token_type,
       END OF indexed_token_type.

TYPES sorted_tokens_type TYPE SORTED TABLE OF indexed_token_type WITH UNIQUE KEY index.

"! Analyze ABAP Statement of type K (Other ABAP key word)
CLASS cl_ep_analyze_other_keyword DEFINITION.
  PUBLIC SECTION.
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
             is_class_stmnt_info TYPE bool,
             class_is_inheriting TYPE bool,
             class_inherits_from TYPE string,
             is_static           TYPE bool,
             name                TYPE string,
           END OF info_type.
    DATA: g_info TYPE info_type READ-ONLY.
  PRIVATE SECTION.
    DATA g_sorted_tokens TYPE sorted_tokens_type.
ENDCLASS.

CLASS cl_ep_analyze_other_keyword IMPLEMENTATION.

  METHOD constructor.
    g_sorted_tokens = sorted_tokens.

  ENDMETHOD.

  METHOD analyze.
    ASSERT statement-type EQ 'K'.
    g_info = VALUE #( ).

    " First Run, what is the keyword
    READ TABLE g_sorted_tokens ASSIGNING FIELD-SYMBOL(<token>) WITH TABLE KEY index = statement-from.
    IF sy-subrc <> ok.
      " TBD Error handling
      " In the moment ignore
      RETURN.
    ENDIF.

    CASE <token>-str.
      WHEN 'CLASS'.
        g_info-is_class_stmnt_info = true.

      WHEN 'ENDCLASS'.
        g_info-statement_type = end_class.
      WHEN 'PUBLIC'.
        g_info-statement_type = start_public.
      WHEN 'PROTECTED'.
        g_info-statement_type = start_protected.
      WHEN 'PRIVATE'.
        g_info-statement_type = start_private.
      WHEN 'METHODS'.
        " info-is_method_stmnt = true.
        g_info-statement_type = method_definition.
      WHEN 'CLASS-METHODS'.
        g_info-statement_type = method_definition.
        g_info-is_static = true.
      WHEN 'METHOD'.
        g_info-statement_type = start_method_implementation.
      WHEN 'ENDMETHOD'.
        g_info-statement_type = end_method_implementation.

      WHEN 'DATA'.
        g_info-statement_type = attribute_definition.
      WHEN 'CLASS-DATA'.
        g_info-statement_type = attribute_definition.
        g_info-is_static = true.
      WHEN OTHERS.
        " TBD
        " Add further, in the moment ignore
        RETURN.
    ENDCASE.

    " Second Run, what is the name
    IF g_info-is_class_stmnt_info EQ true
    OR g_info-statement_type EQ method_definition
    OR g_info-statement_type EQ start_method_implementation
    OR g_info-statement_type EQ attribute_definition.

      DATA(position_of_name) = statement-from + 1.
      READ TABLE g_sorted_tokens ASSIGNING <token> WITH TABLE KEY index = position_of_name.
      IF sy-subrc <> ok.
        " TBD Error handling
        " In the moment ignore
        RETURN.
      ENDIF.

      g_info-name = <token>-str.

      " Third run, further keywords
      IF g_info-is_class_stmnt_info EQ true.
        LOOP AT g_sorted_tokens ASSIGNING <token> WHERE index > position_of_name
                                                       AND index <= statement-to.
          CASE <token>-str.
            WHEN 'DEFINITION'.
              g_info-statement_type = start_class_definition.
            WHEN 'IMPLEMENTATION'.
              g_info-statement_type = start_class_implementation.
            WHEN 'INHERITING'.
              g_info-class_is_inheriting = true.
              DATA(superclass_is_at) = sy-tabix + 2.
              READ TABLE g_sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_superclass_token>) WITH TABLE KEY index = superclass_is_at.
              IF sy-subrc EQ ok.
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

ENDCLASS.


CLASS cl_program_analyzer DEFINITION.
  PUBLIC SECTION.
    METHODS extract
      IMPORTING
        module_reference TYPE i
        program          TYPE progname
      CHANGING
        model            TYPE REF TO cl_model.

ENDCLASS.

CLASS cl_program_analyzer IMPLEMENTATION.

  METHOD extract.
    DATA source TYPE TABLE OF string.
    READ REPORT program INTO source.

    DATA: tokens TYPE STANDARD TABLE OF stokes.

    DATA: sorted_tokens TYPE sorted_tokens_type.

    DATA statements TYPE STANDARD TABLE OF sstmnt.

    SCAN ABAP-SOURCE source TOKENS INTO tokens STATEMENTS INTO statements.

    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<ls_token_2>).
      sorted_tokens = VALUE #( BASE sorted_tokens ( index = sy-tabix
                                                    str   = <ls_token_2>-str
                                                    row   = <ls_token_2>-row
                                                    col   = <ls_token_2>-col
                                                    type  = <ls_token_2>-type ) ).
    ENDLOOP.

    SORT statements BY from.

    IF g_parameter_list_tokens EQ true.
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
             in_class_definition      TYPE bool,
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
             instanciable       TYPE bool,
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
    DATA aok TYPE REF TO cl_ep_analyze_other_keyword.
    aok = NEW cl_ep_analyze_other_keyword( sorted_tokens = sorted_tokens ).

    LOOP AT statements ASSIGNING FIELD-SYMBOL(<statement>).

      token_number = 0.
      CASE <statement>-type.
        WHEN 'K'.

          aok->analyze( statement = <statement> ).
          CASE aok->g_info-statement_type.
            WHEN aok->start_class_definition.
              " SAP_2_FAMIX_28        Determine local classes in programs
              context-in_class_definition = true.
              actual_class_with_model_id-classname = aok->g_info-name.
              classes_with_model_id = VALUE #( BASE classes_with_model_id ( actual_class_with_model_id ) ).
              IF aok->g_info-class_is_inheriting EQ true.
                " SAP_2_FAMIX_37        Determine local inheritances of classes
                inheritances = VALUE #( BASE inheritances ( subclass = actual_class_with_model_id-classname
                                                                  superclass = aok->g_info-class_inherits_from ) ).
              ENDIF.
            WHEN aok->start_public.
              context-in_section = public.
            WHEN aok->start_protected.
              context-in_section = protected.
            WHEN aok->start_private.
              context-in_section = private.
            WHEN aok->end_class.
              context-in_section = none.
              context-in_class_definition = false.
              context-implementation_of_class = VALUE #( ).
            WHEN aok->method_definition.
              " SAP_2_FAMIX_29      Determine local class methods in programs
              IF aok->g_info-is_static EQ true.
                actual_method = VALUE #( classname = actual_class_with_model_id-classname
                                         in_section = context-in_section ).
              ELSE.
                actual_method = VALUE #( classname = actual_class_with_model_id-classname
                                         in_section = context-in_section
                                         instanciable = true ).
              ENDIF.
              actual_method-methodname = aok->g_info-name.
            WHEN aok->start_class_implementation.
              context-implementation_of_class = aok->g_info-name.
            WHEN aok->start_method_implementation.
              context-implementation_of_method = aok->g_info-name.
              IF g_parameter_list_tokens EQ true.
                FORMAT COLOR COL_GROUP.
              ENDIF.
            WHEN aok->end_method_implementation.
              context-implementation_of_method = VALUE #( ).
              IF g_parameter_list_tokens EQ true.
                FORMAT COLOR COL_BACKGROUND.
              ENDIF.
            WHEN OTHERS.

          ENDCASE.

        WHEN OTHERS.

      ENDCASE.

      IF g_parameter_list_tokens EQ true.
        WRITE: / <statement>-type.
        LOOP AT sorted_tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE
            index >= <statement>-from
        AND index <= <statement>-to.
          WRITE: '|', <token>-type, <token>-str.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    " Add local classes to model

    DATA(sap_class) = NEW cl_sap_class( model ).

    LOOP AT classes_with_model_id ASSIGNING FIELD-SYMBOL(<class>).
      " SAP_2_FAMIX_30        Map local classes of programs to FAMIX.Class

      <class>-id_in_model = sap_class->add( EXPORTING name_group = CONV string( program )
                                                        name       = <class>-classname ).

      sap_class->set_parent_program( sap_program = CONV string( program ) ).


    ENDLOOP.

    " Add local methods to model

    DATA(famix_method) = NEW cl_famix_method( model ).

    LOOP AT methods ASSIGNING FIELD-SYMBOL(<method>).
      READ TABLE classes_with_model_id ASSIGNING FIELD-SYMBOL(<class_2>) WITH TABLE KEY classname = <method>-classname.
      <method>-class_id_in_model = <class_2>-id_in_model.

      " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method

      <method>-method_id_in_model = famix_method->add( EXPORTING name_group = <class_2>-classname
                                                                 name       = <method>-methodname ).
      " SAP_2_FAMIX_43        Fill the attribut signature of FAMIX.METHOD with the name of the method
      famix_method->set_signature( signature = <method>-methodname ).

      " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class


      famix_method->set_parent_type( EXPORTING parent_element = 'FAMIX.Class'
                                               parent_id      =  <class_2>-id_in_model ).
    ENDLOOP.

    " Add local inheritances to model

    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).
    LOOP AT inheritances INTO DATA(inheritance).
      " SAP_2_FAMIX_38        Map local inheritances of classes to FAMIX.Inheritance
      famix_inheritance->add( ).
      famix_inheritance->set_sub_and_super_class( EXPORTING subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = CONV #( program )
                                                            subclass_name         = inheritance-subclass
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = CONV #( program )
                                                            superclass_name       = inheritance-superclass ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS cl_extract_sap IMPLEMENTATION.

  METHOD extract.

    TYPES:BEGIN OF package_type,
            devclass TYPE devclass,
          END OF package_type.

    DATA components_infos TYPE components_infos_type.
    DATA new_components_infos TYPE components_infos_type.
    DATA processed_components_infos TYPE components_infos_type.
    DATA classes TYPE STANDARD TABLE OF class_interface_type.
    DATA programs TYPE STANDARD TABLE OF program_type.
    DATA existing_classes TYPE HASHED TABLE OF class_type WITH UNIQUE KEY class.

    DATA class_components TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.

    " Do not use singleton pattern, but define each instance only one time at the start

    DATA(model) = NEW cl_model( ).
    DATA(sap_package) = NEW cl_sap_package( model ).
    DATA(sap_program) = NEW cl_sap_program( model ).
    DATA(sap_class) = NEW cl_sap_class( model ).
    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).
    DATA(famix_method) = NEW cl_famix_method( model ).
    DATA(famix_attribute) = NEW cl_famix_attribute( model ).
    DATA(famix_invocation) = NEW cl_famix_invocation( model ).
    DATA(famix_access) = NEW cl_famix_access( model ).

    " Set TADIR mapping
    g_tadir_components_mapping = VALUE #( ( object = 'CLAS' component = 'GlobClass' )
                                          ( object = 'INTF' component = 'GlobIntf' )
                                          ( object = 'PROG' component = 'ABAPProgramm') ).

    _set_default_language( model ).

    DATA nothing_selected TYPE bool.

    IF g_filter_using_package EQ true.
      DATA(select_by_top_package) = true.
    ELSEIF g_filter_using_name EQ true.
      select_by_top_package = false.
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
    _select_requested_components( EXPORTING  sap_package          = sap_package
                                             select_by_top_package = select_by_top_package
                                             package_to_analyze    = g_parameter_package_to_analyze
                                   IMPORTING components_infos      = components_infos
                                             nothing_selected      = nothing_selected ).



    " Select requested components by complex filters





    IF nothing_selected EQ true.
      nothing_done = true.
      RETURN.
    ENDIF.

    WHILE lines( components_infos ) <> 0.

      _analyze_components( EXPORTING components_infos = components_infos
                           IMPORTING classes          = classes
                                     programs         = programs ).

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

      _determine_inheritances_betwee( famix_inheritance = famix_inheritance
                                      existing_classes  = existing_classes ).

      class_components = _determine_class_components( existing_classes ).

      _add_to_class_components_to_mo( class_components = class_components
                                      famix_method     = famix_method
                                      famix_attribute  = famix_attribute ).

      new_components_infos = _determine_usage_of_methods( sap_class       = sap_class
                                                          class_components  = class_components
                                                          famix_method      = famix_method
                                                          famix_attribute   = famix_attribute
                                                          famix_invocation  = famix_invocation
                                                          famix_access      = famix_access ).

      " Determine package for new components

      " TBD Find more performant solution

      LOOP AT new_components_infos ASSIGNING FIELD-SYMBOL(<component_infos>).
        "READ
        DATA(object) = g_tadir_components_mapping[ KEY comp component = <component_infos>-component ]-object.

        SELECT SINGLE devclass FROM tadir INTO @<component_infos>-package
          WHERE pgmid = 'R3TR'
            AND object = @object
            AND obj_name = @<component_infos>-component_name.

        IF sy-subrc <> ok.
          " TBD
          " Report errors
          DELETE new_components_infos WHERE component = <component_infos>-component
                                        AND component_name = <component_infos>-component_name.
        ENDIF.

      ENDLOOP.

      INSERT LINES OF components_infos INTO TABLE processed_components_infos.

      components_infos = VALUE #( ).


      " SAP_2_FAMIX_48      Allow to select all using objects
      " Fullfilled by adding new_components_infos to components_infos and repeating the analysis in the while loop

      LOOP AT new_components_infos ASSIGNING FIELD-SYMBOL(<component_infos_2>).

        READ TABLE processed_components_infos TRANSPORTING NO FIELDS WITH TABLE KEY component = <component_infos_2>-component component_name = <component_infos_2>-component_name.

        IF sy-subrc <> ok.

          components_infos = VALUE #( BASE components_infos ( <component_infos_2> ) ).

        ENDIF.

      ENDLOOP.

    ENDWHILE.

    model->make_mse( IMPORTING mse_model = mse_model ).

  ENDMETHOD.

  METHOD _determine_usages.

    DATA where_used_name TYPE eu_lname.
    CASE class_component-cmptype.
      WHEN comptype_method.

        " SAP_2_FAMIX_17      Determine usage of class methods by programs and classes
        " SAP_2_FAMIX_18      Determine usage of interface methods by programs and classes

        where_used_name = class_component-clsname && |\\ME:| && class_component-cmpname.
        SELECT * FROM wbcrossgt INTO TABLE @DATA(where_used_components) WHERE otype = 'ME' AND name = @where_used_name.
      WHEN comptype_attribute.

        " SAP_2_FAMIX_19      Determine usage of class attributes by programs and classes
        " SAP_2_FAMIX_20      Determine usage of interface attributes by programs and classes

        where_used_name = class_component-clsname && |\\DA:| && class_component-cmpname.
        SELECT * FROM wbcrossgt INTO TABLE @where_used_components WHERE otype = 'DA' AND name = @where_used_name.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    LOOP AT where_used_components ASSIGNING FIELD-SYMBOL(<where_used_component>).
      SELECT SINGLE * FROM ris_prog_tadir INTO @DATA(ris_prog_tadir_line) WHERE program_name = @<where_used_component>-include.
      IF sy-subrc EQ ok.
        CASE ris_prog_tadir_line-object_type.
          WHEN 'CLAS'.
            " Used by method
            DATA: using_method TYPE string.
            IF ris_prog_tadir_line-method_name IS INITIAL.
              using_method = 'DUMMY'.
            ELSE.
              using_method = CONV string( ris_prog_tadir_line-method_name ).
            ENDIF.


            DATA(using_method_id) = famix_method->get_id( class  = CONV string( ris_prog_tadir_line-object_name )
                                                             method = using_method ).
            IF using_method_id EQ 0.

              IF g_param_usage_outpack_groupd EQ false.

                " Method does not exist, create the class
                " SAP_2_FAMIX_21      If a component is used by a class that is not selected, add this class to the model
                " SAP_2_FAMIX_22      Do not assign classes that included due to usage to a package

                famix_class->add( EXPORTING name = CONV string( ris_prog_tadir_line-object_name )
                                  IMPORTING exists_already_with_id = DATA(exists_already_with_id) ).

                IF exists_already_with_id IS INITIAL.

                  " SAP_2_FAMIX_47      If no dummy class is specified in case a using class is not in the initial selection, analyze this classes also

                  new_components_infos = VALUE #( BASE new_components_infos (  component_name = ris_prog_tadir_line-object_name
                                                                               component   = g_tadir_components_mapping[ object = 'CLAS' ]-component ) ).

                ENDIF.

              ELSE.
                " SAP_2_FAMIX_35        Add a usage to a single dummy class "OTHER_SAP_CLASS" if required by a parameter

                famix_class->add( EXPORTING name = 'OTHER_SAP_CLASS'
                                  IMPORTING exists_already_with_id = exists_already_with_id ).

              ENDIF.

              " Now there is a class, but no duplicate class

              IF g_param_usage_outpack_groupd EQ false.
                using_method_id = famix_method->get_id( class  = CONV string( ris_prog_tadir_line-object_name )
                                                        method = using_method ).
              ELSE.
                using_method_id = famix_method->get_id( class  = 'OTHER_SAP_CLASS'
                                                        method = 'OTHER_SAP_METHOD' ).
              ENDIF.


              IF using_method_id EQ 0.
                IF g_param_usage_outpack_groupd EQ false.
                  " Now also the method is to be created
                  " SAP_2_FAMIX_23       If a component is used by a class that is not selected, add the using methods to the model

                  using_method_id = famix_method->add( EXPORTING name = using_method ).
                  " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
                  famix_method->set_signature( signature = using_method ).
                  famix_method->set_parent_type( EXPORTING parent_element = 'FAMIX.Class'
                                                           parent_name    = CONV string( ris_prog_tadir_line-object_name ) ).
                  famix_method->store_id( class  = CONV string( ris_prog_tadir_line-object_name )
                                          method = using_method ).
                ELSE.

                  " SAP_2_FAMIX_36        Add a usage to a single dummy method "OTHER_SAP_METHOD" if required by a parameter

                  using_method_id = famix_method->add( EXPORTING name = 'OTHER_SAP_METHOD' ).
                  " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
                  famix_method->set_signature( signature = 'OTHER_SAP_METHOD' ).
                  famix_method->set_parent_type( EXPORTING parent_element = 'FAMIX.Class'
                                                           parent_name    = 'OTHER_SAP_CLASS' ).
                  famix_method->store_id( class  = 'OTHER_SAP_CLASS'
                                          method = 'OTHER_SAP_METHOD' ).
                ENDIF.
              ENDIF.

            ENDIF.

            CASE class_component-cmptype.
              WHEN comptype_method.

                " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
                " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
                IF famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method_id
                                                                     candidates_id = used_id ).
                  famix_invocation->add( ).
                  famix_invocation->set_invocation_by_reference( EXPORTING sender_id     = using_method_id
                                                                           candidates_id = used_id
                                                                           signature     = 'DUMMY' ).
                ENDIF.
              WHEN comptype_attribute.
                " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
                " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

                IF famix_access->is_new_access( accessor_id = using_method_id
                                                variable_id = used_id ).
                  famix_access->add( ).
                  famix_access->set_accessor_variable_relation( EXPORTING accessor_id = using_method_id
                                                                          variable_id = used_id ).
                ENDIF.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.


          WHEN OTHERS.
            " TBD Implement other usages
        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_default_language.

    " Set default language

    DATA(famix_custom_source_language) = NEW cl_famix_custom_source_lang( model ).

    famix_custom_source_language->add( name = 'ABAP' ).

    " Do not assign any entities to ABAP, because otherwise this will not be the default language anymore
    " So do not do this for ABAP, but maybe for another language
    " famix_package->set_declared_source_language( EXPORTING source_language_element = 'FAMIX.CustomSourceLanguage'
    "                                                        source_language_name    = 'ABAP' ).

  ENDMETHOD.


  METHOD _determine_packages_to_analyze.

    " Determine packages to analyze

    "! A temporal helper table used to find all packages (development classes) in the selection
    DATA temp_packages_to_search TYPE STANDARD TABLE OF package_type.

    sap_package->add( name = CONV string( package_first-devclass ) ).

    INSERT VALUE package_type( devclass = package_first-devclass ) INTO TABLE processed_packages.

    temp_packages_to_search = VALUE #( ( devclass = g_parameter_package_to_analyze ) ).
    WHILE temp_packages_to_search IS NOT INITIAL.
      IF temp_packages_to_search IS NOT INITIAL.
        SELECT devclass, parentcl FROM tdevc INTO TABLE @DATA(packages)
         FOR ALL ENTRIES IN @temp_packages_to_search WHERE parentcl = @temp_packages_to_search-devclass.
      ENDIF.

      temp_packages_to_search = VALUE #( ).

      LOOP AT packages INTO DATA(package).

        INSERT VALUE package_type( devclass = package-devclass ) INTO TABLE processed_packages.
        IF sy-subrc EQ ok.
          " New package
          " Search again
          temp_packages_to_search = VALUE #( BASE temp_packages_to_search ( devclass = package-devclass ) ).
          sap_package->add( name = CONV string( package-devclass ) ).
          sap_package->set_parent_package( parent_package = CONV string( package-parentcl ) ).
        ENDIF.

      ENDLOOP.

      SORT temp_packages_to_search.
      DELETE ADJACENT DUPLICATES FROM temp_packages_to_search.

    ENDWHILE.

  ENDMETHOD.


  METHOD _analyze_components.

    " Loop over all packages to find classes and programms

    " SAP_2_FAMIX_1     Extract classes from Dictionary
    " SAP_2_FAMIX_2     Extract interfaces as FAMIX.Class with attribute isinterface

    MOVE-CORRESPONDING components_infos TO classes.

    LOOP AT components_infos ASSIGNING FIELD-SYMBOL(<component_infos>).

      IF <component_infos>-component EQ 'GlobClass'
      OR <component_infos>-component EQ 'GlobIntf'.

        classes = VALUE #( BASE classes ( obj_name = <component_infos>-component_name ) ).

      ELSE.

        programs = VALUE #( BASE programs ( program = <component_infos>-component_name ) ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _read_all_programs.

    " Read all programs

    " SAP_2_FAMIX_4     Extract programs

    LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).

      DATA(module_reference) = sap_program->add( EXPORTING name = <program>-program ).

      READ TABLE components_infos ASSIGNING FIELD-SYMBOL(<component_infos>) WITH TABLE KEY component = 'ABAPProgramm' component_name = <program>-program.
      ASSERT sy-subrc EQ ok.

      sap_package->add( name  = CONV string( <component_infos>-package ) ).

      sap_program->set_parent_package( parent_package = CONV string( <component_infos>-package ) ).

      IF p_iprog EQ true.

        DATA(program_analyzer) = NEW cl_program_analyzer( ).

        program_analyzer->extract( EXPORTING module_reference = module_reference
                                             program          = CONV #( <program>-program )
                                    CHANGING model            = model ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _add_classes_to_model.

    " Add to model
    LOOP AT existing_classes INTO DATA(existing_class).

      " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
      " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
      sap_class->add( name = CONV string( existing_class-class ) ).

      READ TABLE components_infos ASSIGNING FIELD-SYMBOL(<component_infos>) WITH TABLE KEY component = 'GlobClass' component_name = existing_class-class.
      IF sy-subrc <> ok.
        " It may be an interface
        READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = 'GlobIntf' component_name = existing_class-class.
        ASSERT sy-subrc EQ ok.

      ENDIF.

      sap_package->add( EXPORTING name = CONV string( <component_infos>-package ) ).

      sap_class->set_parent_package( parent_package = CONV string( <component_infos>-package ) ).
      IF <component_infos>-component EQ 'GlobIntf'.
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        sap_class->is_interface( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _determine_inheritances_betwee.

    " Determine inheritances between selected classes

    DATA: inheritances TYPE STANDARD TABLE OF  inheritance_type.

    IF existing_classes IS NOT INITIAL.
      SELECT clsname, refclsname, reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE @inheritances
        FOR ALL ENTRIES IN @existing_classes WHERE clsname = @existing_classes-class
                                               AND version = 1.
    ENDIF.

    " Delete all inheritances where superclass is not in selected packages
    LOOP AT inheritances INTO DATA(inheritance).
      READ TABLE existing_classes TRANSPORTING NO FIELDS WITH TABLE KEY class = inheritance-refclsname.
      IF sy-subrc <> ok.
        DELETE inheritances.
      ENDIF.
    ENDLOOP.

    " Add inheritances to model
    LOOP AT inheritances INTO DATA(inheritance_2).
      CASE inheritance_2-reltype.
        WHEN 2.
          " Inheritance
          " SAP_2_FAMIX_39     Map all inheritances between classes in selected packages to FAMIX.Inheritance
          famix_inheritance->add( ).
          famix_inheritance->set_sub_and_super_class( EXPORTING subclass_element      = 'FAMIX.Class'
                                                                subclass_name_group   = ''
                                                                subclass_name         = CONV #( inheritance_2-clsname )
                                                                superclass_element    = 'FAMIX.Class'
                                                                superclass_name_group = ''
                                                                superclass_name       = CONV #( inheritance_2-refclsname ) ).
        WHEN 1.
          " Interface implementation
          " SAP_2_FAMIX_40        Map all interface implementations of interfaces in selected packages by classes of selected packages by FAMIX.Inheritance

          famix_inheritance->add( ).
          famix_inheritance->set_sub_and_super_class( EXPORTING subclass_element      = 'FAMIX.Class'
                                                                subclass_name_group   = ''
                                                                subclass_name         = CONV #( inheritance_2-clsname )
                                                                superclass_element    = 'FAMIX.Class'
                                                                superclass_name_group = ''
                                                                superclass_name       = CONV #( inheritance_2-refclsname ) ).

        WHEN 0.
          " Interface composition     (i COMPRISING i_ref)
          " TBD
        WHEN 5.
          " Enhancement            ( c enhances c_ref)
          " TBD
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD _determine_class_components.

    " Determine class components

    " SAP_2_FAMIX_9         Extract methods of classes
    " SAP_2_FAMIX_10        Extract methods of interfaces
    " SAP_2_FAMIX_11        Extract attributes of classes
    " SAP_2_FAMIX_12        Extract attributes of interfaces

    IF existing_classes IS NOT INITIAL.
      "
      SELECT clsname, cmpname, cmptype FROM seocompo INTO TABLE @class_components
        FOR ALL ENTRIES IN @existing_classes
        WHERE
          clsname = @existing_classes-class.

    ENDIF.

  ENDMETHOD.


  METHOD _add_to_class_components_to_mo.

    " Add to class components to model

    LOOP AT class_components INTO DATA(class_component).

      CASE class_component-cmptype.
        WHEN comptype_attribute. "Attribute

          " SAP_2_FAMIX_13        Mapp attributes of classes to FAMIX.Attribute
          " SAP_2_FAMIX_14        Mapp attributes of interfaces to FAMIX.Attribute

          DATA(existing_id) = famix_attribute->get_id( EXPORTING class     = CONV string( class_component-clsname )
                                                                 attribute = CONV string( class_component-cmpname ) ).
          IF existing_id EQ not_found.

            famix_attribute->add( name = CONV string( class_component-cmpname ) ).
            famix_attribute->set_parent_type(
              EXPORTING
                parent_element = 'FAMIX.Class'
                parent_name    = CONV string( class_component-clsname ) ).
            famix_attribute->store_id( EXPORTING class     = CONV string( class_component-clsname )
                                                 attribute = CONV string( class_component-cmpname ) ).

          ENDIF.

        WHEN comptype_method. "Method

          " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
          " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method

          existing_id = famix_method->get_id( class  = CONV string( class_component-clsname )
                                              method = CONV string( class_component-cmpname ) ).

          IF existing_id EQ not_found.

            famix_method->add( name = CONV string( class_component-cmpname ) ).
            " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
            " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
            famix_method->set_signature( signature = CONV string( class_component-cmpname ) ).
            famix_method->set_parent_type(
              EXPORTING
                parent_element = 'FAMIX.Class'
                parent_name    = CONV string( class_component-clsname ) ).
            famix_method->store_id( EXPORTING class  = CONV string( class_component-clsname )
                                              method = CONV string( class_component-cmpname ) ).
          ENDIF.
        WHEN 2. "Event
        WHEN 3. "Type
        WHEN OTHERS.
          " TBD Warn

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _determine_usage_of_methods.

    DATA class_component TYPE class_component_type.

    " Determine usage of methods

    LOOP AT class_components INTO class_component WHERE cmptype = comptype_attribute  " Methods
                                                     OR cmptype = comptype_method. "Attributes

      CASE class_component-cmptype.
        WHEN comptype_method.
          DATA(used_id) = famix_method->get_id( class  = CONV string( class_component-clsname )
                                                method = CONV string( class_component-cmpname ) ).

        WHEN comptype_attribute.
          used_id = famix_attribute->get_id( class     = CONV string( class_component-clsname )
                                             attribute = CONV string( class_component-cmpname ) ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      INSERT LINES OF _determine_usages( famix_class      = sap_class
                                         class_component  = class_component
                                         famix_method     = famix_method
                                         famix_invocation = famix_invocation
                                         famix_access     = famix_access
                                         used_id          = used_id ) INTO TABLE new_components_infos.

    ENDLOOP.

  ENDMETHOD.


  METHOD _read_all_classes.

    " Read all classes

    " Determine existing classes
    IF classes IS NOT INITIAL.
      SELECT clsname AS class FROM seoclass INTO TABLE @existing_classes FOR ALL ENTRIES IN @classes
        WHERE
          clsname = @classes-obj_name.
    ENDIF.

  ENDMETHOD.

  METHOD _select_requested_components.

    DATA first_package TYPE tdevc.
    DATA processed_packages TYPE cl_extract_sap=>processed_packages_type.
    DATA object TYPE trobjtype.

    IF select_by_top_package EQ true.

      " Select components in package and sub package
      " SAP_2_FAMIX_3     Select all components in a package and the sub packages of this package

      SELECT SINGLE devclass, parentcl FROM tdevc INTO @first_package WHERE devclass = @package_to_analyze.
      IF sy-subrc <> ok.
        WRITE: 'Package does not exist: ', package_to_analyze.
        nothing_selected  = true.
      ENDIF.

      processed_packages = _determine_packages_to_analyze( sap_package = sap_package
                                                           package_first = first_package ).

    ENDIF.

    IF   select_by_top_package EQ false
      OR processed_packages IS NOT INITIAL.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            IF p_clas EQ true.
              object = 'CLAS'.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 2.
            IF p_intf EQ true.
              object = 'INTF'.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 3.
            IF p_prog EQ true.
              object = 'PROG'.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        IF select_by_top_package EQ true.
          SELECT obj_name, object, devclass FROM tadir INTO @DATA(tadir_component) FOR ALL ENTRIES IN @processed_packages
            WHERE pgmid = 'R3TR'
              AND object = @object
              AND devclass = @processed_packages-devclass.

            components_infos = VALUE #( BASE components_infos ( component = g_tadir_components_mapping[ object = tadir_component-object ]-component
                                                                component_name = tadir_component-obj_name
                                                                package = tadir_component-devclass ) ).

          ENDSELECT.
        ELSE.
          SELECT obj_name, object, devclass FROM tadir INTO @tadir_component
            WHERE pgmid = 'R3TR'
              AND object = @object
              AND obj_name IN @s_compsn
              AND devclass IN @s_pack.

            components_infos = VALUE #( BASE components_infos ( component = g_tadir_components_mapping[ object = tadir_component-object ]-component
                                                                component_name = tadir_component-obj_name
                                                                package = tadir_component-devclass ) ).

          ENDSELECT.
        ENDIF.
      ENDDO.
    ENDIF.

    IF lines( components_infos ) EQ 0.
      WRITE: 'Nothing selected '.
      nothing_selected  = true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: mse_model TYPE cl_model=>lines_type.
  IF g_parameter_extract_from_sap EQ false.
    cl_make_demo_model=>make( IMPORTING mse_model = mse_model ).
  ELSE.
    DATA(sap_extractor) = NEW cl_extract_sap( ).
    DATA(nothing_done) = sap_extractor->extract( IMPORTING mse_model = mse_model ).
  ENDIF.

  IF nothing_done EQ true.
    RETURN.
  ENDIF.

  DATA(model_outputer) = NEW cl_output_model( ).
  model_outputer->make( mse_model = mse_model ).