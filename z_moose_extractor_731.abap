*&---------------------------------------------------------------------*
*& Report  Z_MOOSE
*&---------------------------------------------------------------------*

report z_moose no standard page heading.
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
"! Last activation:
"! 16.02.2016 21:44 issue20 Rainer Winkler
"!
*REPORT yrw1_moose_extractor.
tables tadir. "So that select-options work

"! To not compare sy-subrc to zero, but more readable to ok
constants ok type i value 0.

"! To compare with ID that are returned from methods where the value 0 denotes not found
constants not_found type i value 0.
"! Redefines abap_bool to simplify coding (Not always reading abap_...)
types bool type abap_bool.
constants:
  "! Redefines abap_true to simplify coding (Not always reading abap_...)
  true  type bool value abap_true,
  "! Redefines abap_false to simplify coding (Not always reading abap_...)
  false type bool value abap_false.

selection-screen begin of block block_global_source with frame title text-001.

parameters: p_sap as checkbox default 'X'.
"! Extract from SAP
data g_parameter_extract_from_sap type bool.
g_parameter_extract_from_sap = p_sap.

selection-screen end of block block_global_source.

selection-screen begin of block block_selct_sap_comp with frame title text-002.

parameters: p_clas as checkbox default 'X'.
parameters: p_intf as checkbox default 'X'.
parameters: p_prog as checkbox default 'X'.
parameters: p_iprog as checkbox default ' '. "Internal parts of reports


parameters: rb_fpack radiobutton group rbsl default 'X'.
"! Filter using package
data g_filter_using_package type bool.
g_filter_using_package = rb_fpack.

parameters: p_pack type parentcl default ''.
"! Package to be analyzed
data g_parameter_package_to_analyze type parentcl.
g_parameter_package_to_analyze = p_pack.

parameters: rb_fname radiobutton group rbsl.
"! Filter using name
data g_filter_using_name type bool.
g_filter_using_name = rb_fname.

select-options s_pack for tadir-devclass.

select-options s_compsn for tadir-obj_name.

selection-screen end of block block_selct_sap_comp.

selection-screen begin of block block_using_comp with frame title text-003.

parameters: p_dm as checkbox default ' '.
"! Usages outside package grouped
"! If false, a recursive search for using components is performed until no further using components are found
data g_param_usage_outpack_groupd type bool.
g_param_usage_outpack_groupd = p_dm.

selection-screen end of block block_using_comp.

selection-screen begin of block block_infos with frame title text-004.

parameters: p_list as checkbox default ' '.
"! List Tokens of selected programs
data g_parameter_list_tokens type bool.
g_parameter_list_tokens = p_list.

selection-screen end of block block_infos.



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

selection-screen begin of block bl_model_settings with frame title text-100.

parameters p_down as checkbox default 'X'.
"! Download model to file
data g_parameter_download_file type bool.
g_parameter_download_file = p_down.
selection-screen end of block bl_model_settings.

" Begin Model
"! Specifies a model.
"! Instanciate only once, otherwise there will be multiple models each containing only part of the informations.
*----------------------------------------------------------------------*
*       CLASS cl_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_model definition.
  public section.

    types: begin of line_type,
             line type string,
           end of line_type.
    types: lines_type type standard table of line_type.

    methods constructor.

    "! Add a named entity
    "! @parameter elementname | The name of the FAMIX Element. Like FAMIX.NamedEntity
    "! @parameter name_group | optional to handle cases where names may be duplicates
    "! @parameter is_named_entity | True if the entity has a name
    "! @parameter can_be_referenced_by_name | True if referencing by name is possible (For this the name has to be unique)
    "! @parameter name | the name of a FAMIX Entity that inherits from FAMIX.NamedEntity leave empty is is_named_entity is false
    "! @parameter exists_already_with_id | only if can_be_referenced_by_name true. Zero if it does not yet exist, otherwise filled with id
    "! @parameter processedid | the id in model either if just created or already existing
    methods add_entity
      importing elementname                   type clike
                name_group                    type clike default ''
                is_named_entity               type bool
                can_be_referenced_by_name     type bool
                name                          type clike optional
      exporting value(exists_already_with_id) type i
                value(processed_id)           type i.

    "! Generates a string with a valid MSE file
    methods make_mse
      exporting
        mse_model type lines_type.

    "! Generates an attribute of type string
    "! @parameter attribute_name | the name of the attribute
    "! @parameter string | The value of the attribute
    methods add_string
      importing
        attribute_name type clike
        string         type clike.

    "! Generates an attribute of type reference using a name
    "! @parameter attribute_name | the name of the attribute
    "! @parameter elementname | the element type of the reference
    "! @parameter name_of_reference | the reference
    methods add_reference
      importing
        attribute_name          type clike
        elementname             type clike
        name_group_of_reference type clike optional
        name_of_reference       type clike.

    "! Generates an attribute of type reference using an id
    "! @parameter attribute_name | the name of the attribute
    "! @parameter reference_id | the id of the reference
    methods add_reference_by_id
      importing
        attribute_name type clike
        reference_id   type i.

    methods add_boolean
      importing
        attribute_name type clike
        is_true        type bool.

  private section.
    types: begin of element_in_model_type,
             id              type i,
             is_named_entity type bool,
             elementname     type string,
           end of element_in_model_type.
    "! A table with all Elements in the model
    data g_elements_in_model type hashed table of element_in_model_type with unique key id.

    types: begin of named_entity_type,
             elementname type string,
             name_group  type string,
             xname       type string,
             id          type i,
           end of named_entity_type.

    "! A table to find IDs using the names
    data g_named_entities type hashed table of named_entity_type with unique key elementname name_group xname.

    types value_type type c length 1.

    "! An attribute where a name is specified
    constants string_value type value_type value 'S'.

    "! An attribute where a reference is specified
    constants reference_value type value_type value 'R'.

    constants boolean_value type value_type value 'B'.

    types: begin of attribute_type,
             id             type i,
             attribute_id   type i,
             attribute_name type string,
             value_type     type value_type,
             string         type string,
             reference      type i,
             boolean        type bool,
           end of attribute_type.

    "! A table with all the attributes of an entity
    data g_attributes type sorted table of attribute_type with unique key id attribute_id.

    "! The ID of processed entity in the model
    data g_processed_id type i.
    "! The ID of any attribute. Unique together with mv_id
    data g_attribute_id type i.


endclass.                    "cl_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_model implementation.

  method constructor.
    g_processed_id = 0.
  endmethod.                    "constructor

  method add_entity.

    field-symbols <ls_name> like line of g_named_entities.
    data ls_named_entity    like line of g_named_entities.

    if can_be_referenced_by_name eq true.

      read table g_named_entities assigning <ls_name>
            with table key elementname = elementname name_group = name_group xname = name.
      if sy-subrc eq ok.
        exists_already_with_id = <ls_name>-id.
        processed_id = <ls_name>-id.
        return.
      endif.

    endif.

    add 1 to g_processed_id.
    g_attribute_id = 0.

    if can_be_referenced_by_name eq true.
      ls_named_entity-elementname = elementname.
      ls_named_entity-name_group  = name_group.
      ls_named_entity-xname       = name.
      ls_named_entity-id          = g_processed_id.
      insert ls_named_entity into table g_named_entities.
*      g_named_entities = VALUE #( BASE g_named_entities ( elementname = elementname name_group = name_group xname = name id = g_processed_id ) ).
    endif.

    data gs_elements_in_model like line of g_elements_in_model.
    gs_elements_in_model-id = g_processed_id.
    gs_elements_in_model-is_named_entity = is_named_entity.
    gs_elements_in_model-elementname = elementname.
    insert gs_elements_in_model into table g_elements_in_model.

*    g_elements_in_model = VALUE #( BASE g_elements_in_model ( id = g_processed_id
*                                                              is_named_entity = is_named_entity
*                                                              elementname = elementname ) ).

    if is_named_entity eq true.
      me->add_string( exporting attribute_name = 'name' string = name ).
    endif.

    processed_id = g_processed_id.

  endmethod.                    "add_entity

  method make_mse.

    " SAP_2_FAMIX_34      Allow to export the model in the .mse Moose format

    data: mse_model_line type line_type.

    mse_model_line-line = |( |.

    sort g_elements_in_model by id.

    data is_first type boolean value true.
    field-symbols <element_in_model> like line of g_elements_in_model.

    loop at g_elements_in_model assigning <element_in_model>.
      if is_first eq false.

*        mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).
*        mse_model_line = VALUE #( ).
        append mse_model_line to mse_model.
        clear mse_model_line.
      endif.

      mse_model_line-line = mse_model_line-line && |(| && <element_in_model>-elementname.
      if <element_in_model>-is_named_entity eq true.

        mse_model_line-line = mse_model_line-line && | (id: | && <element_in_model>-id && | )|.
      endif.
      field-symbols <attribute> like line of g_attributes.
      loop at g_attributes assigning <attribute> where id = <element_in_model>-id.

*        mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).
        append mse_model_line to mse_model.
        mse_model_line-line = |  (| && <attribute>-attribute_name.
        case <attribute>-value_type.
          when string_value.

            mse_model_line-line = mse_model_line-line && | '| && <attribute>-string && |')|.

          when reference_value.

            mse_model_line-line = mse_model_line-line && | (ref: | && <attribute>-reference && |))|.

          when boolean_value.

            case <attribute>-boolean.
              when true.
                mse_model_line-line = mse_model_line-line && | true)|.
              when false.
                mse_model_line-line = mse_model_line-line && | false)|.
              when others.
                assert 1 = 2.
            endcase.

          when others.
            assert 1 = 2.
        endcase.

      endloop.

      mse_model_line-line = mse_model_line-line && |)|.

      is_first = false.
    endloop.

    mse_model_line-line = mse_model_line-line && |)|.
*    mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).
    append mse_model_line to mse_model.

  endmethod.                    "make_mse

  method add_reference.
    field-symbols <named_entity> like line of g_named_entities.

    read table g_named_entities assigning <named_entity> with table key elementname = elementname
                                                                                      name_group = name_group_of_reference
                                                                                      xname = name_of_reference.
    assert sy-subrc eq ok.
    add 1 to g_attribute_id.
*    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
*                                                attribute_id   = g_attribute_id
*                                                attribute_name = attribute_name
*                                                value_type     = reference_value
*                                                reference      = <named_entity>-id ) ).
    data gs_attribute like line of g_attributes.
    gs_attribute-id             = g_processed_id.
    gs_attribute-attribute_id   = g_attribute_id.
    gs_attribute-attribute_name = attribute_name.
    gs_attribute-value_type     = reference_value.
    gs_attribute-reference      = <named_entity>-id.
    append gs_attribute to g_attributes.

  endmethod.                    "add_reference

  method add_reference_by_id.
    data gs_attribute like line of g_attributes.

    add 1 to g_attribute_id.
*    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
*                                                attribute_id   = g_attribute_id
*                                                attribute_name = attribute_name
*                                                value_type     = reference_value
*                                                reference      = reference_id ) ).
    gs_attribute-id             = g_processed_id.
    gs_attribute-attribute_id   = g_attribute_id.
    gs_attribute-attribute_name = attribute_name.
    gs_attribute-value_type     = reference_value.
    gs_attribute-reference      = reference_id.
    append gs_attribute to g_attributes.

  endmethod.                    "add_reference_by_id

  method add_string.
    data gs_attribute like line of g_attributes.
    add 1 to g_attribute_id.
*    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id
*                                                attribute_id   = g_attribute_id
*                                                attribute_name = attribute_name
*                                                value_type     = string_value
*                                                string         = string ) ).

    gs_attribute-id             = g_processed_id.
    gs_attribute-attribute_id   = g_attribute_id.
    gs_attribute-attribute_name = attribute_name.
    gs_attribute-value_type     = string_value.
    gs_attribute-string         = string.
    append gs_attribute to g_attributes.

  endmethod.                    "add_string

  method add_boolean.
    data gs_attribute like line of g_attributes.
    add 1 to g_attribute_id.
*    g_attributes = value #( base g_attributes ( ID             = g_processed_id
*                                                attribute_id   = g_attribute_id
*                                                attribute_name = attribute_name
*                                                value_type     = boolean_value
*                                                boolean        = is_true ) ).
    gs_attribute-id             = g_processed_id.
    gs_attribute-attribute_id   = g_attribute_id.
    gs_attribute-attribute_name = attribute_name.
    gs_attribute-value_type     = boolean_value.
    gs_attribute-boolean        = is_true.
    append gs_attribute to g_attributes.


  endmethod.                    "add_boolean

endclass.                    "cl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_output_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_output_model definition.
  public section.
    methods make
      importing
        mse_model type cl_model=>lines_type.
endclass.                    "cl_output_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_output_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_output_model implementation.

  method make.
    " Download the file

    data: filename    type string,
          pathname    type string,
          fullpath    type string,
          user_action type i.

    if g_parameter_download_file eq true.

      cl_gui_frontend_services=>file_save_dialog( exporting default_extension = 'mse'
                                                  changing  filename    = filename       " File Name to Save
                                                            path        = pathname       " Path to File
                                                            fullpath    = fullpath       " Path + File Name
                                                            user_action = user_action ). " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)

      if user_action = cl_gui_frontend_services=>action_cancel.
        write: / 'Canceled by user'.
      else.

        call function 'GUI_DOWNLOAD'
          exporting
            filename = fullpath
          tables
            data_tab = mse_model.

      endif.

    endif.
    field-symbols <mse_model_line> like line of mse_model.
    loop at mse_model assigning <mse_model_line>.
      write: / <mse_model_line>-line.
    endloop.
  endmethod.                    "make

endclass.                    "cl_output_model IMPLEMENTATION
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

class cl_famix_entity definition abstract.
  public section.
    methods constructor importing model type ref to cl_model.
  protected section.
    data g_elementname type string.
    data g_model type ref to cl_model.
    data g_last_used_id type i.
endclass.                    "cl_famix_entity DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_entity IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_entity implementation.

  method constructor.
    g_model = model.
  endmethod.                    "constructor

endclass.                    "cl_famix_entity IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_sourced_entity DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_sourced_entity definition abstract inheriting from cl_famix_entity.
  public section.
    "! Declare source language
    "! @parameter source_language_element | the FAMIX element of the source language
    "! @parameter source_language_name | the name of the source language
    methods set_declared_source_language
      importing
        source_language_element type clike
        source_language_name    type clike.
endclass.                    "cl_famix_sourced_entity DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_sourced_entity IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_sourced_entity implementation.

  method set_declared_source_language.
    g_model->add_reference( exporting attribute_name    = 'declaredSourceLanguage'
                                      elementname       = source_language_element
                                      name_of_reference = source_language_name ).
  endmethod.                    "set_declared_source_language

endclass.                    "cl_famix_sourced_entity IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_named_entity DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_named_entity definition inheriting from cl_famix_sourced_entity abstract.
  public section.

    "! Call once to create a new named entity
    "! @parameter exists_already_with_id | contains the id if entry already existed
    "! @parameter id | the id in model either if just created or already existing
    methods add
      importing name_group                    type clike optional
                name                          type clike
      exporting value(exists_already_with_id) type i
                value(id)                     type i.
    "! Call once to set the parent package
    "! @parameter parent_package | the name of an element of type FAMIX.Package
    methods set_parent_package importing parent_package type clike.

  protected section.

endclass.                    "cl_famix_named_entity DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_named_entity IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_named_entity implementation.

  method add.
    g_model->add_entity( exporting elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name_group = name_group
                                        name = name
                              importing exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

  method set_parent_package.
    g_model->add_reference( elementname       = 'FAMIX.Package'
                            name_of_reference = parent_package
                            attribute_name    = 'parentPackage' ).
  endmethod.                    "set_parent_package

endclass.                    "cl_famix_named_entity IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_parameter DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_parameter definition inheriting from cl_famix_named_entity.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add redefinition.
    "! Set the parent behavioural entity, either a method or a function
    "! @parameter parent_id | id of parent entity
    methods set_parent_behavioural_entity
      importing
        parent_id type i.
endclass.                    "cl_famix_parameter DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_parameter IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_parameter implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Parameter'.
  endmethod.                    "constructor

  method set_parent_behavioural_entity.
    g_model->add_reference_by_id( exporting attribute_name = 'parentBehaviouralEntity'
                                            reference_id   = parent_id ).
  endmethod.                    "set_parent_behavioural_entity

  method add.
    g_model->add_entity( exporting elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = false
                                        name = name
                              importing exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

endclass.                    "cl_famix_parameter IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_attribute DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_attribute definition inheriting from cl_famix_named_entity.
  public section.
    methods constructor importing model type ref to cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    methods store_id
      importing
        class     type clike
        attribute type clike.
    "! Returns the ID for a given attribute of a class
    "! Returns 0 if the attribute is not known
    "! @parameter class | the class of the attribute
    "! @parameter attribute | the attribute name
    "! @parameter id | the ID of the element
    methods get_id
      importing
                class     type clike
                attribute type clike
      returning value(id) type i.
    methods add redefinition.

    "! set the parent type, for instance the class the method is contained in
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | the name of the parent element
    methods set_parent_type
      importing
        parent_element type clike
        parent_name    type clike.
  private section.
    types: begin of attribute_id_type,
             class     type string,
             attribute type string,
             id        type i,
           end of attribute_id_type.
    data: g_attribute_ids type hashed table of attribute_id_type with unique key class attribute.
endclass.                    "cl_famix_attribute DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_attribute IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_attribute implementation.
  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Attribute'.
  endmethod.                    "constructor
  method set_parent_type.
    g_model->add_reference( exporting attribute_name    = 'parentType'
                                      elementname       = parent_element
                                      name_of_reference = parent_name ).
  endmethod.                    "set_parent_type
  method add.
    g_model->add_entity(
               exporting elementname = g_elementname
                         is_named_entity = true
                         can_be_referenced_by_name = false
                         name = name
               importing processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add


  method store_id.

    data gs_attribute_id like line of g_attribute_ids.

*    g_attribute_ids = value #( base g_attribute_ids ( ID        = g_last_used_id
*                                                    CLASS     = CLASS
*                                                    attribute = attribute ) ).
    gs_attribute_id-id = g_last_used_id.
    gs_attribute_id-class = class.
    gs_attribute_id-attribute = attribute.
    insert gs_attribute_id into table g_attribute_ids.

  endmethod.                    "store_id

  method get_id.
    field-symbols <attribute_id> like line of g_attribute_ids.

    read table g_attribute_ids assigning <attribute_id> with table key class = class attribute = attribute.
    if sy-subrc eq ok.
      id = <attribute_id>-id.
    else.
      id = 0.
    endif.
  endmethod.                    "get_id

endclass.                    "cl_famix_attribute IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_container_entity DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_container_entity definition inheriting from cl_famix_named_entity abstract.
  public section.
    "! Set the container an element is in
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container | the name of the Container
    methods set_container importing container_element type string
                                    parent_container  type string.
    "! Set the container an element is in using the reference
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container_id | the id of the Container
    methods set_container_by_id importing container_element   type string
                                          parent_container_id type i.
  protected section.

endclass.                    "cl_famix_container_entity DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_container_entity IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_container_entity implementation.

  method set_container.
    g_model->add_reference( exporting elementname       = container_element
                                      name_of_reference = parent_container
                                      attribute_name    = 'container' ).
  endmethod.                    "set_container

  method set_container_by_id.
    g_model->add_reference_by_id( exporting attribute_name = 'container'
                                            reference_id   = parent_container_id ).

  endmethod.                    "set_container_by_id

endclass.                    "cl_famix_container_entity IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_behavioural_entity DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_behavioural_entity definition inheriting from cl_famix_container_entity abstract.
  public section.
    "! Set the signature of a method
    "! This might not be relevant for ABAP, but is contained here for completeness
    "! @parameter signature | The signature like myMethod( myParameters, ...)
    methods set_signature importing signature type clike.

endclass.                    "cl_famix_behavioural_entity DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_behavioural_entity IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_behavioural_entity implementation.

  method set_signature.
    g_model->add_string( exporting attribute_name = 'signature'
                                   string         = signature ).
  endmethod.                    "set_signature

endclass.                    "cl_famix_behavioural_entity IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_namespace DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_namespace definition inheriting from cl_famix_container_entity.

  public section.
    methods constructor importing model type ref to cl_model.

endclass.                    "cl_famix_namespace DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_namespace IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_namespace implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Namespace'.
  endmethod.                    "constructor

endclass.                    "cl_famix_namespace IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_package DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_package definition inheriting from cl_famix_named_entity.

  public section.
    methods constructor importing model type ref to cl_model.

    methods add redefinition.

endclass.                    "cl_famix_package DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_package IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_package implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Package'.
  endmethod.                    "constructor

  method add.
    g_model->add_entity( exporting elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              importing exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

endclass.                    "cl_famix_package IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_module DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_module definition inheriting from cl_famix_named_entity.

  public section.
    methods constructor importing model type ref to cl_model.

    methods add redefinition.

endclass.                    "cl_famix_module DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_module IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_module implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Module'.
  endmethod.                    "constructor

  method add.
    g_model->add_entity( exporting elementname = g_elementname
                                   is_named_entity = true
                                   can_be_referenced_by_name = true
                                   name = name
                              importing exists_already_with_id = exists_already_with_id
                                   processed_id = id  ).
    g_last_used_id = id.
  endmethod.                    "add

endclass.                    "cl_famix_module IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_method DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_method definition inheriting from cl_famix_behavioural_entity.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add redefinition.
    "! set the parent type, for instance the class the method is contained in
    "! Provide either parent_name or parent_id
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | optional the name of the parent element
    "! @parameter parent_id | optional the id of the parent element
    methods set_parent_type
      importing
        parent_element type clike
        parent_name    type clike optional
        parent_id      type i optional.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    methods store_id
      importing
        class  type clike
        method type clike.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    methods get_id
      importing
                class     type clike
                method    type clike
      returning value(id) type i.
  private section.
    types: begin of ty_method_id,
             class  type string,
             method type string,
             id     type i,
           end of ty_method_id.
    data: g_method_ids type hashed table of ty_method_id with unique key class method.
endclass.                    "cl_famix_method DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_method IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_method implementation.
  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Method'.
  endmethod.                    "constructor

  method set_parent_type.
    if parent_name is supplied.
      g_model->add_reference( exporting attribute_name    = 'parentType'
                                        elementname       = parent_element
                                        name_of_reference = parent_name ).
    elseif parent_id is supplied.
      g_model->add_reference_by_id( exporting attribute_name = 'parentType'
                                              reference_id   = parent_id ).
    else.
      assert 1 = 2.
    endif.
  endmethod.                    "set_parent_type

  method add.
    g_model->add_entity(
                    exporting elementname               = g_elementname
                              is_named_entity           = true
                              can_be_referenced_by_name = false
                              name = name
                    importing processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

  method store_id.

    data gs_method_id like line of g_method_ids.

*    g_method_ids = value #( base g_method_ids ( ID    = g_last_used_id
*                                                CLASS = CLASS method = METHOD ) ).
    gs_method_id-id = g_last_used_id.
    gs_method_id-class = class.
    gs_method_id-method = method.
    insert gs_method_id into table g_method_ids.

  endmethod.                    "store_id

  method get_id.
    field-symbols <method_id> like line of g_method_ids.

    read table g_method_ids assigning <method_id> with table key class = class
                                                               method = method.
    if sy-subrc eq ok.
      id = <method_id>-id.
    else.
      id = 0.
    endif.
  endmethod.                    "get_id

endclass.                    "cl_famix_method IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_class definition inheriting from cl_famix_container_entity.
  public section.
    methods constructor importing model type ref to cl_model.
    "! Set if it is an interface
    methods is_interface.
endclass.                    "cl_famix_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_class implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Class'.
  endmethod.                    "constructor

  method is_interface.
    g_model->add_boolean( exporting attribute_name = 'isInterface'
                                    is_true        = true ).
  endmethod.                    "is_interface

endclass.                    "cl_famix_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_association DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_association definition inheriting from cl_famix_sourced_entity abstract.
  public section.
    methods add
      returning value(id) type i.
endclass.                    "cl_famix_association DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_association IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_association implementation.

  method add.
    g_model->add_entity( exporting elementname               = g_elementname
                                        is_named_entity           = false
                                        can_be_referenced_by_name = false
                                        importing processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

endclass.                    "cl_famix_association IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_access DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_access definition inheriting from cl_famix_association.
  public section.
    methods constructor importing model type ref to cl_model.
    "! Checks that accessor and variable of an access are a new access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    methods is_new_access
      importing
                accessor_id   type i
                variable_id   type i
      returning value(is_new) type bool.
    "! defines accessor and variable of an access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    methods set_accessor_variable_relation
      importing
        accessor_id type i
        variable_id type i.
  private section.
    types: begin of ty_accessor_variable_id,
             accessor_id type i,
             variable_id type i,
           end of  ty_accessor_variable_id.
    data: g_accessor_variable_ids type hashed table of ty_accessor_variable_id with unique key accessor_id variable_id.
endclass.                    "cl_famix_access DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_access IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_access implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Access'.
  endmethod.                    "constructor

  method set_accessor_variable_relation.
*    g_accessor_variable_ids = value #( base g_accessor_variable_ids ( accessor_id = accessor_id variable_id = variable_id ) ).
    data gs_accessor_id like line of g_accessor_variable_ids.
    gs_accessor_id-accessor_id = accessor_id.
    gs_accessor_id-variable_id = variable_id.
    insert gs_accessor_id into table g_accessor_variable_ids.

    g_model->add_reference_by_id( exporting attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( exporting attribute_name = 'variable'
                                            reference_id   = variable_id ).
  endmethod.                    "set_accessor_variable_relation

  method is_new_access.
    read table g_accessor_variable_ids transporting no fields with table key accessor_id = accessor_id variable_id = variable_id.
    if sy-subrc <> ok.
      is_new = true.
    endif.
  endmethod.                    "is_new_access

endclass.                    "cl_famix_access IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_invocation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_invocation definition inheriting from cl_famix_association.
  public section.
    methods constructor importing model type ref to cl_model.

    methods is_new_invocation_to_candidate
      importing
                sender_id     type i
                candidates_id type i
      returning value(is_new) type bool.

    "! defines an invocation
    "! this also models standard call by functions or methods to components other than attributes
    "! Us this method to reference the receiver using his id
    "! @parameter sender_id | the id of the sender or calling method or function
    "! @parameter candidates_id | the id of the candidate, this is the used method or function of type BehaviouralEntity in case of method or function usage
    "! @parameter receiver_id | optional the id of the receiver or called method or function
    "! @parameter signature | optional a signature
    "! @parameter receiver_source_code | optional a receiver source code
    methods set_invocation_by_reference
      importing
        sender_id            type i
        candidates_id        type i optional
        receiver_id          type i optional
        signature            type string optional
        receiver_source_code type string optional.

  private section.
    types: begin of ty_sender_candidate,
             sender_id     type i,
             candidates_id type i,
           end of ty_sender_candidate.

    data g_sender_candidates type hashed table of ty_sender_candidate with unique key sender_id candidates_id.

endclass.                    "cl_famix_invocation DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_invocation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_invocation implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Invocation'.
  endmethod.                    "constructor

  method is_new_invocation_to_candidate.
    read table g_sender_candidates transporting no fields with table key sender_id = sender_id candidates_id = candidates_id.
    if sy-subrc <> ok.
      is_new = true.
    endif.
  endmethod.                    "is_new_invocation_to_candidate

  method set_invocation_by_reference.
    g_model->add_reference_by_id( exporting attribute_name = 'sender'
                                            reference_id   = sender_id ).
    if candidates_id is supplied.
      data gs_sender_candidate like line of g_sender_candidates.
*      g_sender_candidates = value #( base g_sender_candidates ( sender_id = sender_id candidates_id = candidates_id ) ).
      gs_sender_candidate-sender_id = sender_id.
      gs_sender_candidate-candidates_id = candidates_id.
      insert gs_sender_candidate into table g_sender_candidates.

      g_model->add_reference_by_id( exporting attribute_name = 'candidates'
                                              reference_id   = candidates_id ).
    endif.

    if receiver_id is supplied.
      g_model->add_reference_by_id( exporting attribute_name = 'receiver'
                                              reference_id   = receiver_id ).
    endif.
    if signature is supplied.
      g_model->add_string( exporting attribute_name = 'signature'
                                     string         = signature ).
    endif.
    if receiver_source_code is supplied.
      g_model->add_string( exporting attribute_name = 'receiverSourceCode'
                                     string         = receiver_source_code ).
    endif.
  endmethod.                    "set_invocation_by_reference

endclass.                    "cl_famix_invocation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_inheritance DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_inheritance definition inheriting from cl_famix_association.
  public section.
    methods constructor importing model type ref to cl_model.
    "! defines an inheritance
    "! @parameter subclass_element | the FAMIX element of the subclass Type
    "! @parameter subclass_name_group | the name group of the subclass
    "! @parameter subclass_name | the name of the subclass
    "! @parameter superclass_element | the FAMIX element of the subclass Type
    "! @parameter superclass_name_group | the name group
    "! @parameter superclass_name | the name of the subclass of the superclass
    methods set_sub_and_super_class
      importing
        subclass_element      type clike
        subclass_name_group   type clike
        subclass_name         type clike
        superclass_element    type clike
        superclass_name_group type clike
        superclass_name       type clike.

endclass.                    "cl_famix_inheritance DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_inheritance IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_inheritance implementation.
  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Inheritance'.
  endmethod.                    "constructor
  method set_sub_and_super_class.
    g_model->add_reference( exporting attribute_name          = 'subclass'
                                      elementname             = subclass_element
                                      name_group_of_reference = subclass_name_group
                                      name_of_reference       = subclass_name ).
    g_model->add_reference( exporting attribute_name          = 'superclass'
                                      elementname             = superclass_element
                                      name_group_of_reference = superclass_name_group
                                      name_of_reference       = superclass_name ).

  endmethod.                    "set_sub_and_super_class

endclass.                    "cl_famix_inheritance IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_reference DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_reference definition inheriting from cl_famix_association.
  public section.
    methods constructor importing model type ref to cl_model.
    "! defines an inheritance
    "! @parameter target_id | the FAMIX id of the target element
    "! @parameter source_id | the FAMIX id of the source element
    methods set_target_source
      importing
        target_id type i
        source_id type i.
endclass.                    "cl_famix_reference DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_reference IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_reference implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.Reference'.
  endmethod.                    "constructor

  method set_target_source.

    g_model->add_reference_by_id( exporting attribute_name    = 'target'
                                            reference_id      = target_id ).
    g_model->add_reference_by_id( exporting attribute_name    = 'source'
                                            reference_id       = source_id ).
  endmethod.                    "set_target_source

endclass.                    "cl_famix_reference IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_famix_custom_source_lang DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_custom_source_lang definition inheriting from cl_famix_entity.
  public section.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    methods add importing name                          type string
                exporting value(exists_already_with_id) type i
                          value(id)                     type i.
    methods constructor importing model type ref to cl_model.
endclass.                    "cl_famix_custom_source_lang DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_famix_custom_source_lang IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_famix_custom_source_lang implementation.

  method constructor.
    call method super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  endmethod.                    "constructor

  method add.
    g_model->add_entity( exporting elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              importing exists_already_with_id = exists_already_with_id
                                processed_id = id ).
    g_last_used_id = id.
  endmethod.                    "add

endclass.                    "cl_famix_custom_source_lang IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_make_demo_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_make_demo_model definition.
  public section.
    class-methods make
      exporting
        mse_model type cl_model=>lines_type.
endclass.                    "cl_make_demo_model DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_make_demo_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_make_demo_model implementation.

  method make.
    data model  type ref to cl_model.

    data famix_namespace  type ref to cl_famix_namespace.
    data famix_package      type ref to cl_famix_package.
    data famix_class        type ref to cl_famix_class.
    data famix_method         type ref to cl_famix_method.
    data famix_attribute    type ref to cl_famix_attribute.
    data famix_inheritance  type ref to cl_famix_inheritance.

    create object famix_namespace
      exporting
        model = model.
    create object famix_package
      exporting
        model = model.
    create object famix_class
      exporting
        model = model.
    create object famix_method
      exporting
        model = model.
    create object famix_attribute
      exporting
        model = model.
    create object famix_inheritance
      exporting
        model = model.





*    data(model) = new cl_model( ).
*
*    data(famix_namespace) = new cl_famix_namespace( model ).
*    data(famix_package) = new cl_famix_package( model ).
*    data(famix_class) = new cl_famix_class( model ).
*    data(famix_method) = new cl_famix_method( model ).
*    data(famix_attribute) = new cl_famix_attribute( model ).
*    data(famix_inheritance) = new cl_famix_inheritance( model ).

    famix_namespace->add( name = 'aNamespace' ).
    famix_package->add( name = 'aPackage' ).
    famix_package->add( name = 'anotherPackage' ).
    famix_package->set_parent_package( parent_package = 'aPackage' ).
    famix_class->add( name = 'ClassA' ).
    famix_class->set_container( exporting container_element = 'FAMIX.Namespace'
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
    famix_inheritance->set_sub_and_super_class( exporting subclass_element   = 'FAMIX.Class'
                                                          subclass_name_group = ''
                                                          subclass_name      = 'ClassB'
                                                          superclass_element = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name    = 'ClassA' ).

    model->make_mse( importing mse_model = mse_model ).
  endmethod.                    "make

endclass.                    "cl_make_demo_model IMPLEMENTATION
******************************************** End Include Z_FAMIX_ABAP *****************************

" include z_sap_2_famix
******************************************** Begin Include Z_SAP_2_FAMIX ****************************

"! This is the master class for all classes that build a model for the SAP system.
"! Its main usage is in the moment to display the connection of the classes to extract SAP model data in the extracted model.
"! It may later be replaced by cl_sap_abap, cl_sap_bw, ...
*----------------------------------------------------------------------*
*       CLASS cl_sap DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap definition.
endclass.                    "cl_sap DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap implementation.

endclass.                    "cl_sap IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_package DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_package definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add
      importing
        name type clike.
    "! Call once to set the parent package
    methods set_parent_package
      importing
        parent_package type clike.
  private section.
    data: g_famix_package type ref to cl_famix_package.
endclass.                    "cl_sap_package DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_package IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_package implementation.

  method constructor.
    super->constructor( ).
    create object g_famix_package
      exporting
        model = model.
*    g_famix_package = new cl_famix_package( model = model ).
  endmethod.                    "constructor

  method add.
    g_famix_package->add( name = name ).
  endmethod.                    "add

  method set_parent_package.
    g_famix_package->set_parent_package( parent_package = parent_package ).
  endmethod.                    "set_parent_package

endclass.                    "cl_sap_package IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_class definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    "! Add global class
    methods add
      importing name                          type clike
      exporting value(exists_already_with_id) type i
                value(id)                     type i.
    "! Specify the parent program for a local class
    methods set_parent_program
      importing
        sap_program type string.
    methods set_parent_package
      importing
        parent_package type clike.
    methods is_interface.
    "! Add local class of a program
    "! @parameter program | the name of the program the local class is part of
    methods add_local
      importing
        program   type string
        name      type any
      returning
        value(id) type i.
  private section.
    data: g_famix_class type ref to cl_famix_class.
endclass.                    "cl_sap_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_class implementation.
  method constructor.
    super->constructor( ).
*    g_famix_class = new cl_famix_class( model = model ).
    create object g_famix_class
      exporting
        model = model.
  endmethod.                    "constructor

  method add.
    g_famix_class->add( exporting name_group             = ''
                                       name                   = name
                             importing exists_already_with_id = exists_already_with_id
                                  id = id ).
  endmethod.                    "add

  method set_parent_program.

    " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

    g_famix_class->set_container( exporting container_element = 'FAMIX.Module'
                                            parent_container  = sap_program ).
  endmethod.                    "set_parent_program

  method set_parent_package.
    g_famix_class->set_parent_package( parent_package = parent_package ).
  endmethod.                    "set_parent_package


  method is_interface.
    g_famix_class->is_interface( ).
  endmethod.                    "is_interface


  method add_local.
    g_famix_class->add( exporting name_group = program
                                  name       = name
                        importing id = id ).
  endmethod.                    "add_local

endclass.                    "cl_sap_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_attribute DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_attribute definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods get_id
      importing
        class     type clike
        attribute type clike
      returning
        value(id) type i.
    methods add
      importing
        class     type clike
        attribute type clike.
  private section.
    data: g_famix_attribute type ref to cl_famix_attribute.
endclass.                    "cl_sap_attribute DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_attribute IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_attribute implementation.

  method constructor.
    super->constructor( ).
*    g_famix_attribute = new cl_famix_attribute( model = model ).
    create object g_famix_attribute
      exporting
        model = model.
  endmethod.                    "constructor


  method get_id.
    " SAP_2_FAMIX_13        Mapp attributes of classes to FAMIX.Attribute
    " SAP_2_FAMIX_14        Mapp attributes of interfaces to FAMIX.Attribute
    id = g_famix_attribute->get_id( class     = class
                                    attribute = attribute ).
  endmethod.                    "get_id


  method add.

    g_famix_attribute->add( name = attribute ).
    g_famix_attribute->set_parent_type( exporting parent_element = 'FAMIX.Class'
                                                  parent_name    = class ).
    g_famix_attribute->store_id( exporting class     = class
                                           attribute = attribute ).

  endmethod.                    "add

endclass.                    "cl_sap_attribute IMPLEMENTATION

"! Specifies a SAP method
*----------------------------------------------------------------------*
*       CLASS cl_sap_method DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_method definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    "! Returns the ID for a given method of a global class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    methods get_id
      importing
        class     type clike
        method    type clike
      returning
        value(id) type i.
    "! Add a method for a global SAP class or a global SAP instance
    methods add
      importing
        class     type clike
        method    type clike
      returning
        value(id) type i.
    methods add_local_method
      importing
        class_name  type clike
        class_id    type i
        method_name type clike
      returning
        value(id)   type i.
  private section.
    data: g_famix_method type ref to cl_famix_method.

endclass.                    "cl_sap_method DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_method IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_method implementation.

  method constructor.
    super->constructor( ).

*    g_famix_method = new cl_famix_method( model = model ).
    create object g_famix_method
      exporting
        model = model.

  endmethod.                    "constructor


  method get_id.
    id = g_famix_method->get_id( class  = class
                                 method = method ).
  endmethod.                    "get_id


  method add.
    " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
    " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
    g_famix_method->add( exporting name = method importing id = id ).

    " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
    " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( signature = method ).

    g_famix_method->set_parent_type( exporting parent_element = 'FAMIX.Class'
                                               parent_name    = class ).

    g_famix_method->store_id( exporting class  = class
                                        method = method ).

  endmethod.                    "add


  method add_local_method.

    " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method
    g_famix_method->add( exporting name_group = class_name " TBD Why name of class in name_group?
                                        name       = method_name
                                        importing id = id ).
    " SAP_2_FAMIX_43        Fill the attribute signature of FAMIX.METHOD with the name of the method
    g_famix_method->set_signature( signature = method_name ).

    " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class
    g_famix_method->set_parent_type( exporting parent_element = 'FAMIX.Class'
                                               parent_id      =  class_id ).
  endmethod.                    "add_local_method

endclass.                    "cl_sap_method IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_inheritance DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_inheritance definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add.
    methods set_sub_and_super_class
      importing
        subclass_name   type clike
        superclass_name type clike.
    methods set_interface_for_class
      importing
        interface_name type clike
        class_name     type clike.
    methods set_local_sub_and_super_class
      importing
        program         type clike
        subclass_name   type any
        superclass_name type any.
  private section.
    data: g_famix_inheritance type ref to cl_famix_inheritance.
endclass.                    "cl_sap_inheritance DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_inheritance IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_inheritance implementation.

  method constructor.
    super->constructor( ).
*    g_famix_inheritance = new cl_famix_inheritance( model = model ).
    create object g_famix_inheritance
      exporting
        model = model.
  endmethod.                    "constructor


  method add.
    g_famix_inheritance->add( ).
  endmethod.                    "add


  method set_sub_and_super_class.

    " SAP_2_FAMIX_39     Map all inheritances between classes in selected packages to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( exporting subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = ''
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = ''
                                                            superclass_name       = superclass_name ).
  endmethod.                    "set_sub_and_super_class


  method set_interface_for_class.

    " SAP_2_FAMIX_40        Map all interface implementations of interfaces in selected packages by classes of selected packages by FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( exporting subclass_element      = 'FAMIX.Class'
                                                          subclass_name_group   = ''
                                                          subclass_name         = interface_name
                                                          superclass_element    = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name       = class_name ).
  endmethod.                    "set_interface_for_class


  method set_local_sub_and_super_class.

    " SAP_2_FAMIX_38        Map local inheritances of classes to FAMIX.Inheritance
    g_famix_inheritance->set_sub_and_super_class( exporting subclass_element      = 'FAMIX.Class'
                                                            subclass_name_group   = program
                                                            subclass_name         = subclass_name
                                                            superclass_element    = 'FAMIX.Class'
                                                            superclass_name_group = program
                                                            superclass_name       = superclass_name ).
  endmethod.                    "set_local_sub_and_super_class

endclass.                    "cl_sap_inheritance IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_invocation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_invocation definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add_invocation
      importing
        used_method_id  type i
        using_method_id type i.
  private section.
    data: g_famix_invocation type ref to cl_famix_invocation.
endclass.                    "cl_sap_invocation DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_invocation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_invocation implementation.

  method constructor.
    super->constructor( ).
*    g_famix_invocation = new cl_famix_invocation( model = model ).
*    DATA g_famix_invocation TYPE REF TO cl_famix_invocation.
    create object g_famix_invocation
      exporting
        model = model.
  endmethod.                    "constructor


  method add_invocation.
    " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
    if g_famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method_id
                                                           candidates_id = used_method_id ) = true.
      g_famix_invocation->add( ).
      g_famix_invocation->set_invocation_by_reference( exporting sender_id     = using_method_id
                                                                 candidates_id = used_method_id
                                                                 signature     = 'DUMMY' ).
    endif.
  endmethod.                    "add_invocation

endclass.                    "cl_sap_invocation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_sap_access DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_access definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add_access
      importing
        used_attribute type i
        using_method   type i.
  private section.
    data: g_famix_access type ref to cl_famix_access.
endclass.                    "cl_sap_access DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_access IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_access implementation.

  method constructor.
    super->constructor( ).
*    g_famix_access = new cl_famix_access( model = model ).
*    DATA g_famix_access TYPE REF TO cl_famix_access.
    create object g_famix_access
      exporting
        model = model.
  endmethod.                    "constructor


  method add_access.
    " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
    " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

    if g_famix_access->is_new_access( accessor_id = using_method
                                      variable_id = used_attribute ) = true.
      g_famix_access->add( ).
      g_famix_access->set_accessor_variable_relation( exporting accessor_id = using_method
                                                                variable_id = used_attribute ).
    endif.
  endmethod.                    "add_access

endclass.                    "cl_sap_access IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS cl_sap_program DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_program definition inheriting from cl_sap.
  public section.
    methods constructor importing model type ref to cl_model.
    methods add
      importing
        name      type clike
      returning
        value(id) type i.
    "! Call once to set the parent package of a program
    methods set_parent_package
      importing
        parent_package type clike.
  private section.
    data g_famix_module type ref to cl_famix_module.
endclass.                    "cl_sap_program DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_sap_program IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_sap_program implementation.

  method constructor.
    super->constructor( ).
*    g_famix_module = new cl_famix_module( model = model ).
*    DATA g_famix_module TYPE REF TO cl_famix_module.
    create object g_famix_module
      exporting
        model = model.
  endmethod.                    "constructor


  method add.

    " SAP_2_FAMIX_5     Map program to FAMIX.Module
    g_famix_module->add( exporting name = name importing id = id ).

  endmethod.                    "add


  method set_parent_package.
    g_famix_module->set_parent_package( parent_package = parent_package ).
  endmethod.                    "set_parent_package

endclass.                    "cl_sap_program IMPLEMENTATION



******************************************** End Include Z_SAP_2_FAMIX ******************************

types: begin of class_component_type,
         clsname type seocompo-clsname,
         cmpname type seocompo-cmpname,
         cmptype type seocompo-cmptype,
       end of class_component_type.

types component_type type string.

types: begin of component_infos_type,
         component      type component_type,
         component_name type string,
         package        type devclass,
       end of component_infos_type.

types components_infos_type type hashed table of component_infos_type with unique key component component_name.

types: begin of map_tadir_component_type,
         object    type trobjtype, " The SAP TADIR Name
         component type component_type, " As called here
       end of map_tadir_component_type.

types tadir_components_mapping_type type hashed table of map_tadir_component_type with unique key object
                                                                            with unique hashed key comp components component.

types: begin of class_interface_type,
         obj_name type seoclsname,
       end of class_interface_type.

types: begin of program_type,
         program type string,
       end of program_type.

types:begin of class_type,
        class type seoclsname,
      end of class_type.

types: begin of inheritance_type,
         clsname    type seometarel-clsname,
         refclsname type seometarel-refclsname,
         reltype    type seometarel-reltype,
       end of inheritance_type.

*----------------------------------------------------------------------*
*       CLASS cl_extract_sap DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_extract_sap definition.
  public section.
    methods extract
      exporting
                mse_model           type cl_model=>lines_type
                value(nothing_done) type bool.
  private section.

    "! Maps the component lists from SAP (table TADIR) to the component list used in this program
    data g_tadir_components_mapping type tadir_components_mapping_type.

    constants comptype_attribute type seocmptype value '0'.
    constants comptype_method type seocmptype value '1'.


    methods _set_default_language
      importing
        model type ref to cl_model.

    types: begin of package_type,
             devclass type devclass,
           end of package_type.
    types: begin of package,
             devclass type devclass,
             parentcl type devclass,
           end of package.
    types:
      processed_packages_type type hashed table of package_type with unique key devclass.
    methods _determine_packages_to_analyze
      importing
        sap_package               type ref to cl_sap_package
        package_first             type tdevc
      returning
        value(processed_packages) type processed_packages_type.
    types:
      classes_type  type standard table of class_interface_type with default key,
      programs_type type standard table of program_type with default key.
    methods _analyze_components
      importing
        components_infos type components_infos_type
      exporting
        value(classes)   type classes_type
        value(programs)  type programs_type.
    methods _read_all_programs
      importing
        sap_package      type ref to cl_sap_package
        sap_program      type ref to cl_sap_program
        components_infos type components_infos_type
        programs         type programs_type
      changing
        model            type ref to cl_model.
    types:existing_classes_type type hashed table of class_type with unique key class.
    methods _add_classes_to_model
      importing
        sap_package      type ref to cl_sap_package
        sap_class        type ref to cl_sap_class
        components_infos type components_infos_type
        existing_classes type existing_classes_type.
    methods _determine_inheritances_betwee
      importing
        sap_inheritance  type ref to cl_sap_inheritance
        existing_classes type existing_classes_type.
    types: class_components_type   type hashed table of class_component_type with unique key clsname cmpname.
    methods _determine_class_components
      importing
        existing_classes        type existing_classes_type
      returning
        value(class_components) type class_components_type.
    methods _add_to_class_components_to_mo
      importing
        class_components type class_components_type
        sap_method       type ref to cl_sap_method
        sap_attribute    type ref to cl_sap_attribute.
    methods _determine_usage_of_methods
      importing
                sap_class                   type ref to cl_sap_class
                class_components            type class_components_type
                sap_method                  type ref to cl_sap_method
                sap_attribute               type ref to cl_sap_attribute
                sap_invocation              type ref to cl_sap_invocation
                sap_access                  type ref to cl_sap_access
      returning value(new_components_infos) type components_infos_type.
    "! Determine usages for components
    "! If using components are not part of the model, they are either added or replaced by a dummy component
    methods _determine_usages
      importing
                sap_class                   type ref to cl_sap_class
                class_component             type class_component_type
                sap_method                  type ref to cl_sap_method
                sap_invocation              type ref to cl_sap_invocation
                sap_access                  type ref to cl_sap_access
                used_id                     type i
      returning value(new_components_infos) type components_infos_type.
    types:
      classes_4_type        type standard table of class_interface_type with default key.
    methods _read_all_classes
      importing
        classes                 type classes_4_type
      returning
        value(existing_classes) type existing_classes_type.
    "! Evaluate user selection and return initial list of objects to analyze
    "! @parameter nothing_selected | nothing is selected
    methods _select_requested_components
      importing
        sap_package           type ref to cl_sap_package
        package_to_analyze    type parentcl
        select_by_top_package type bool
      exporting
        components_infos      type components_infos_type
        nothing_selected      type bool.

endclass.                    "cl_extract_sap DEFINITION

types: begin of indexed_token_type,
         index type i,
         str   type string,
         row   type token_row,
         col   type token_col,
         type  type token_type,
       end of indexed_token_type.

types sorted_tokens_type type sorted table of indexed_token_type with unique key index.

"! Analyze ABAP Statement of type K (Other ABAP key word)
*----------------------------------------------------------------------*
*       CLASS cl_ep_analyze_other_keyword DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_ep_analyze_other_keyword definition.
  public section.
    methods constructor
      importing
        sorted_tokens type sorted_tokens_type.
    methods analyze
      importing
        statement type sstmnt.
    types statement_type type c length 2.
    constants:

      start_class_definition      type statement_type value 'CD',
      start_class_implementation  type statement_type value 'CI',
      end_class                   type statement_type value 'CE',
      method_definition           type statement_type value 'MD',
      start_method_implementation type statement_type value 'MI',
      end_method_implementation   type statement_type value 'ME',
      attribute_definition        type statement_type value 'AD',
      start_public                type statement_type value 'PU',
      start_protected             type statement_type value 'PO',
      start_private               type statement_type value 'PR'.


    types: begin of info_type,
             statement_type      type statement_type,
             is_class_stmnt_info type bool,
             class_is_inheriting type bool,
             class_inherits_from type string,
             is_static           type bool,
             name                type string,
           end of info_type.
    data: g_info type info_type read-only.
  private section.
    data g_sorted_tokens type sorted_tokens_type.
endclass.                    "cl_ep_analyze_other_keyword DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_ep_analyze_other_keyword IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_ep_analyze_other_keyword implementation.

  method constructor.
    g_sorted_tokens = sorted_tokens.

  endmethod.                    "constructor

  method analyze.
    assert statement-type eq 'K'.
    clear g_info.
*    g_info = value #( ).

    field-symbols <token> like line of g_sorted_tokens.

    " First Run, what is the keyword
    read table g_sorted_tokens assigning <token> with table key index = statement-from.
    if sy-subrc <> ok.
      " TBD Error handling
      " In the moment ignore
      return.
    endif.

    case <token>-str.
      when 'CLASS'.
        g_info-is_class_stmnt_info = true.

      when 'ENDCLASS'.
        g_info-statement_type = end_class.
      when 'PUBLIC'.
        g_info-statement_type = start_public.
      when 'PROTECTED'.
        g_info-statement_type = start_protected.
      when 'PRIVATE'.
        g_info-statement_type = start_private.
      when 'METHODS'.
        " info-is_method_stmnt = true.
        g_info-statement_type = method_definition.
      when 'CLASS-METHODS'.
        g_info-statement_type = method_definition.
        g_info-is_static = true.
      when 'METHOD'.
        g_info-statement_type = start_method_implementation.
      when 'ENDMETHOD'.
        g_info-statement_type = end_method_implementation.

      when 'DATA'.
        g_info-statement_type = attribute_definition.
      when 'CLASS-DATA'.
        g_info-statement_type = attribute_definition.
        g_info-is_static = true.
      when others.
        " TBD
        " Add further, in the moment ignore
        return.
    endcase.

    " Second Run, what is the name
    if g_info-is_class_stmnt_info eq true
    or g_info-statement_type eq method_definition
    or g_info-statement_type eq start_method_implementation
    or g_info-statement_type eq attribute_definition.

*      data(position_of_name) = statement-from + 1.
      data position_of_name type i.
      position_of_name =  statement-from + 1.
      read table g_sorted_tokens assigning <token> with table key index = position_of_name.
      if sy-subrc <> ok.
        " TBD Error handling
        " In the moment ignore
        return.
      endif.

      g_info-name = <token>-str.

      " Third run, further keywords
      if g_info-is_class_stmnt_info eq true.
        loop at g_sorted_tokens assigning <token> where index > position_of_name
                                                       and index <= statement-to.
          case <token>-str.
            when 'DEFINITION'.
              g_info-statement_type = start_class_definition.
            when 'IMPLEMENTATION'.
              g_info-statement_type = start_class_implementation.
            when 'INHERITING'.
              g_info-class_is_inheriting = true.
*              data(superclass_is_at) = sy-tabix + 2.
              data superclass_is_at type i.
              superclass_is_at  = sy-tabix + 2.
              field-symbols <ls_superclass_token> like line of g_sorted_tokens.
              read table g_sorted_tokens assigning <ls_superclass_token> with table key index = superclass_is_at.
              if sy-subrc eq ok.
                g_info-class_inherits_from = <ls_superclass_token>-str.
              else.
                " TBD Error handling
                " In the moment ignore
                return.
              endif.
          endcase.
        endloop.
      endif.

    endif.

  endmethod.                    "analyze

endclass.                    "cl_ep_analyze_other_keyword IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS cl_program_analyzer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_program_analyzer definition.
  public section.
    methods extract
      importing
        module_reference type i
        program          type clike "progname
      changing
        model            type ref to cl_model.

endclass.                    "cl_program_analyzer DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_program_analyzer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_program_analyzer implementation.

  method extract.
    data source type table of string.
    read report program into source.

    data: tokens type standard table of stokes.

    data: sorted_tokens type sorted_tokens_type.

    data statements type standard table of sstmnt.

    scan abap-source source tokens into tokens statements into statements.
    field-symbols <ls_token_2> like line of tokens.
    data token like line of sorted_tokens.
    loop at tokens assigning <ls_token_2>.
*      sorted_tokens = value #( base sorted_tokens ( INDEX = sy-tabix
*                                                    str   = <ls_token_2>-str
*                                                    row   = <ls_token_2>-row
*                                                    col   = <ls_token_2>-col
*                                                    type  = <ls_token_2>-type ) ).
      token-index = sy-tabix.
      token-str   = <ls_token_2>-str.
      token-row   = <ls_token_2>-row.
      token-col   = <ls_token_2>-col.
      token-type  = <ls_token_2>-type.
      insert token into table sorted_tokens.
    endloop.

    sort statements by from.

    if g_parameter_list_tokens eq true.
      write: /.
      write: / program.

    endif.

    types section_type type c length 1.

    constants: "! Is in public section
      public    type section_type value '1',
      "! Is in protected section
      protected type section_type value '2',
      "! Is in private section
      private   type section_type value '3',
      "! Not in a section
      none      type section_type value ' '.

    types: begin of codecontext_type,
             in_section               type section_type,
             in_class_definition      type bool,
             implementation_of_class  type string,
             implementation_of_method type string,
           end of codecontext_type.

    "! Context of statement in the code
    data context type codecontext_type.

    types: begin of class_with_model_id_type,
             classname   type string,
             id_in_model type i,
           end of class_with_model_id_type.

    data: classes_with_model_id      type hashed table of class_with_model_id_type with unique key classname,
          actual_class_with_model_id type class_with_model_id_type.

    types: begin of method_type,
             classname          type string,
             class_id_in_model  type i,
             methodname         type string,
             method_id_in_model type i,
             in_section         type section_type,
             instanciable       type bool,
           end of method_type.

    data: methods       type standard table of method_type,
          actual_method type method_type.

    types: begin of inheritance_type,
             subclass   type string,
             superclass type string,
           end of inheritance_type.

    data: inheritances type standard table of inheritance_type.

    data token_number type i.

    "! Instance that analyzes other ABAP Keywords
    data aok type ref to cl_ep_analyze_other_keyword.
    create object aok
      exporting
        sorted_tokens = sorted_tokens.
    field-symbols <statement> like line of statements.
    loop at statements assigning <statement>.

      token_number = 0.
      case <statement>-type.
        when 'K'.

          aok->analyze( statement = <statement> ).
          case aok->g_info-statement_type.
            when aok->start_class_definition.
              " SAP_2_FAMIX_28        Determine local classes in programs
              context-in_class_definition = true.
              actual_class_with_model_id-classname = aok->g_info-name.
*              classes_with_model_id = value #( base classes_with_model_id ( actual_class_with_model_id ) ).
              data class_with_model_id like line of classes_with_model_id.
              insert actual_class_with_model_id into table classes_with_model_id.

              if aok->g_info-class_is_inheriting eq true.
                " SAP_2_FAMIX_37        Determine local inheritances of classes

*                inheritances = value #( base inheritances ( subclass = actual_class_with_model_id-classname
*                                                            superclass = aok->g_info-class_inherits_from ) ).
                data inheritance like line of inheritances.
                inheritance-subclass = actual_class_with_model_id-classname.
                inheritance-superclass = aok->g_info-class_inherits_from.
                insert inheritance into table inheritances.
              endif.
            when aok->start_public.
              context-in_section = public.
            when aok->start_protected.
              context-in_section = protected.
            when aok->start_private.
              context-in_section = private.
            when aok->end_class.
              context-in_section = none.
              context-in_class_definition = false.
*              context-implementation_of_class = value #( ).
              clear context-implementation_of_class.
            when aok->method_definition.
              " SAP_2_FAMIX_29      Determine local class methods in programs
              if aok->g_info-is_static eq true.
*                actual_method = value #( classname = actual_class_with_model_id-classname
*                                         in_section = context-in_section ).
                actual_method-classname = actual_class_with_model_id-classname.
                actual_method-in_section = context-in_section.
              else.
*                actual_method = value #( classname = actual_class_with_model_id-classname
*                                         in_section = context-in_section
*                                         instanciable = true ).
                actual_method-classname = actual_class_with_model_id-classname.
                actual_method-in_section = context-in_section.
                actual_method-instanciable = true.
              endif.
              actual_method-methodname = aok->g_info-name.
            when aok->start_class_implementation.
              context-implementation_of_class = aok->g_info-name.
            when aok->start_method_implementation.
              context-implementation_of_method = aok->g_info-name.
              if g_parameter_list_tokens eq true.
                format color col_group.
              endif.
            when aok->end_method_implementation.
*              context-implementation_of_method = value #( ).
              clear context-implementation_of_method.
              if g_parameter_list_tokens eq true.
                format color col_background.
              endif.
            when others.

          endcase.

        when others.

      endcase.

      if g_parameter_list_tokens eq true.
        write: / <statement>-type.
        field-symbols <token> like line of sorted_tokens.
        loop at sorted_tokens assigning <token> where
            index >= <statement>-from
        and index <= <statement>-to.
          write: '|', <token>-type, <token>-str.
        endloop.
      endif.

    endloop.

    " Add local classes to model

*    data(sap_class) = new cl_sap_class( model ).
    data sap_class type ref to cl_sap_class.
    create object sap_class
      exporting
        model = model.
    field-symbols <class> like line of classes_with_model_id.
    loop at classes_with_model_id assigning <class>.
      " SAP_2_FAMIX_30        Map local classes of programs to FAMIX.Class
      data help_program type string.
      help_program = program.
      sap_class->add_local( exporting program = help_program
                                      name    = <class>-classname
                            receiving id = <class>-id_in_model ).

      sap_class->set_parent_program( sap_program = help_program  ).


    endloop.

    " Add local methods to model

*    data(sap_method) = new cl_sap_method( model ).
    data sap_method type ref to cl_sap_method.
    create object sap_method
      exporting
        model = model.

    field-symbols <class_2> like line of classes_with_model_id.
    field-symbols <method> like line of methods.
    loop at methods assigning <method>.
      read table classes_with_model_id assigning <class_2> with table key classname = <method>-classname.
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
    endloop.

    " Add local inheritances to model

*    data(sap_inheritance) = new cl_sap_inheritance( model ).
    data sap_inheritance type ref to cl_sap_inheritance.
    create object sap_inheritance
      exporting
        model = model.
    loop at inheritances into inheritance.

      sap_inheritance->add( ).
      sap_inheritance->set_local_sub_and_super_class( exporting program = help_program
                                                                subclass_name   = inheritance-subclass
                                                                superclass_name = inheritance-superclass ).

    endloop.

  endmethod.                    "extract

endclass.                    "cl_program_analyzer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS cl_extract_sap IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class cl_extract_sap implementation.

  method extract.

    types:begin of package_type,
            devclass type devclass,
          end of package_type.

    data components_infos type components_infos_type.
    data new_components_infos type components_infos_type.
    data processed_components_infos type components_infos_type.
    data classes type standard table of class_interface_type.
    data programs type standard table of program_type.
    data existing_classes type hashed table of class_type with unique key class.

    data class_components type hashed table of class_component_type with unique key clsname cmpname.

    " Do not use singleton pattern, but define each instance only one time at the start. Multiple instanciation allowed unless
    " specified for the class

*    data(model) = new cl_model( ).
*    data(sap_package) = new cl_sap_package( model ).
*    data(sap_program) = new cl_sap_program( model ).
*    data(sap_class) = new cl_sap_class( model ).
*    data(sap_inheritance) = new cl_sap_inheritance( model ).
*    data(sap_method) = new cl_sap_method( model ).
*    data(sap_attribute) = new cl_sap_attribute( model ).
*    data(sap_invocation) = new cl_sap_invocation( model ).
*    data(sap_access) = new cl_sap_access( model ).

    data model            type ref to cl_model.
    data sap_package     type ref to cl_sap_package.
    data sap_program     type ref to cl_sap_program.
    data sap_class       type ref to cl_sap_class.
    data sap_inheritance type ref to cl_sap_inheritance.
    data sap_method      type ref to cl_sap_method.
    data sap_attribute   type ref to cl_sap_attribute.
    data sap_invocation  type ref to cl_sap_invocation.
    data sap_access      type ref to cl_sap_access.

    create object model.
    create object sap_package
      exporting
        model = model.
    create object sap_program
      exporting
        model = model.
    create object sap_class
      exporting
        model = model.
    create object sap_inheritance
      exporting
        model = model.
    create object sap_method
      exporting
        model = model.
    create object sap_attribute
      exporting
        model = model.
    create object sap_invocation
      exporting
        model = model.
    create object sap_access
      exporting
        model = model.

    " Set TADIR mapping
*    g_tadir_components_mapping = value #( ( OBJECT = 'CLAS' COMPONENT = 'GlobClass' )
*                                          ( OBJECT = 'INTF' COMPONENT = 'GlobIntf' )
*                                          ( OBJECT = 'PROG' COMPONENT = 'ABAPProgramm') ).

    data gs_mapping type map_tadir_component_type.
    gs_mapping-object = 'CLAS'. gs_mapping-component = 'GlobClass'. insert gs_mapping into table g_tadir_components_mapping.
    gs_mapping-object = 'INTF'. gs_mapping-component = 'GlobIntf'. insert gs_mapping into table g_tadir_components_mapping.
    gs_mapping-object = 'PROG'. gs_mapping-component = 'ABAPProgram'. insert gs_mapping into table g_tadir_components_mapping.



    _set_default_language( model ).

    data nothing_selected type bool.

    if g_filter_using_package eq true.
*      data(select_by_top_package) = true.
      data select_by_top_package type boolean.
      select_by_top_package = true.
    elseif g_filter_using_name eq true.
      select_by_top_package = false.
    else.
      assert 1 = 2.
    endif.
    _select_requested_components( exporting  sap_package          = sap_package
                                             select_by_top_package = select_by_top_package
                                             package_to_analyze    = g_parameter_package_to_analyze
                                   importing components_infos      = components_infos
                                             nothing_selected      = nothing_selected ).



    " Select requested components by complex filters





    if nothing_selected eq true.
      nothing_done = true.
      return.
    endif.

    while lines( components_infos ) <> 0.

      _analyze_components( exporting components_infos = components_infos
                           importing classes          = classes
                                     programs         = programs ).

      _read_all_programs( exporting sap_package    = sap_package
                                    sap_program      = sap_program
                                    components_infos = components_infos
                                    programs         = programs
                           changing model = model ).

      existing_classes = _read_all_classes( classes ).

      _add_classes_to_model( sap_package     = sap_package
                             sap_class       = sap_class
                             components_infos  = components_infos
                             existing_classes  = existing_classes ).

      _determine_inheritances_betwee( sap_inheritance = sap_inheritance
                                      existing_classes  = existing_classes ).

      class_components = _determine_class_components( existing_classes ).

      _add_to_class_components_to_mo( class_components = class_components
                                      sap_method     = sap_method
                                      sap_attribute  = sap_attribute ).

      new_components_infos = _determine_usage_of_methods( sap_class       = sap_class
                                                          class_components  = class_components
                                                          sap_method      = sap_method
                                                          sap_attribute   = sap_attribute
                                                          sap_invocation  = sap_invocation
                                                          sap_access      = sap_access ).

      " Determine package for new components

      " TBD Find more performant solution
      field-symbols <component_infos> like line of new_components_infos.
      loop at new_components_infos assigning <component_infos>.
        "READ
*        data(object) = g_tadir_components_mapping[ KEY comp COMPONENT = <component_infos>-component ]-OBJECT.
        data object type trobjtype.
        data ls_tadir like line of g_tadir_components_mapping.
        read table g_tadir_components_mapping
              into ls_tadir
              with key component  = <component_infos>-component.
        object = ls_tadir-object.

        select single devclass from tadir
          into <component_infos>-package
         where pgmid = 'R3TR'
           and object = object
           and obj_name = <component_infos>-component_name.

        if sy-subrc <> ok.
          " TBD
          " Report errors
          delete new_components_infos where component = <component_infos>-component
                                        and component_name = <component_infos>-component_name.
        endif.

      endloop.

      insert lines of components_infos into table processed_components_infos.

*      components_infos = value #( ).
      clear components_infos.


      " SAP_2_FAMIX_48      Allow to select all using objects
      " Fullfilled by adding new_components_infos to components_infos and repeating the analysis in the while loop
      field-symbols <component_infos_2> like line of new_components_infos.
      loop at new_components_infos assigning <component_infos_2>.

        read table processed_components_infos transporting no fields with table key component = <component_infos_2>-component component_name = <component_infos_2>-component_name.

        if sy-subrc <> ok.
          insert <component_infos_2> into table components_infos.
*          components_infos = value #( base components_infos ( <component_infos_2> ) ).

        endif.

      endloop.

    endwhile.

    model->make_mse( importing mse_model = mse_model ).

  endmethod.                    "extract

  method _determine_usages.

    data where_used_name type eu_lname.
    case class_component-cmptype.
      when comptype_method.

        " SAP_2_FAMIX_17      Determine usage of class methods by programs and classes
        " SAP_2_FAMIX_18      Determine usage of interface methods by programs and classes

        where_used_name = class_component-clsname && |\\ME:| && class_component-cmpname.
        data where_used_components type standard table of wbcrossgt.
        select * from wbcrossgt into table where_used_components where otype = 'ME' and name = where_used_name.
      when comptype_attribute.

        " SAP_2_FAMIX_19      Determine usage of class attributes by programs and classes
        " SAP_2_FAMIX_20      Determine usage of interface attributes by programs and classes

        where_used_name = class_component-clsname && |\\DA:| && class_component-cmpname.
        select * from wbcrossgt into table where_used_components where otype = 'DA' and name = where_used_name.
      when others.
        assert 1 = 2.
    endcase.

    field-symbols <where_used_component> like line of where_used_components.
    loop at where_used_components assigning <where_used_component>.
*      data ris_prog_tadir_line type ris_prog_tadir.
*      SELECT SINGLE * FROM ris_prog_tadir
*        INTO ris_prog_tadir_line
*       WHERE program_name = <where_used_component>-include.
      data ls_mtdkey type seocpdkey.

      call function 'SEO_METHOD_GET_NAME_BY_INCLUDE'
        exporting
          progname = <where_used_component>-include
        importing
          mtdkey   = ls_mtdkey.

      if sy-subrc eq ok.
*        CASE ris_prog_tadir_line-object_type.
*          WHEN 'CLAS'.
        " Used by method
        data: using_method type string.
*            IF ris_prog_tadir_line-method_name IS INITIAL.
        if ls_mtdkey-cpdname is initial.
          using_method = 'DUMMY'.
        else.
          using_method = ls_mtdkey-cpdname.
        endif.

        data using_method_id type i.
*            data(using_method_id) = sap_method->get_id( class  = conv string( ris_prog_tadir_line-object_name )
*                                                             METHOD = using_method ).
        using_method_id = sap_method->get_id( class  = ls_mtdkey-clsname
                                              method = using_method ).

        if using_method_id eq 0.

          if g_param_usage_outpack_groupd eq false.

            " Method does not exist, create the class
            " SAP_2_FAMIX_21      If a component is used by a class that is not selected, add this class to the model
            " SAP_2_FAMIX_22      Do not assign classes that included due to usage to a package

            data exists_already_with_id type i.
            sap_class->add( exporting name = ls_mtdkey-cpdname
                            importing exists_already_with_id = exists_already_with_id ).

            if exists_already_with_id is initial.

              " SAP_2_FAMIX_47      If no dummy class is specified in case a using class is not in the initial selection, analyze this classes also

*              new_components_infos = value #( base new_components_infos (
*                                                   component_name = ris_prog_tadir_line-object_name
*                                                   COMPONENT   = g_tadir_components_mapping[ OBJECT = 'CLAS' ]-COMPONENT ) ).
              data new_components_info like line of new_components_infos.

              data gs_tadir_comp_map like line of g_tadir_components_mapping.
              read table g_tadir_components_mapping into gs_tadir_comp_map with table key object = 'CLAS'.
              if sy-subrc = 0.
                new_components_info-component_name = ls_mtdkey-cpdname.
                new_components_info-component   = gs_tadir_comp_map-component .
                insert new_components_info into table new_components_infos.
              endif.

            endif.

          else.
            " SAP_2_FAMIX_35        Add a usage to a single dummy class "OTHER_SAP_CLASS" if required by a parameter

            sap_class->add( exporting name = 'OTHER_SAP_CLASS'
                            importing exists_already_with_id = exists_already_with_id ).

          endif.

          " Now there is a class, but no duplicate class

          if g_param_usage_outpack_groupd eq false.
            using_method_id = sap_method->get_id( class  = ls_mtdkey-cpdname
                                                  method = using_method ).
          else.
            using_method_id = sap_method->get_id( class  = 'OTHER_SAP_CLASS'
                                                  method = 'OTHER_SAP_METHOD' ).
          endif.


          if using_method_id eq 0.
            if g_param_usage_outpack_groupd eq false.
              " Now also the method is to be created
              " SAP_2_FAMIX_23       If a component is used by a class that is not selected, add the using methods to the model

              using_method_id = sap_method->add( class  = ls_mtdkey-cpdname
                                                 method = using_method ).

            else.

              " SAP_2_FAMIX_36        Add a usage to a single dummy method "OTHER_SAP_METHOD" if required by a parameter

              using_method_id = sap_method->add( class  = 'OTHER_SAP_CLASS'
                                                 method = 'OTHER_SAP_METHOD'  ).

            endif.
          endif.

        endif.

        case class_component-cmptype.
          when comptype_method.

            sap_invocation->add_invocation( used_method_id = used_id
                                            using_method_id = using_method_id ).

          when comptype_attribute.

            sap_access->add_access( used_attribute = used_id
                                    using_method = using_method_id ).

          when others.
            assert 1 = 2.
        endcase.


*          WHEN OTHERS.
        " TBD Implement other usages
*        ENDCASE.
      endif.

    endloop.

  endmethod.                    "_determine_usages


  method _set_default_language.

    " Set default language

*    data(famix_custom_source_language) = new cl_famix_custom_source_lang( model ).
    data famix_custom_source_language type ref to cl_famix_custom_source_lang.
    create object famix_custom_source_language
      exporting
        model = model.

    famix_custom_source_language->add( name = 'ABAP' ).

    " Do not assign any entities to ABAP, because otherwise this will not be the default language anymore
    " So do not do this for ABAP, but maybe for another language
    " famix_package->set_declared_source_language( EXPORTING source_language_element = 'FAMIX.CustomSourceLanguage'
    "                                                        source_language_name    = 'ABAP' ).

  endmethod.                    "_set_default_language


  method _determine_packages_to_analyze.

    " Determine packages to analyze

    "! A temporal helper table used to find all packages (development classes) in the selection
    data temp_packages_to_search type standard table of package_type.

    sap_package->add( name = package_first-devclass ).

    data processed_package like line of processed_packages.
    processed_package-devclass = package_first-devclass.
    insert processed_package into table processed_packages.
*    INSERT value package_type( devclass = package_first-devclass ) INTO TABLE processed_packages.


*    temp_packages_to_search = value #( ( devclass = g_parameter_package_to_analyze ) ).
    data temp_package_to_search like line of temp_packages_to_search.
    temp_package_to_search-devclass = g_parameter_package_to_analyze.
    insert temp_package_to_search into table temp_packages_to_search.

    data packages type standard table of package.

    while temp_packages_to_search is not initial.
      if temp_packages_to_search is not initial.
        select devclass  parentcl from tdevc into table packages
         for all entries in temp_packages_to_search
          where parentcl = temp_packages_to_search-devclass.
      endif.

*        temp_packages_to_search = value #( ).
      clear temp_packages_to_search.

      data package like line of packages.
      loop at packages into package.
        processed_package-devclass = package-devclass.
        insert processed_package into table processed_packages.
*        INSERT value package_type( devclass = package-devclass ) INTO TABLE processed_packages.
        if sy-subrc eq ok.
          " New package
          " Search again
          temp_package_to_search-devclass = package-devclass.
          insert temp_package_to_search into table temp_packages_to_search.
*          temp_packages_to_search = value #( base temp_packages_to_search ( devclass = package-devclass ) ).
          sap_package->add( name = package-devclass ).
          sap_package->set_parent_package( parent_package = package-parentcl ).
        endif.

      endloop.

      sort temp_packages_to_search.
      delete adjacent duplicates from temp_packages_to_search.

    endwhile.

  endmethod.                    "_determine_packages_to_analyze


  method _analyze_components.

    " Loop over all packages to find classes and programms

    " SAP_2_FAMIX_1     Extract classes from Dictionary
    " SAP_2_FAMIX_2     Extract interfaces as FAMIX.Class with attribute isinterface

*    MOVE-CORRESPONDING components_infos TO classes.
    data class like line of classes.

    field-symbols <component_infos> like line of components_infos.

    loop at components_infos assigning <component_infos>.
      move-corresponding <component_infos> to class.
      insert class into table classes.
*    LOOP AT components_infos ASSIGNING field-symbol(<component_infos>).

      if <component_infos>-component eq 'GlobClass'
      or <component_infos>-component eq 'GlobIntf'.

        class-obj_name = <component_infos>-component_name.
        insert class into table classes.
*        classes = value #( base CLASSES ( obj_name = <component_infos>-component_name ) ).

      else.
        data program like line of programs.
        program-program = <component_infos>-component_name.
        insert program into table programs.
*        programs = value #( base programs ( program = <component_infos>-component_name ) ).

      endif.

    endloop.

  endmethod.                    "_analyze_components


  method _read_all_programs.

    " Read all programs

    " SAP_2_FAMIX_4     Extract programs

    field-symbols <program> like line of programs.
    data module_reference type i.
    field-symbols <component_infos> like line of components_infos.
    data program_analyzer type ref to cl_program_analyzer.

*    LOOP AT programs ASSIGNING field-symbol(<program>).
    loop at programs assigning <program>.

      module_reference = sap_program->add( name = <program>-program ).

      read table components_infos assigning <component_infos>
            with table key component = 'ABAPProgram'
                           component_name = <program>-program.
      assert sy-subrc eq ok.

      sap_package->add( name  = <component_infos>-package ).

      sap_program->set_parent_package( parent_package = <component_infos>-package ).

      if p_iprog eq true.

*        data(program_analyzer) = new cl_program_analyzer( ).
        create object program_analyzer.

        program_analyzer->extract( exporting module_reference = module_reference
                                             program          = <program>-program
                                    changing model            = model ).

      endif.

    endloop.

  endmethod.                    "_read_all_programs


  method _add_classes_to_model.

    data existing_class like line of existing_classes.
    field-symbols <component_infos> like line of components_infos.

    " Add to model
    loop at existing_classes into existing_class.

      " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
      " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
      sap_class->add( name = existing_class-class ).

      read table components_infos assigning <component_infos> with table key component = 'GlobClass' component_name = existing_class-class.
      if sy-subrc <> ok.
        " It may be an interface
        read table components_infos assigning <component_infos> with table key component = 'GlobIntf' component_name = existing_class-class.
        assert sy-subrc eq ok.

      endif.

      sap_package->add( exporting name = <component_infos>-package ).

      sap_class->set_parent_package( parent_package = <component_infos>-package ).
      if <component_infos>-component eq 'GlobIntf'.
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        sap_class->is_interface( ).
      endif.

    endloop.

  endmethod.                    "_add_classes_to_model

  method _determine_inheritances_betwee.

    " Determine inheritances between selected classes

    data: inheritances type standard table of  inheritance_type.

    if existing_classes is not initial.
      select clsname refclsname reltype from seometarel into corresponding fields of table inheritances
        for all entries in existing_classes where clsname = existing_classes-class
                                               and version = 1.
    endif.

    data inheritance like line of inheritances.

    " Delete all inheritances where superclass is not in selected packages
    loop at inheritances into inheritance.
      read table existing_classes transporting no fields with table key class = inheritance-refclsname.
      if sy-subrc <> ok.
        delete inheritances.
      endif.
    endloop.

    data inheritance_2 like line of inheritances.

    " Add inheritances to model
    loop at inheritances into inheritance_2.
      case inheritance_2-reltype.
        when 2.
          " Inheritance

          sap_inheritance->add( ).
          sap_inheritance->set_sub_and_super_class( exporting subclass_name         = inheritance_2-clsname
                                                              superclass_name       = inheritance_2-refclsname ).
        when 1.
          " Interface implementation

          sap_inheritance->add( ).
          sap_inheritance->set_interface_for_class( exporting interface_name        =  inheritance_2-clsname
                                                              class_name            = inheritance_2-refclsname ).

        when 0.
          " Interface composition     (i COMPRISING i_ref)
          " TBD
        when 5.
          " Enhancement            ( c enhances c_ref)
          " TBD
      endcase.
    endloop.

  endmethod.                    "_determine_inheritances_betwee


  method _determine_class_components.

    " Determine class components

    " SAP_2_FAMIX_9         Extract methods of classes
    " SAP_2_FAMIX_10        Extract methods of interfaces
    " SAP_2_FAMIX_11        Extract attributes of classes
    " SAP_2_FAMIX_12        Extract attributes of interfaces

    if existing_classes is not initial.
      "
      select clsname cmpname cmptype from seocompo into table class_components
        for all entries in existing_classes
        where
          clsname = existing_classes-class.

    endif.

  endmethod.                    "_determine_class_components


  method _add_to_class_components_to_mo.

    " Add to class components to model
    data class_component like line of class_components.
    loop at class_components into class_component.

      case class_component-cmptype.
        when comptype_attribute. "Attribute

          data existing_id type i.
          existing_id =  sap_attribute->get_id( class     = class_component-clsname
                                                attribute = class_component-cmpname ).
          if existing_id eq not_found.

            sap_attribute->add( exporting class     = class_component-clsname
                                          attribute = class_component-cmpname ).
*            famix_attribute->set_parent_type(
*              EXPORTING
*                parent_element = 'FAMIX.Class'
*                parent_name    = CONV string( class_component-clsname ) ).
*            famix_attribute->store_id( EXPORTING class     = CONV string( class_component-clsname )
*                                                 attribute = CONV string( class_component-cmpname ) ).

          endif.

        when comptype_method. "Method

          existing_id = sap_method->get_id( class  = class_component-clsname
                                            method = class_component-cmpname ).

          if existing_id eq not_found.

            sap_method->add( class = class_component-clsname
                             method = class_component-cmpname ).

          endif.
        when 2. "Event
        when 3. "Type
        when others.
          " TBD Warn

      endcase.

    endloop.

  endmethod.                    "_add_to_class_components_to_mo


  method _determine_usage_of_methods.

    data class_component type class_component_type.

    " Determine usage of methods

    loop at class_components into class_component where cmptype = comptype_attribute  " Methods
                                                     or cmptype = comptype_method. "Attributes

      case class_component-cmptype.
        when comptype_method.
          data used_id type i.
          used_id = sap_method->get_id( class  = class_component-clsname
                                        method = class_component-cmpname ).

        when comptype_attribute.
          used_id = sap_attribute->get_id( class     = class_component-clsname
                                            attribute = class_component-cmpname ).

        when others.
          assert 1 = 2.
      endcase.

      insert lines of _determine_usages( sap_class        = sap_class
                                         class_component  = class_component
                                         sap_method       = sap_method
                                         sap_invocation   = sap_invocation
                                         sap_access       = sap_access
                                         used_id          = used_id ) into table new_components_infos.

    endloop.

  endmethod.                    "_determine_usage_of_methods


  method _read_all_classes.

    " Read all classes

    " Determine existing classes
    if classes is not initial.
      select clsname as class from seoclass into table existing_classes for all entries in classes
        where
          clsname = classes-obj_name.
    endif.

  endmethod.                    "_read_all_classes

  method _select_requested_components.

    data first_package type tdevc.
    data processed_packages type cl_extract_sap=>processed_packages_type.
    data object type trobjtype.
    data component_info like line of components_infos.
    data map like line of g_tadir_components_mapping.

    if select_by_top_package eq true.

      " Select components in package and sub package
      " SAP_2_FAMIX_3     Select all components in a package and the sub packages of this package

      select single devclass parentcl from tdevc into first_package where devclass = package_to_analyze.
      if sy-subrc <> ok.
        write: 'Package does not exist: ', package_to_analyze.
        nothing_selected  = true.
      endif.

      processed_packages = _determine_packages_to_analyze( sap_package = sap_package
                                                           package_first = first_package ).

    endif.

    if   select_by_top_package eq false
      or processed_packages is not initial.
      do 3 times.
        case sy-index.
          when 1.
            if p_clas eq true.
              object = 'CLAS'.
            else.
              continue.
            endif.
          when 2.
            if p_intf eq true.
              object = 'INTF'.
            else.
              continue.
            endif.
          when 3.
            if p_prog eq true.
              object = 'PROG'.
            else.
              continue.
            endif.
          when others.
            assert 1 = 2.
        endcase.
        if select_by_top_package eq true.
          data: begin of tadir_component,
                   obj_name like tadir-obj_name,
                   object   like tadir-object,
                   devclass like tadir-devclass,
                end of tadir_component.
          select obj_name object devclass from tadir into tadir_component for all entries in processed_packages
            where pgmid = 'R3TR'
              and object = object
              and devclass = processed_packages-devclass.

            read table g_tadir_components_mapping into map with table key object = tadir_component-object.
            if sy-subrc = 0.

              component_info-component = map-component.
            endif.
            component_info-component_name = tadir_component-obj_name.
            component_info-package = tadir_component-devclass.
            insert component_info into table components_infos.
*                components_infos = value #( base components_infos ( COMPONENT = g_tadir_components_mapping[ OBJECT = tadir_component-object ]-COMPONENT
*                                                                    component_name = tadir_component-obj_name
*                                                                    package = tadir_component-devclass ) ).

          endselect.
        else.
          select obj_name object devclass from tadir into tadir_component
            where pgmid = 'R3TR'
              and object = object
              and obj_name in s_compsn
              and devclass in s_pack.
            read table g_tadir_components_mapping into map with table key object = tadir_component-object.
            if sy-subrc = 0.

              component_info-component = map-component.
            endif.
            component_info-component_name = tadir_component-obj_name.
            component_info-package = tadir_component-devclass.
            insert component_info into table components_infos.

*            components_infos = value #( base components_infos ( COMPONENT = g_tadir_components_mapping[ OBJECT = tadir_component-object ]-COMPONENT
*                                                                component_name = tadir_component-obj_name
*                                                                package = tadir_component-devclass ) ).

          endselect.
        endif.
      enddo.
    endif.

    if lines( components_infos ) eq 0.
      write: 'Nothing selected '.
      nothing_selected  = true.
    endif.

  endmethod.                    "_select_requested_components

endclass.                    "cl_extract_sap IMPLEMENTATION

start-of-selection.

  data: mse_model type cl_model=>lines_type.
  if g_parameter_extract_from_sap eq false.
    cl_make_demo_model=>make( importing mse_model = mse_model ).
  else.
*    data(sap_extractor) = new cl_extract_sap( ).
    data sap_extractor type ref to cl_extract_sap.
    create object sap_extractor.

*    data(nothing_done) = sap_extractor->extract( importing mse_model = mse_model ).
    data nothing_done type boolean.
    sap_extractor->extract( importing mse_model    = mse_model
                                      nothing_done = nothing_done ).
  endif.

  if nothing_done eq true.
    return.
  endif.


*  data(model_outputer) = new cl_output_model( ).
  data model_outputer type ref to cl_output_model.
  create object model_outputer.
  model_outputer->make( mse_model = mse_model ).