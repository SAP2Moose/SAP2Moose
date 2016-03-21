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

"! Last activation:
"! 21.03.2016 23:04 issue17 Rainer Winkler
"!
"! Keep logic compatible to ABAP 7.31 to allow also conversion into the other direction
REPORT z_release_translator.

"! Redefines abap_bool to simplify coding (Not always reading abap_...)
TYPES bool TYPE abap_bool.
CONSTANTS:
  "! Redefines abap_true to simplify coding (Not always reading abap_...)
  true  TYPE bool VALUE abap_true,
  "! Redefines abap_false to simplify coding (Not always reading abap_...)
  false TYPE bool VALUE abap_false.

TYPES: stringtable TYPE STANDARD TABLE OF string WITH DEFAULT KEY.



CLASS cl_read DEFINITION.
  PUBLIC SECTION.
    METHODS do
      RETURNING VALUE(source) TYPE stringtable.
ENDCLASS.

CLASS cl_read IMPLEMENTATION.

  METHOD do.
    READ REPORT 'YRW1_MOOSE_EXTRACTOR' INTO source.
  ENDMETHOD.

ENDCLASS.

CLASS cl_download DEFINITION.
  PUBLIC SECTION.
    METHODS do
      IMPORTING source TYPE stringtable.
ENDCLASS.

CLASS cl_download IMPLEMENTATION.

  METHOD do.
    " Download the file

    DATA: filename    TYPE string,
          pathname    TYPE string,
          fullpath    TYPE string,
          user_action TYPE i.



    cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension = 'abap'
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
          data_tab = source.

    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_conversion DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF codeline_type,
             code      TYPE string,
             condensed TYPE string,
           END OF codeline_type.
    TYPES: codelines_type TYPE STANDARD TABLE OF codeline_type WITH KEY code.
    TYPES: BEGIN OF replace_type,
             replace_id TYPE i,
             only_once  TYPE true,
             abap_740   TYPE codelines_type,
             abap_731   TYPE codelines_type,
             replaced   TYPE i,
           END OF replace_type.
    TYPES: replaces_type TYPE STANDARD TABLE OF replace_type WITH KEY replace_id.
    DATA: g_replaces TYPE replaces_type.
    METHODS constructor.
    METHODS get_conversion
      RETURNING VALUE(replaces) TYPE replaces_type.
ENDCLASS.

CLASS cl_conversion IMPLEMENTATION.

  METHOD constructor.

    DATA: codeline           TYPE codeline_type,
          codelines_abap_740 TYPE codelines_type,
          codelines_abap_731 TYPE codelines_type,
          replace            TYPE replace_type,
          replace_id         TYPE i.

    replace_id = 1.

    DEFINE start_building_table.
      CLEAR codelines_abap_740.
      CLEAR codelines_abap_731.
    END-OF-DEFINITION.

    DEFINE add_abap_740.
      codeline-code = &1.
      codeline-condensed = &1.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed to UPPER CASE.
      APPEND codeline TO codelines_abap_740.
    END-OF-DEFINITION.

    DEFINE add_abap_731.
      codeline-code = &1.
      codeline-condensed = &1.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed to UPPER CASE.
      APPEND codeline TO codelines_abap_731.
    END-OF-DEFINITION.

    DEFINE only_once.

      replace-only_once = true.

    END-OF-DEFINITION.

    DEFINE add_replace.

      replace-replace_id = replace_id.
      replace-abap_740 = codelines_abap_740.
      replace-abap_731 = codelines_abap_731.

      g_replaces = value #( base g_replaces ( replace ) ).

      CLEAR codelines_abap_740.
      CLEAR codelines_abap_731.

      CLEAR replace.

      ADD 1 TO replace_id.

    END-OF-DEFINITION.

    DATA c TYPE string.

    start_building_table.


    c = |"! This is the original version implemented in ABAP 7.40 (Do not change this line, it will automatically be converted to the ABAP 7.31 statement)|. add_abap_740 c.

    c = |"! This is a copy migrated to ABAP 7.31                                                                                                          |. add_abap_731 c.
    c = |"!                                                                                                                                               |. add_abap_731 c.
    c = |"! Thanks to Enno Wulff for providing the initial ABAP 7.31 version                                                                              |. add_abap_731 c.
    add_replace.

    c = |      EXPORTING VALUE(exists_already_with_id) TYPE i |. add_abap_740 c.
    c = |      RETURNING VALUE(processed_id)           TYPE i.|. add_abap_740 c.

    c = |      EXPORTING value(exists_already_with_id) TYPE i |. add_abap_731 c.
    c = |                value(processed_id)           TYPE i.|. add_abap_731 c.
    add_replace.

    " CLASS cl_model

    c = |  METHOD add_entity.                                                                                                                              |. add_abap_740 c.
    c = |                                                                                                                                                  |. add_abap_740 c.
    c = |    IF can_be_referenced_by_name EQ true.                                                                                                         |. add_abap_740 c.
    c = |                                                                                                                                                  |. add_abap_740 c.
    c = |      READ TABLE g_named_entities ASSIGNING FIELD-SYMBOL(<ls_name>) WITH TABLE KEY elementname = elementname name_group = name_group xname = name.|. add_abap_740 c.


    c = |  METHOD add_entity.                                                                                                                              |. add_abap_731 c.
    c = |                                                                                                                                                  |. add_abap_731 c.
    c = |    FIELD-SYMBOLS <ls_name> LIKE LINE OF g_named_entities.                                                                                        |. add_abap_731 c.

    c = |                                                                                                                                                  |. add_abap_731 c.
    c = |    IF can_be_referenced_by_name EQ true.                                                                                                         |. add_abap_731 c.
    c = |                                                                                                                                                  |. add_abap_731 c.
    c = |      READ TABLE g_named_entities ASSIGNING <ls_name>                                                                                             |. add_abap_731 c.
    c = |            WITH TABLE KEY elementname = elementname name_group = name_group xname = name.                                                        |. add_abap_731 c.
    add_replace.



    c = |      g_named_entities = VALUE #( BASE g_named_entities ( elementname = elementname name_group = name_group xname = name id = g_processed_id ) ).|. add_abap_740 c.

    c = |      DATA ls_named_entity    LIKE LINE OF g_named_entities.  " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion                   |. add_abap_731 c.
    c = |      CLEAR ls_named_entity.                                                                                                                     |. add_abap_731 c.
    c = |      ls_named_entity-elementname = elementname.                                                                                                 |. add_abap_731 c.
    c = |      ls_named_entity-name_group  = name_group.                                                                                                  |. add_abap_731 c.
    c = |      ls_named_entity-xname       = name.                                                                                                        |. add_abap_731 c.
    c = |      ls_named_entity-id          = g_processed_id.                                                                                              |. add_abap_731 c.
    c = |      INSERT ls_named_entity INTO TABLE g_named_entities.                                                                                        |. add_abap_731 c.
    add_replace.

    c = |    g_elements_in_model = VALUE #( BASE g_elements_in_model ( id = g_processed_id                                               |. add_abap_740 c.
    c = |                                                              is_named_entity = is_named_entity                                 |. add_abap_740 c.
    c = |                                                              elementname = elementname ) ).                                    |. add_abap_740 c.

    c = |    DATA ls_elements_in_model LIKE LINE OF g_elements_in_model. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_elements_in_model.                                                                                                 |. add_abap_731 c.
    c = |    ls_elements_in_model-id = g_processed_id.                                                                                   |. add_abap_731 c.
    c = |    ls_elements_in_model-is_named_entity = is_named_entity.                                                                     |. add_abap_731 c.
    c = |    ls_elements_in_model-elementname = elementname.                                                                             |. add_abap_731 c.
    c = |    INSERT ls_elements_in_model INTO TABLE g_elements_in_model.                                                                 |. add_abap_731 c.
    add_replace.

    " METHOD make_mse.

    c = |    DATA(is_first) = true.                |. add_abap_740 c.

    c = |    DATA is_first TYPE boolean VALUE true.|. add_abap_731 c. "'.
    add_replace.

    c = |    LOOP AT g_elements_in_model ASSIGNING FIELD-SYMBOL(<element_in_model>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <element_in_model> LIKE LINE OF g_elements_in_model.     |. add_abap_731 c.
    c = |                                                                           |. add_abap_731 c.
    c = |    LOOP AT g_elements_in_model ASSIGNING <element_in_model>.              |. add_abap_731 c.
    add_replace.

    c = |        mse_model = VALUE #( BASE mse_model ( mse_model_line ) ).|. add_abap_740 c.

    c = |        APPEND mse_model_line TO mse_model.                      |. add_abap_731 c.

    add_replace.

    c = |      LOOP AT g_attributes ASSIGNING FIELD-SYMBOL(<attribute>) WHERE id = <element_in_model>-id.|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <attribute> LIKE LINE OF g_attributes.                                      |. add_abap_731 c.
    c = |      LOOP AT g_attributes ASSIGNING <attribute> WHERE id = <element_in_model>-id.              |. add_abap_731 c.
    add_replace.

    c = |        mse_model_line = VALUE #( ).|. add_abap_740 c.

    c = |        CLEAR mse_model_line.       |. add_abap_731 c.
    add_replace.

    " METHOD add_reference.

    c = |    READ TABLE g_named_entities ASSIGNING FIELD-SYMBOL(<named_entity>) WITH TABLE KEY elementname = elementname           |. add_abap_740 c.
    c = |                                                                                      name_group = name_group_of_reference|. add_abap_740 c.
    c = |                                                                                      xname = name_of_reference.          |. add_abap_740 c.

    c = |    FIELD-SYMBOLS <named_entity> LIKE LINE OF g_named_entities.                                                           |. add_abap_731 c.
    c = |                                                                                                                          |. add_abap_731 c.
    c = |    READ TABLE g_named_entities ASSIGNING <named_entity> WITH TABLE KEY elementname = elementname                         |. add_abap_731 c.
    c = |                                                                                      name_group = name_group_of_reference|. add_abap_731 c.
    c = |                                                                                      xname = name_of_reference.          |. add_abap_731 c.
    add_replace.



    c = |    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id                                   |. add_abap_740 c.
    c = |                                                attribute_id   = g_attribute_id                                   |. add_abap_740 c.
    c = |                                                attribute_name = attribute_name                                   |. add_abap_740 c.
    c = |                                                value_type     = reference_value                                  |. add_abap_740 c.
    c = |                                                reference      = <named_entity>-id ) ).                           |. add_abap_740 c.

    c = |    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion |. add_abap_731 c.
    c = |    CLEAR ls_attribute.                                                                                           |. add_abap_731 c.
    c = |    ls_attribute-id             = g_processed_id.                                                                 |. add_abap_731 c.
    c = |    ls_attribute-attribute_id   = g_attribute_id.                                                                 |. add_abap_731 c.
    c = |    ls_attribute-attribute_name = attribute_name.                                                                 |. add_abap_731 c.
    c = |    ls_attribute-value_type     = reference_value.                                                                |. add_abap_731 c.
    c = |    ls_attribute-reference      = <named_entity>-id.                                                              |. add_abap_731 c.
    c = |    APPEND ls_attribute TO g_attributes.                                                                          |. add_abap_731 c.
    add_replace.

    " METHOD add_reference_by_id.

    c = |    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id                                  |. add_abap_740 c.
    c = |                                                attribute_id   = g_attribute_id                                  |. add_abap_740 c.
    c = |                                                attribute_name = attribute_name                                  |. add_abap_740 c.
    c = |                                                value_type     = reference_value                                 |. add_abap_740 c.
    c = |                                                reference      = reference_id ) ).                               |. add_abap_740 c.

    c = |    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_attribute.                                                                                          |. add_abap_731 c.
    c = |    ls_attribute-id             = g_processed_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_id   = g_attribute_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_name = attribute_name.                                                                |. add_abap_731 c.
    c = |    ls_attribute-value_type     = reference_value.                                                               |. add_abap_731 c.
    c = |    ls_attribute-reference      = reference_id.                                                                  |. add_abap_731 c.
    c = |    APPEND ls_attribute TO g_attributes.                                                                         |. add_abap_731 c.
    add_replace.

    " METHOD add_string.

    c = |    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id                                  |. add_abap_740 c.
    c = |                                                attribute_id   = g_attribute_id                                  |. add_abap_740 c.
    c = |                                                attribute_name = attribute_name                                  |. add_abap_740 c.
    c = |                                                value_type     = string_value                                    |. add_abap_740 c.
    c = |                                                string         = string ) ).                                     |. add_abap_740 c.

    c = |    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_attribute.                                                                                          |. add_abap_731 c.
    c = |    ls_attribute-id             = g_processed_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_id   = g_attribute_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_name = attribute_name.                                                                |. add_abap_731 c.
    c = |    ls_attribute-value_type     = string_value.                                                                  |. add_abap_731 c.
    c = |    ls_attribute-string         = string.                                                                        |. add_abap_731 c.
    c = |    APPEND ls_attribute TO g_attributes.                                                                         |. add_abap_731 c.
    add_replace.

    " METHOD add_boolean.

    c = |    g_attributes = VALUE #( BASE g_attributes ( id             = g_processed_id                                  |. add_abap_740 c.
    c = |                                                attribute_id   = g_attribute_id                                  |. add_abap_740 c.
    c = |                                                attribute_name = attribute_name                                  |. add_abap_740 c.
    c = |                                                value_type     = boolean_value                                   |. add_abap_740 c.
    c = |                                                boolean        = is_true ) ).                                    |. add_abap_740 c.

    c = |    DATA ls_attribute LIKE LINE OF g_attributes. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_attribute.                                                                                          |. add_abap_731 c.
    c = |    ls_attribute-id             = g_processed_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_id   = g_attribute_id.                                                                |. add_abap_731 c.
    c = |    ls_attribute-attribute_name = attribute_name.                                                                |. add_abap_731 c.
    c = |    ls_attribute-value_type     = boolean_value.                                                                 |. add_abap_731 c.
    c = |    ls_attribute-boolean        = is_true.                                                                       |. add_abap_731 c.
    c = |    APPEND ls_attribute TO g_attributes.                                                                         |. add_abap_731 c.
    add_replace.

    " CLASS cl_output_model
    c = |    LOOP AT mse_model ASSIGNING FIELD-SYMBOL(<mse_model_line>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <mse_model_line> LIKE LINE OF mse_model.     |. add_abap_731 c. "'.
    c = |    LOOP AT mse_model ASSIGNING <mse_model_line>.              |. add_abap_731 c. "'.
    add_replace.

    " CLASS cl_famix_named_entity

    c = |      EXPORTING VALUE(exists_already_with_id) TYPE i |. add_abap_740 c.
    c = |      RETURNING VALUE(id)                     TYPE i.|. add_abap_740 c.

    c = |      EXPORTING value(exists_already_with_id) TYPE i |. add_abap_731 c.
    c = |                value(id)                     TYPE i.|. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_named_entity

    c = |    id = g_model->add_entity( EXPORTING elementname = g_elementname                       |. add_abap_740 c.
    c = |                                        is_named_entity = true                            |. add_abap_740 c.
    c = |                                        can_be_referenced_by_name = true                  |. add_abap_740 c.
    c = |                                        name_group = name_group                           |. add_abap_740 c.
    c = |                                        name = name                                       |. add_abap_740 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id ).|. add_abap_740 c.

    c = |    g_model->add_entity( EXPORTING elementname = g_elementname                            |. add_abap_731 c.
    c = |                                        is_named_entity = true                            |. add_abap_731 c.
    c = |                                        can_be_referenced_by_name = true                  |. add_abap_731 c.
    c = |                                        name_group = name_group                           |. add_abap_731 c.
    c = |                                        name = name                                       |. add_abap_731 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id   |. add_abap_731 c.
    c = |                                        processed_id = id ).                              |. add_abap_731 c.
    add_replace.

    " METHOD add.

    c = |    id = g_model->add_entity( EXPORTING elementname = g_elementname                       |. add_abap_740 c.
    c = |                                        is_named_entity = true                            |. add_abap_740 c.
    c = |                                        can_be_referenced_by_name = false                 |. add_abap_740 c.
    c = |                                        name = name                                       |. add_abap_740 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id ).|. add_abap_740 c.

    c = |    g_model->add_entity( EXPORTING elementname = g_elementname                            |. add_abap_731 c.
    c = |                                        is_named_entity = true                            |. add_abap_731 c.
    c = |                                        can_be_referenced_by_name = false                 |. add_abap_731 c.
    c = |                                        name = name                                       |. add_abap_731 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id   |. add_abap_731 c.
    c = |                                        processed_id = id ).                              |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_attribute
    " METHOD add.
    c = |    id = g_model->add_entity( elementname = g_elementname      |. add_abap_740 c.
    c = |                              is_named_entity = true           |. add_abap_740 c.
    c = |                              can_be_referenced_by_name = false|. add_abap_740 c.
    c = |                              name = name ).                   |. add_abap_740 c.

    c = |    g_model->add_entity(                                       |. add_abap_731 c.
    c = |               EXPORTING elementname = g_elementname           |. add_abap_731 c.
    c = |                         is_named_entity = true                |. add_abap_731 c.
    c = |                         can_be_referenced_by_name = false     |. add_abap_731 c.
    c = |                         name = name                           |. add_abap_731 c.
    c = |               IMPORTING processed_id = id ).                  |. add_abap_731 c.
    add_replace.

    " METHOD store_id.

    c = |    g_attribute_ids = VALUE #( BASE g_attribute_ids ( id        = g_last_used_id                                       |. add_abap_740 c.
    c = |                                                    class     = class                                                  |. add_abap_740 c.
    c = |                                                    attribute = attribute ) ).                                         |. add_abap_740 c.

    c = |    DATA ls_attribute_id LIKE LINE OF g_attribute_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_attribute_id.                                                                                             |. add_abap_731 c.
    c = |    ls_attribute_id-id = g_last_used_id.                                                                               |. add_abap_731 c.
    c = |    ls_attribute_id-class = class.                                                                                     |. add_abap_731 c.
    c = |    ls_attribute_id-attribute = attribute.                                                                             |. add_abap_731 c.
    c = |    INSERT ls_attribute_id INTO TABLE g_attribute_ids.                                                                 |. add_abap_731 c.
    add_replace.

    " METHOD get_id.

    c = |    READ TABLE g_attribute_ids ASSIGNING FIELD-SYMBOL(<attribute_id>) WITH TABLE KEY class = class attribute = attribute.|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <attribute_id> LIKE LINE OF g_attribute_ids.                                                           |. add_abap_731 c.
    c = |                                                                                                                         |. add_abap_731 c.
    c = |    READ TABLE g_attribute_ids ASSIGNING <attribute_id> WITH TABLE KEY class = class attribute = attribute.              |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_package
    " METHOD add.

    c = |    id = g_model->add_entity( EXPORTING elementname = g_elementname                       |. add_abap_740 c.
    c = |                                        is_named_entity = true                            |. add_abap_740 c.
    c = |                                        can_be_referenced_by_name = true                  |. add_abap_740 c.
    c = |                                        name = name                                       |. add_abap_740 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id ).|. add_abap_740 c.

    c = |    g_model->add_entity( EXPORTING elementname = g_elementname                            |. add_abap_731 c.
    c = |                                        is_named_entity = true                            |. add_abap_731 c.
    c = |                                        can_be_referenced_by_name = true                  |. add_abap_731 c.
    c = |                                        name = name                                       |. add_abap_731 c.
    c = |                              IMPORTING exists_already_with_id = exists_already_with_id   |. add_abap_731 c.
    c = |                                        processed_id = id ).                              |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_module
    " METHOD add.

    " METHOD store_id.

    c = |    g_method_ids = VALUE #( BASE g_method_ids ( id    = g_last_used_id                                           |. add_abap_740 c.
    c = |                                                class = class method = method ) ).                               |. add_abap_740 c.

    c = |    DATA ls_method_id LIKE LINE OF g_method_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_method_id.                                                                                          |. add_abap_731 c.
    c = |    ls_method_id-id = g_last_used_id.                                                                            |. add_abap_731 c.
    c = |    ls_method_id-class = class.                                                                                  |. add_abap_731 c.
    c = |    ls_method_id-method = method.                                                                                |. add_abap_731 c.
    c = |    INSERT ls_method_id INTO TABLE g_method_ids.                                                                 |. add_abap_731 c.
    add_replace.

    " METHOD get_id.

    c = |    READ TABLE g_method_ids ASSIGNING FIELD-SYMBOL(<method_id>) WITH TABLE KEY class = class      |. add_abap_740 c.
    c = |                                                                                  method = method.|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <method_id> LIKE LINE OF g_method_ids.                                          |. add_abap_731 c.
    c = |                                                                                                  |. add_abap_731 c.
    c = |    READ TABLE g_method_ids ASSIGNING <method_id> WITH TABLE KEY class = class                    |. add_abap_731 c.
    c = |                                                               method = method.                   |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_association
    " METHOD add.

    c = |    id = g_model->add_entity( EXPORTING elementname               = g_elementname|. add_abap_740 c.
    c = |                                        is_named_entity           = false        |. add_abap_740 c.
    c = |                                        can_be_referenced_by_name = false ).     |. add_abap_740 c.

    c = |    g_model->add_entity( EXPORTING elementname               = g_elementname     |. add_abap_731 c.
    c = |                                        is_named_entity           = false        |. add_abap_731 c.
    c = |                                        can_be_referenced_by_name = false        |. add_abap_731 c.
    c = |                                        IMPORTING processed_id = id ).           |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_access
    " METHOD set_accessor_variable_relation.

    c = |    g_accessor_variable_ids = VALUE #( BASE g_accessor_variable_ids ( accessor_id = accessor_id variable_id = variable_id ) ).|. add_abap_740 c.

    c = |    DATA ls_accessor_id LIKE LINE OF g_accessor_variable_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_accessor_id.                                                                                                     |. add_abap_731 c.
    c = |    ls_accessor_id-accessor_id = accessor_id.                                                                                 |. add_abap_731 c.
    c = |    ls_accessor_id-variable_id = variable_id.                                                                                 |. add_abap_731 c.
    c = |    INSERT ls_accessor_id INTO TABLE g_accessor_variable_ids.                                                                 |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_invocation
    " METHOD set_invocation_by_reference.

    c = |      g_sender_candidates = VALUE #( BASE g_sender_candidates ( sender_id = sender_id candidates_id = candidates_id ) ).         |. add_abap_740 c.

    c = |      DATA ls_sender_candidate LIKE LINE OF g_sender_candidates. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |      CLEAR ls_sender_candidate.                                                                                                 |. add_abap_731 c.
    c = |      ls_sender_candidate-sender_id = sender_id.                                                                                 |. add_abap_731 c.
    c = |      ls_sender_candidate-candidates_id = candidates_id.                                                                         |. add_abap_731 c.
    c = |      INSERT ls_sender_candidate INTO TABLE g_sender_candidates.                                                                 |. add_abap_731 c.
    add_replace.

    " CLASS cl_famix_custom_source_lang
    " METHOD add.

    " CLASS cl_make_demo_model
    " METHOD make.

    c = |    DATA(famix_namespace) = NEW cl_famix_namespace( model ).|. add_abap_740 c.

    c = |    DATA famix_namespace  TYPE REF TO cl_famix_namespace.   |. add_abap_731 c.
    c = |    CREATE OBJECT famix_namespace EXPORTING model = model.  |. add_abap_731 c.
    add_replace.

    c = |    DATA(famix_package) = NEW cl_famix_package( model ). |. add_abap_740 c.

    c = |    DATA famix_package      TYPE REF TO cl_famix_package.|. add_abap_731 c.
    c = |    CREATE OBJECT famix_package EXPORTING model = model. |. add_abap_731 c.
    add_replace.

    c = |    DATA(famix_class) = NEW cl_famix_class( model ).   |. add_abap_740 c.

    c = |    DATA famix_class        TYPE REF TO cl_famix_class.|. add_abap_731 c.
    c = |    CREATE OBJECT famix_class EXPORTING model = model. |. add_abap_731 c.
    add_replace.

    c = |    DATA(famix_method) = NEW cl_famix_method( model ).    |. add_abap_740 c.

    c = |    DATA famix_method         TYPE REF TO cl_famix_method.|. add_abap_731 c.
    c = |    CREATE OBJECT famix_method EXPORTING model = model.   |. add_abap_731 c.
    add_replace.

    c = |    DATA(famix_attribute) = NEW cl_famix_attribute( model ).|. add_abap_740 c.

    c = |    DATA famix_attribute    TYPE REF TO cl_famix_attribute. |. add_abap_731 c.
    c = |    CREATE OBJECT famix_attribute EXPORTING model = model.  |. add_abap_731 c.
    add_replace.

    c = |    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).|. add_abap_740 c.

    c = |    DATA famix_inheritance  TYPE REF TO cl_famix_inheritance.   |. add_abap_731 c.
    c = |    CREATE OBJECT famix_inheritance EXPORTING model = model.    |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_package
    " METHOD constructor.

    c = |    g_famix_package = NEW cl_famix_package( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_package EXPORTING model = model.  |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_class

    " CLASS cl_sap_class
    " METHOD constructor.

    c = |    g_famix_class = NEW cl_famix_class( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_class EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    " METHOD add.

    c = |    id = g_famix_class->add( EXPORTING name_group             = ''                       |. add_abap_740 c.
    c = |                                       name                   = name                     |. add_abap_740 c.
    c = |                             IMPORTING exists_already_with_id = exists_already_with_id ).|. add_abap_740 c.

    c = |    g_famix_class->add( EXPORTING name_group             = ''                            |. add_abap_731 c.
    c = |                                       name                   = name                     |. add_abap_731 c.
    c = |                             IMPORTING exists_already_with_id = exists_already_with_id   |. add_abap_731 c.
    c = |                                  id = id ).                                             |. add_abap_731 c.
    add_replace.

    " METHOD add.

    c = |    id = g_famix_class->add( EXPORTING name_group = program      |. add_abap_740 c.
    c = |                                             name       = name ).|. add_abap_740 c.


    c = |    g_famix_class->add( EXPORTING name_group = program           |. add_abap_731 c.
    c = |                                  name       = name              |. add_abap_731 c.
    c = |                        IMPORTING id = id ).                     |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_attribute
    " METHOD constructor.

    c = |    g_famix_attribute = NEW cl_famix_attribute( model = model ).|. add_abap_740 c. "'.

    c = |    CREATE OBJECT g_famix_attribute EXPORTING model = model.    |. add_abap_731 c. "'.
    add_replace.

    " CLASS cl_sap_method
    " METHOD constructor.

    c = |    g_famix_method = NEW cl_famix_method( model = model ).|. add_abap_740 c. "'.

    c = |    CREATE OBJECT g_famix_method EXPORTING model = model. |. add_abap_731 c. "'.
    add_replace.

    " METHOD add.

    c = |    id = g_famix_method->add( name = method ).                       |. add_abap_740 c.

    c = |    g_famix_method->add( EXPORTING name = method IMPORTING id = id ).|. add_abap_731 c.
    add_replace.

    " METHOD add_local_method.

    c = |    id = g_famix_method->add( EXPORTING name_group = class_name " TBD Why name of class in name_group?|. add_abap_740 c.
    c = |                                        name       = method_name ).                                   |. add_abap_740 c.

    c = |    g_famix_method->add( EXPORTING name_group = class_name " TBD Why name of class in name_group?     |. add_abap_731 c.
    c = |                                        name       = method_name                                      |. add_abap_731 c.
    c = |                                        IMPORTING id = id ).                                          |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_inheritance
    " METHOD constructor.

    c = |    g_famix_inheritance = NEW cl_famix_inheritance( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_inheritance EXPORTING model = model.      |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_invocation
    " METHOD constructor.

    c = |    g_famix_invocation = NEW cl_famix_invocation( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_invocation EXPORTING model = model.     |. add_abap_731 c.
    add_replace.

    " METHOD add_invocation.

    c = |    IF g_famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method_id  |. add_abap_740 c.
    c = |                                                           candidates_id = used_method_id ).|. add_abap_740 c.

    c = |    IF g_famix_invocation->is_new_invocation_to_candidate( sender_id     = using_method_id  |. add_abap_731 c.
    c = |                                                           candidates_id = used_method_id ) |. add_abap_731 c.
    c = |       EQ true.                                                                             |. add_abap_731 c.
    add_replace.

    " CLASS cl_sap_access

    c = |    g_famix_access = NEW cl_famix_access( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_access EXPORTING model = model. |. add_abap_731 c.
    add_replace.

    " METHOD add_access.

    c = |    IF g_famix_access->is_new_access( accessor_id = using_method     |. add_abap_740 c.
    c = |                                      variable_id = used_attribute ).|. add_abap_740 c.

    c = |    IF g_famix_access->is_new_access( accessor_id = using_method     |. add_abap_731 c.
    c = |                                      variable_id = used_attribute ) |. add_abap_731 c.
    c = |       EQ true.                                                      |. add_abap_731 c.
    add_replace.


    " CLASS cl_sap_program
    " METHOD constructor.

    c = |    g_famix_module = NEW cl_famix_module( model = model ).|. add_abap_740 c.

    c = |    CREATE OBJECT g_famix_module EXPORTING model = model. |. add_abap_731 c.
    add_replace.

    " METHOD add.

    c = |    id = g_famix_module->add( name = name ).                       |. add_abap_740 c.

    c = |    g_famix_module->add( EXPORTING name = name IMPORTING id = id ).|. add_abap_731 c.
    add_replace.

    " CLASS cl_extract_sap

    c = |    METHODS extract                                          |. add_abap_740 c.
    c = |      EXPORTING                                              |. add_abap_740 c.
    c = |                mse_model           TYPE cl_model=>lines_type|. add_abap_740 c.
    c = |      RETURNING VALUE(nothing_done) TYPE bool.               |. add_abap_740 c.

    c = |    METHODS extract                                          |. add_abap_731 c.
    c = |      EXPORTING                                              |. add_abap_731 c.
    c = |                mse_model           TYPE cl_model=>lines_type|. add_abap_731 c.
    c = |                value(nothing_done) TYPE bool.               |. add_abap_731 c.
    add_replace.

    " CLASS cl_ep_analyze_other_keyword
    " METHOD analyze.

    c = |    g_info = VALUE #( ).|. add_abap_740 c.

    c = |    CLEAR g_info.       |. add_abap_731 c.
    add_replace.

    c = |    READ TABLE g_sorted_tokens ASSIGNING FIELD-SYMBOL(<token>) WITH TABLE KEY index = statement-from.|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <token> LIKE LINE OF g_sorted_tokens.                                              |. add_abap_731 c.
    c = |    READ TABLE g_sorted_tokens ASSIGNING <token> WITH TABLE KEY index = statement-from.              |. add_abap_731 c.
    add_replace.

    c = |      DATA(position_of_name) = statement-from + 1.|. add_abap_740 c.

    c = |      DATA position_of_name TYPE i.               |. add_abap_731 c.
    c = |      position_of_name =  statement-from + 1.     |. add_abap_731 c.
    add_replace.


    c = |              DATA(superclass_is_at) = sy-tabix + 2.|. add_abap_740 c.

    c = |              DATA superclass_is_at TYPE i.         |. add_abap_731 c.
    c = |              superclass_is_at  = sy-tabix + 2.     |. add_abap_731 c.
    add_replace.

    c = |              READ TABLE g_sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_superclass_token>) WITH TABLE KEY index = superclass_is_at.|. add_abap_740 c.

    c = |              FIELD-SYMBOLS <ls_superclass_token> LIKE LINE OF g_sorted_tokens.                                                |. add_abap_731 c.
    c = |              READ TABLE g_sorted_tokens ASSIGNING <ls_superclass_token> WITH TABLE KEY index = superclass_is_at.              |. add_abap_731 c.
    add_replace.

    " CLASS cl_program_analyzer
    " METHOD extract.

    c = |    LOOP AT tokens ASSIGNING FIELD-SYMBOL(<ls_token_2>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <ls_token_2> LIKE LINE OF tokens.     |. add_abap_731 c.
    c = |    LOOP AT tokens ASSIGNING <ls_token_2>.              |. add_abap_731 c.

    add_replace.

    c = |      sorted_tokens = VALUE #( BASE sorted_tokens ( index = sy-tabix                                            |. add_abap_740 c.
    c = |                                                    str   = <ls_token_2>-str                                    |. add_abap_740 c.
    c = |                                                    row   = <ls_token_2>-row                                    |. add_abap_740 c.
    c = |                                                    col   = <ls_token_2>-col                                    |. add_abap_740 c.
    c = |                                                    type  = <ls_token_2>-type ) ).                              |. add_abap_740 c.

    c = |      DATA ls_token LIKE LINE OF sorted_tokens. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |      CLEAR ls_token.                                                                                           |. add_abap_731 c.
    c = |      ls_token-index = sy-tabix.                                                                                |. add_abap_731 c.
    c = |      ls_token-str   = <ls_token_2>-str.                                                                        |. add_abap_731 c.
    c = |      ls_token-row   = <ls_token_2>-row.                                                                        |. add_abap_731 c.
    c = |      ls_token-col   = <ls_token_2>-col.                                                                        |. add_abap_731 c.
    c = |      ls_token-type  = <ls_token_2>-type.                                                                       |. add_abap_731 c.
    c = |      INSERT ls_token INTO TABLE sorted_tokens.                                                                 |. add_abap_731 c.
    add_replace.

    c = |    aok = NEW cl_ep_analyze_other_keyword( sorted_tokens = sorted_tokens ).|. add_abap_740 c.

    c = |    CREATE OBJECT aok EXPORTING sorted_tokens = sorted_tokens.             |. add_abap_731 c.
    add_replace.

    c = |    LOOP AT statements ASSIGNING FIELD-SYMBOL(<statement>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <statement> LIKE LINE OF statements.     |. add_abap_731 c.
    c = |    LOOP AT statements ASSIGNING <statement>.              |. add_abap_731 c.
    add_replace.

    c = |              classes_with_model_id = VALUE #( BASE classes_with_model_id ( actual_class_with_model_id ) ).|. add_abap_740 c.

    c = |              INSERT actual_class_with_model_id INTO TABLE classes_with_model_id.                          |. add_abap_731 c.
    add_replace.

    c = |                inheritances = VALUE #( BASE inheritances ( subclass = actual_class_with_model_id-classname                      |. add_abap_740 c.
    c = |                                                                  superclass = aok->g_info-class_inherits_from ) ).              |. add_abap_740 c.

    c = |                DATA ls_inheritance_2 LIKE LINE OF inheritances. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |                CLEAR ls_inheritance_2.                                                                                          |. add_abap_731 c.
    c = |                ls_inheritance_2-subclass = actual_class_with_model_id-classname.                                                |. add_abap_731 c.
    c = |                ls_inheritance_2-superclass = aok->g_info-class_inherits_from.                                                   |. add_abap_731 c.
    c = |                INSERT ls_inheritance_2 INTO TABLE inheritances.                                                                 |. add_abap_731 c.
    add_replace.

    c = |              context-implementation_of_class = VALUE #( ).|. add_abap_740 c.

    c = |              CLEAR context-implementation_of_class.       |. add_abap_731 c.
    add_replace.

    c = |                actual_method = VALUE #( classname = actual_class_with_model_id-classname|. add_abap_740 c.
    c = |                                         in_section = context-in_section ).              |. add_abap_740 c.

    c = |                CLEAR actual_method.                                                     |. add_abap_731 c.
    c = |                actual_method-classname = actual_class_with_model_id-classname.          |. add_abap_731 c.
    c = |                actual_method-in_section = context-in_section.                           |. add_abap_731 c.
    add_replace.

    c = |                actual_method = VALUE #( classname = actual_class_with_model_id-classname|. add_abap_740 c.
    c = |                                         in_section = context-in_section                 |. add_abap_740 c.
    c = |                                         instanciable = true ).                          |. add_abap_740 c.

    c = |                CLEAR actual_method.                                                     |. add_abap_731 c.
    c = |                actual_method-classname = actual_class_with_model_id-classname.          |. add_abap_731 c.
    c = |                actual_method-in_section = context-in_section.                           |. add_abap_731 c.
    c = |                actual_method-instanciable = true.                                       |. add_abap_731 c.
    add_replace.

    c = |              context-implementation_of_method = VALUE #( ).|. add_abap_740 c.

    c = |              CLEAR context-implementation_of_method.       |. add_abap_731 c.
    add_replace.

    c = |        LOOP AT sorted_tokens ASSIGNING FIELD-SYMBOL(<token>) WHERE|. add_abap_740 c.
    c = |            index >= <statement>-from                              |. add_abap_740 c.
    c = |        AND index <= <statement>-to.                               |. add_abap_740 c.

    c = |        FIELD-SYMBOLS <token> LIKE LINE OF sorted_tokens.          |. add_abap_731 c.
    c = |        LOOP AT sorted_tokens ASSIGNING <token> WHERE              |. add_abap_731 c.
    c = |            index >= <statement>-from                              |. add_abap_731 c.
    c = |        AND index <= <statement>-to.                               |. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_class) = NEW cl_sap_class( model ).    |. add_abap_740 c.

    c = |    DATA sap_class TYPE REF TO cl_sap_class.        |. add_abap_731 c.
    c = |    CREATE OBJECT sap_class EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |    LOOP AT classes_with_model_id ASSIGNING FIELD-SYMBOL(<class>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <class> LIKE LINE OF classes_with_model_id.     |. add_abap_731 c.
    c = |    LOOP AT classes_with_model_id ASSIGNING <class>.              |. add_abap_731 c.
    add_replace.

    c = |      <class>-id_in_model = sap_class->add_local( EXPORTING program = program             |. add_abap_740 c.
    c = |                                                            name    = <class>-classname ).|. add_abap_740 c.

    c = |      <class>-id_in_model = sap_class->add_local( program = program                       |. add_abap_731 c.
    c = |                                                  name    = <class>-classname ).          |. add_abap_731 c.
    add_replace.

    c = |     DATA(sap_method) = NEW cl_sap_method( model ).  |. add_abap_740 c.

    c = |    DATA sap_method TYPE REF TO cl_sap_method.       |. add_abap_731 c.
    c = |    CREATE OBJECT sap_method EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |    LOOP AT methods ASSIGNING FIELD-SYMBOL(<method>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <method> LIKE LINE OF methods.     |. add_abap_731 c.
    c = |    LOOP AT methods ASSIGNING <method>.              |. add_abap_731 c.
    add_replace.

    c = |      READ TABLE classes_with_model_id ASSIGNING FIELD-SYMBOL(<class_2>) WITH TABLE KEY classname = <method>-classname.|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <class_2> LIKE LINE OF classes_with_model_id.                                                      |. add_abap_731 c.
    c = |      READ TABLE classes_with_model_id ASSIGNING <class_2> WITH TABLE KEY classname = <method>-classname.              |. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_inheritance) = NEW cl_sap_inheritance( model ).|. add_abap_740 c.

    c = |    DATA sap_inheritance TYPE REF TO cl_sap_inheritance.    |. add_abap_731 c.
    c = |    CREATE OBJECT sap_inheritance EXPORTING model = model.  |. add_abap_731 c.
    add_replace.

    c = |    DATA(model) = NEW cl_model( ).             |. add_abap_740 c.

    c = |    DATA model            TYPE REF TO cl_model.|. add_abap_731 c.
    c = |    CREATE OBJECT model.                       |. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_package) = NEW cl_sap_package( model ).  |. add_abap_740 c.

    c = |    DATA sap_package     TYPE REF TO cl_sap_package.  |. add_abap_731 c.
    c = |    CREATE OBJECT sap_package EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_program) = NEW cl_sap_program( model ).  |. add_abap_740 c.

    c = |    DATA sap_program     TYPE REF TO cl_sap_program.  |. add_abap_731 c.
    c = |    CREATE OBJECT sap_program EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_attribute) = NEW cl_sap_attribute( model ).|. add_abap_740 c.

    c = |    DATA sap_attribute   TYPE REF TO cl_sap_attribute.  |. add_abap_731 c.
    c = |    CREATE OBJECT sap_attribute EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_invocation) = NEW cl_sap_invocation( model ).|. add_abap_740 c.

    c = |    DATA sap_invocation  TYPE REF TO cl_sap_invocation.   |. add_abap_731 c.
    c = |    CREATE OBJECT sap_invocation EXPORTING model = model. |. add_abap_731 c.
    add_replace.

    c = |    DATA(sap_access) = NEW cl_sap_access( model ).   |. add_abap_740 c.

    c = |    DATA sap_access      TYPE REF TO cl_sap_access.  |. add_abap_731 c.
    c = |    CREATE OBJECT sap_access EXPORTING model = model.|. add_abap_731 c.
    add_replace.

    c = |   g_tadir_components_mapping = VALUE #( ( object = 'CLAS' component = 'GlobClass' )      |. add_abap_740 c.
    c = |                                          ( object = 'INTF' component = 'GlobIntf' )      |. add_abap_740 c.
    c = |                                          ( object = 'PROG' component = 'ABAPProgramm') ).|. add_abap_740 c.

    c = |    DATA ls_mapping TYPE map_tadir_component_type. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_mapping.                                                                              |. add_abap_731 c.
    c = |    ls_mapping-object = 'CLAS'.                                                                                    |. add_abap_731 c.
    c = |    ls_mapping-component = 'GlobClass'.                                                                            |. add_abap_731 c.
    c = |    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.                                                       |. add_abap_731 c.
    c = |                                                                                                                   |. add_abap_731 c.
    c = |    CLEAR ls_mapping.                                                                              |. add_abap_731 c.
    c = |    ls_mapping-object = 'INTF'.                                                                                    |. add_abap_731 c.
    c = |    ls_mapping-component = 'GlobIntf'.                                                                             |. add_abap_731 c.
    c = |    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.                                                       |. add_abap_731 c.
    c = |                                                                                                                   |. add_abap_731 c.
    c = |    CLEAR ls_mapping.                                                                              |. add_abap_731 c.
    c = |    ls_mapping-object = 'PROG'.                                                                                    |. add_abap_731 c.
    c = |    ls_mapping-component = 'ABAPProgram'.                                                                          |. add_abap_731 c.
    c = |    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.                                                       |. add_abap_731 c.
    add_replace.

    c = |      DATA(select_by_top_package) = true.     |. add_abap_740 c.

    c = |      DATA select_by_top_package TYPE boolean.|. add_abap_731 c.
    c = |      select_by_top_package = true.           |. add_abap_731 c.
    add_replace.

    c = |      LOOP AT new_components_infos ASSIGNING FIELD-SYMBOL(<component_infos>).|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <component_infos> LIKE LINE OF new_components_infos.     |. add_abap_731 c.
    c = |      LOOP AT new_components_infos ASSIGNING <component_infos>.              |. add_abap_731 c.
    add_replace.

    c = |        DATA(object) = g_tadir_components_mapping[ KEY comp component = <component_infos>-component ]-object.             |. add_abap_740 c.

    c = |        DATA object TYPE trobjtype.                                                                                       |. add_abap_731 c.
    c = |        DATA ls_tadir LIKE LINE OF g_tadir_components_mapping.                                                            |. add_abap_731 c.
    c = |        READ TABLE g_tadir_components_mapping                                                                             |. add_abap_731 c.
    c = |              INTO ls_tadir                                                                                               |. add_abap_731 c.
    c = |              WITH KEY component  = <component_infos>-component.                                                          |. add_abap_731 c.
    c = |        ASSERT SY-SUBRC EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing|. add_abap_731 c.
    c = |        object = ls_tadir-object.                                                                                         |. add_abap_731 c.
    add_replace.

    c = |        SELECT SINGLE devclass FROM tadir INTO @<component_infos>-package|. add_abap_740 c.
    c = |          WHERE pgmid = 'R3TR'                                           |. add_abap_740 c.
    c = |            AND object = @object                                         |. add_abap_740 c.
    c = |            AND obj_name = @<component_infos>-component_name.            |. add_abap_740 c.

    c = |        SELECT SINGLE devclass FROM tadir                                |. add_abap_731 c.
    c = |          INTO <component_infos>-package                                 |. add_abap_731 c.
    c = |         WHERE pgmid = 'R3TR'                                            |. add_abap_731 c.
    c = |           AND object = object                                           |. add_abap_731 c.
    c = |           AND obj_name = <component_infos>-component_name.              |. add_abap_731 c.
    add_replace.

    c = |      components_infos = VALUE #( ).|. add_abap_740 c.

    c = |      CLEAR components_infos.       |. add_abap_731 c.
    add_replace.

    c = |      LOOP AT new_components_infos ASSIGNING FIELD-SYMBOL(<component_infos_2>).|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <component_infos_2> LIKE LINE OF new_components_infos.     |. add_abap_731 c.
    c = |      LOOP AT new_components_infos ASSIGNING <component_infos_2>.              |. add_abap_731 c.
    add_replace.

    c = |          components_infos = VALUE #( BASE components_infos ( <component_infos_2> ) ).|. add_abap_740 c.

    c = |          INSERT <component_infos_2> INTO TABLE components_infos.                     |. add_abap_731 c.
    add_replace.

    " METHOD _determine_usages.

    c = |        SELECT * FROM wbcrossgt INTO TABLE @DATA(where_used_components) WHERE otype = 'ME' AND name = @where_used_name.|. add_abap_740 c.

    c = |        DATA where_used_components TYPE STANDARD TABLE OF wbcrossgt.                                                   |. add_abap_731 c.
    c = |        SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'ME' AND name = where_used_name.        |. add_abap_731 c.
    add_replace.

    c = |        SELECT * FROM wbcrossgt INTO TABLE @where_used_components WHERE otype = 'DA' AND name = @where_used_name.|. add_abap_740 c.

    c = |        SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'DA' AND name = where_used_name.  |. add_abap_731 c.
    add_replace.

    c = |    LOOP AT where_used_components ASSIGNING FIELD-SYMBOL(<where_used_component>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <where_used_component> LIKE LINE OF where_used_components.     |. add_abap_731 c.
    c = |    LOOP AT where_used_components ASSIGNING <where_used_component>.              |. add_abap_731 c.
    add_replace.

    " Here a different coding is required for ABAP 7.31
    " TBD Check the validity

    c = |      SELECT SINGLE * FROM ris_prog_tadir INTO @DATA(ris_prog_tadir_line) WHERE program_name = @<where_used_component>-include.|. add_abap_740 c.

    " TBD This is wrong, this function does not return a sy-subrc with this declaration
    c = |      DATA ls_mtdkey TYPE seocpdkey.                                                                                           |. add_abap_731 c.
    c = |      CALL FUNCTION 'SEO_METHOD_GET_NAME_BY_INCLUDE'                                                                           |. add_abap_731 c.
    c = |        EXPORTING                                                                                                              |. add_abap_731 c.
    c = |          progname = <where_used_component>-include                                                                            |. add_abap_731 c.
    c = |        IMPORTING                                                                                                              |. add_abap_731 c.
    c = |          mtdkey   = ls_mtdkey.                                                                                                |. add_abap_731 c.
    add_replace.

    " TBD This is wrong, no check on whether a class is used

    c = |            CASE ris_prog_tadir_line-object_type.|. add_abap_740 c.
    c = |          WHEN 'CLAS'.                           |. add_abap_740 c.

    c = |                                                 |. add_abap_731 c.
    add_replace.

    c = |            IF ris_prog_tadir_line-method_name IS INITIAL.   |. add_abap_740 c.
    c = |              using_method = 'DUMMY'.                        |. add_abap_740 c.
    c = |            ELSE.                                            |. add_abap_740 c.
    c = |              using_method = ris_prog_tadir_line-method_name.|. add_abap_740 c.
    c = |            ENDIF.                                           |. add_abap_740 c.

    c = |        IF ls_mtdkey-cpdname IS INITIAL.                     |. add_abap_731 c.
    c = |          using_method = 'DUMMY'.                            |. add_abap_731 c.
    c = |        ELSE.                                                |. add_abap_731 c.
    c = |          using_method = ls_mtdkey-cpdname.                  |. add_abap_731 c.
    c = |        ENDIF.                                               |. add_abap_731 c.
    add_replace.

    c = |            DATA(using_method_id) = sap_method->get_id( class  = ris_prog_tadir_line-object_name|. add_abap_740 c.
    c = |                                                        method = using_method ).                |. add_abap_740 c.

    c = |        DATA using_method_id TYPE i.                                                            |. add_abap_731 c.
    c = |        using_method_id = sap_method->get_id( class  = ls_mtdkey-clsname                        |. add_abap_731 c.
    c = |                                              method = using_method ).                          |. add_abap_731 c.
    add_replace.

    c = |                sap_class->add( EXPORTING name = ris_prog_tadir_line-object_name                  |. add_abap_740 c.
    c = |                                IMPORTING exists_already_with_id = DATA(exists_already_with_id) ).|. add_abap_740 c.

    c = |            DATA exists_already_with_id TYPE i.                                                   |. add_abap_731 c.
    c = |            sap_class->add( EXPORTING name = ls_mtdkey-cpdname                                    |. add_abap_731 c.
    c = |                            IMPORTING exists_already_with_id = exists_already_with_id ).          |. add_abap_731 c.
    add_replace.

    c = |                  new_components_infos = VALUE #( BASE new_components_infos (  component_name = ris_prog_tadir_line-object_name                          |. add_abap_740 c.
    c = |                                                                               component   = g_tadir_components_mapping[ object = 'CLAS' ]-component ) ).|. add_abap_740 c.

    c = |              DATA ls_new_components_info LIKE LINE OF new_components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion            |. add_abap_731 c.
    c = |                                                                                                                                                         |. add_abap_731 c.
    c = |              DATA ls_tadir_comp_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion           |. add_abap_731 c.
    c = |              READ TABLE g_tadir_components_mapping INTO ls_tadir_comp_map WITH TABLE KEY object = 'CLAS'.                                               |. add_abap_731 c.
    c = |              ASSERT SY-SUBRC EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing                         |. add_abap_731 c.
    c = |              CLEAR ls_new_components_info.                                                                                                              |. add_abap_731 c.
    c = |              ls_new_components_info-component_name = ls_mtdkey-cpdname.                                                                                 |. add_abap_731 c.
    c = |              ls_new_components_info-component   = ls_tadir_comp_map-component .                                                                         |. add_abap_731 c.
    c = |              INSERT ls_new_components_info INTO TABLE new_components_infos.                                                                             |. add_abap_731 c.
    add_replace.

    c = |              IF g_param_usage_outpack_groupd EQ false.                                       |. add_abap_740 c.
    c = |                using_method_id = sap_method->get_id( class  = ris_prog_tadir_line-object_name|. add_abap_740 c.
    c = |                                                        method = using_method ).              |. add_abap_740 c.

    c = |          IF g_param_usage_outpack_groupd EQ false.                                           |. add_abap_731 c.
    c = |            using_method_id = sap_method->get_id( class  = ls_mtdkey-cpdname                  |. add_abap_731 c.
    c = |                                                  method = using_method ).                    |. add_abap_731 c.
    add_replace.

    c = |                  using_method_id = sap_method->add( EXPORTING class  = ris_prog_tadir_line-object_name|. add_abap_740 c.
    c = |                                                               method = using_method ).                |. add_abap_740 c.

    c = |              using_method_id = sap_method->add( class  = ls_mtdkey-cpdname                            |. add_abap_731 c.
    c = |                                                 method = using_method ).                              |. add_abap_731 c.
    add_replace.

    c = |                  using_method_id = sap_method->add( EXPORTING class  = 'OTHER_SAP_CLASS'       |. add_abap_740 c.
    c = |                                                                 method = 'OTHER_SAP_METHOD'  ).|. add_abap_740 c.

    c = |                  using_method_id = sap_method->add( class  = 'OTHER_SAP_CLASS'                 |. add_abap_731 c.
    c = |                                                     method = 'OTHER_SAP_METHOD'  ).            |. add_abap_731 c.
    add_replace.

    c = |          WHEN OTHERS.                  |. add_abap_740 c.
    c = |            " TBD Implement other usages|. add_abap_740 c.
    c = |        ENDCASE.                        |. add_abap_740 c.

    " TBD See above this is wrong

    c = |        " TBD Implement other usages    |. add_abap_731 c.
    add_replace.

    " METHOD _set_default_language.

    c = |    DATA(famix_custom_source_language) = NEW cl_famix_custom_source_lang( model ).|. add_abap_740 c.

    c = |    DATA famix_custom_source_language TYPE REF TO cl_famix_custom_source_lang.    |. add_abap_731 c.
    c = |    CREATE OBJECT famix_custom_source_language EXPORTING model = model.           |. add_abap_731 c.
    add_replace.

    " METHOD _determine_packages_to_analyze.

    c = |    INSERT VALUE package_type( devclass = package_first-devclass ) INTO TABLE processed_packages.                              |. add_abap_740 c.

    c = |    DATA ls_processed_package LIKE LINE OF processed_packages. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_processed_package.                                                                                                |. add_abap_731 c.
    c = |    ls_processed_package-devclass = package_first-devclass.                                                                    |. add_abap_731 c.
    c = |    INSERT ls_processed_package INTO TABLE processed_packages.                                                                 |. add_abap_731 c.
    add_replace.

    c = |    temp_packages_to_search = VALUE #( ( devclass = g_parameter_package_to_analyze ) ).                                                  |. add_abap_740 c.

    c = |    DATA ls_temp_package_to_search LIKE LINE OF temp_packages_to_search. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |    CLEAR ls_temp_package_to_search.                                                                                                     |. add_abap_731 c.
    c = |    ls_temp_package_to_search-devclass = g_parameter_package_to_analyze.                                                                 |. add_abap_731 c.
    c = |    INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.                                                                 |. add_abap_731 c.
    add_replace.

    c = |        SELECT devclass, parentcl FROM tdevc INTO TABLE @DATA(packages)                                    |. add_abap_740 c.
    c = |         FOR ALL ENTRIES IN @temp_packages_to_search WHERE parentcl = @temp_packages_to_search-devclass.   |. add_abap_740 c.

    c = |        types: BEGIN OF abap_731_package_type,                                                             |. add_abap_731 c.
    c = |          devclass TYPE tdevc-devclass,                                                                    |. add_abap_731 c.
    c = |          parentcl type tdevc-parentcl,                                                                    |. add_abap_731 c.
    c = |               END OF abap_731_package_type.                                                               |. add_abap_731 c.
    c = |        data: packages type standard table of abap_731_package_type WITH DEFAULT KEY.                      |. add_abap_731 c.
    c = |        SELECT devclass  parentcl FROM tdevc INTO TABLE packages                                           |. add_abap_731 c.
    c = |         FOR ALL ENTRIES IN temp_packages_to_search                                                        |. add_abap_731 c.
    c = |          WHERE parentcl = temp_packages_to_search-devclass.                                               |. add_abap_731 c.
    add_replace.

    c = |      temp_packages_to_search = VALUE #( ).|. add_abap_740 c.

    c = |      CLEAR temp_packages_to_search.       |. add_abap_731 c.
    add_replace.

    c = |      LOOP AT packages INTO DATA(package).|. add_abap_740 c.

    c = |      DATA package LIKE LINE OF packages. |. add_abap_731 c.
    c = |      LOOP AT packages INTO package.      |. add_abap_731 c.
    add_replace.

    c = |        INSERT VALUE package_type( devclass = package-devclass ) INTO TABLE processed_packages.|. add_abap_740 c.

    c = |        CLEAR ls_processed_package.                                                            |. add_abap_731 c.
    c = |        ls_processed_package-devclass = package-devclass.                                      |. add_abap_731 c.
    c = |        INSERT ls_processed_package INTO TABLE processed_packages.                             |. add_abap_731 c.
    add_replace.

    c = |          temp_packages_to_search = VALUE #( BASE temp_packages_to_search ( devclass = package-devclass ) ).|. add_abap_740 c.

    c = |          CLEAR ls_temp_package_to_search.                                                                  |. add_abap_731 c.
    c = |          ls_temp_package_to_search-devclass = package-devclass.                                            |. add_abap_731 c.
    c = |          INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.                              |. add_abap_731 c.
    add_replace.


    c = |    MOVE-CORRESPONDING components_infos TO classes.                                                            |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |    LOOP AT components_infos ASSIGNING FIELD-SYMBOL(<component_infos>).                                        |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |      IF <component_infos>-component EQ 'GlobClass'                                                            |. add_abap_740 c.
    c = |      OR <component_infos>-component EQ 'GlobIntf'.                                                            |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |        classes = VALUE #( BASE classes ( obj_name = <component_infos>-component_name ) ).                     |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |      ELSE.                                                                                                    |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |        programs = VALUE #( BASE programs ( program = <component_infos>-component_name ) ).                    |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |      ENDIF.                                                                                                   |. add_abap_740 c.
    c = |                                                                                                               |. add_abap_740 c.
    c = |    ENDLOOP.                                                                                                   |. add_abap_740 c.

    c = |    DATA class LIKE LINE OF classes.                                                                           |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |    FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.                                             |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |    LOOP AT components_infos ASSIGNING <component_infos>.                                                      |. add_abap_731 c.
    c = |      MOVE-CORRESPONDING <component_infos> TO class.                                                           |. add_abap_731 c.
    c = |      INSERT class INTO TABLE classes.                                                                         |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |      IF <component_infos>-component EQ 'GlobClass'                                                            |. add_abap_731 c.
    c = |      OR <component_infos>-component EQ 'GlobIntf'.                                                            |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |        class-obj_name = <component_infos>-component_name.                                                     |. add_abap_731 c.
    c = |        INSERT class INTO TABLE classes.                                                                       |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |      ELSE.                                                                                                    |. add_abap_731 c.
    c = |        DATA ls_program LIKE LINE OF programs. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion|. add_abap_731 c.
    c = |        CLEAR ls_program.                                                                                      |. add_abap_731 c.
    c = |        ls_program-program = <component_infos>-component_name.                                                 |. add_abap_731 c.
    c = |        INSERT ls_program INTO TABLE programs.                                                                 |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |      ENDIF.                                                                                                   |. add_abap_731 c.
    c = |                                                                                                               |. add_abap_731 c.
    c = |    ENDLOOP.                                                                                                   |. add_abap_731 c.
    add_replace.

    c = |    LOOP AT programs ASSIGNING FIELD-SYMBOL(<program>).|. add_abap_740 c.

    c = |    FIELD-SYMBOLS <program> LIKE LINE OF programs.     |. add_abap_731 c.
    c = |    LOOP AT programs ASSIGNING <program>.              |. add_abap_731 c.
    add_replace.

    c = |      DATA(module_reference) = sap_program->add( EXPORTING name = <program>-program ).|. add_abap_740 c.

    c = |      DATA module_reference TYPE i.                                                   |. add_abap_731 c.
    c = |      module_reference = sap_program->add( name = <program>-program ).                |. add_abap_731 c.
    add_replace.

    c = |      READ TABLE components_infos ASSIGNING FIELD-SYMBOL(<component_infos>) WITH TABLE KEY component = 'ABAPProgramm' component_name = <program>-program.|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.                                                                                     |. add_abap_731 c.
    c = |      READ TABLE components_infos ASSIGNING <component_infos>                                                                                            |. add_abap_731 c.
    c = |            WITH TABLE KEY component = 'ABAPProgram'                                                                                                     |. add_abap_731 c.
    c = |                           component_name = <program>-program.                                                                                           |. add_abap_731 c.
    add_replace.

    c = |        DATA(program_analyzer) = NEW cl_program_analyzer( ).  |. add_abap_740 c.

    c = |        DATA program_analyzer TYPE REF TO cl_program_analyzer.|. add_abap_731 c.
    c = |        CREATE OBJECT program_analyzer.                       |. add_abap_731 c.
    add_replace.

    " METHOD _add_classes_to_model.

    c = |    LOOP AT existing_classes INTO DATA(existing_class).|. add_abap_740 c.

    c = |    DATA existing_class LIKE LINE OF existing_classes. |. add_abap_731 c.
    c = |    LOOP AT existing_classes INTO existing_class.      |. add_abap_731 c.
    add_replace.

    c = |      READ TABLE components_infos ASSIGNING FIELD-SYMBOL(<component_infos>) WITH TABLE KEY component = 'GlobClass' component_name = existing_class-class.|. add_abap_740 c.

    c = |      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.                                                                                     |. add_abap_731 c.
    c = |      READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = 'GlobClass' component_name = existing_class-class.              |. add_abap_731 c.
    add_replace.

    " METHOD _determine_inheritances_betwee.

    c = |      SELECT clsname, refclsname, reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE @inheritances|. add_abap_740 c.
    c = |        FOR ALL ENTRIES IN @existing_classes WHERE clsname = @existing_classes-class                      |. add_abap_740 c.
    c = |                                               AND version = 1.                                           |. add_abap_740 c.

    c = |      SELECT clsname refclsname reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE inheritances   |. add_abap_731 c.
    c = |        FOR ALL ENTRIES IN existing_classes WHERE clsname = existing_classes-class                        |. add_abap_731 c.
    c = |                                               AND version = 1.                                           |. add_abap_731 c.
    add_replace.

    c = |    LOOP AT inheritances INTO DATA(inheritance).|. add_abap_740 c.

    c = |    DATA inheritance LIKE LINE OF inheritances. |. add_abap_731 c.
    c = |    LOOP AT inheritances INTO inheritance.      |. add_abap_731 c.
    add_replace.

    c = |    LOOP AT inheritances INTO DATA(inheritance_2).|. add_abap_740 c.

    c = |    DATA inheritance_2 LIKE LINE OF inheritances. |. add_abap_731 c.
    c = |    LOOP AT inheritances INTO inheritance_2.      |. add_abap_731 c.
    add_replace.

    " METHOD _determine_class_components.

    c = |      SELECT clsname, cmpname, cmptype FROM seocompo INTO TABLE @class_components|. add_abap_740 c.
    c = |        FOR ALL ENTRIES IN @existing_classes                                     |. add_abap_740 c.
    c = |        WHERE                                                                    |. add_abap_740 c.
    c = |          clsname = @existing_classes-class.                                     |. add_abap_740 c.

    c = |      SELECT clsname cmpname cmptype FROM seocompo INTO TABLE class_components   |. add_abap_731 c.
    c = |        FOR ALL ENTRIES IN existing_classes                                      |. add_abap_731 c.
    c = |        WHERE                                                                    |. add_abap_731 c.
    c = |          clsname = existing_classes-class.                                      |. add_abap_731 c.
    add_replace.

    " METHOD _add_to_class_components_to_mo.

    c = |    LOOP AT class_components INTO DATA(class_component).|. add_abap_740 c.

    c = |    DATA class_component LIKE LINE OF class_components. |. add_abap_731 c.
    c = |    LOOP AT class_components INTO class_component.      |. add_abap_731 c.
    add_replace.

    c = |          DATA(existing_id) = sap_attribute->get_id( EXPORTING class     = class_component-clsname   |. add_abap_740 c.
    c = |                                                               attribute = class_component-cmpname ).|. add_abap_740 c.

    c = |          DATA existing_id TYPE i.                                                                   |. add_abap_731 c.
    c = |          existing_id =  sap_attribute->get_id( class     = class_component-clsname                  |. add_abap_731 c.
    c = |                                                attribute = class_component-cmpname ).               |. add_abap_731 c.
    add_replace.

    " METHOD _determine_usage_of_methods.

    c = |          DATA(used_id) = sap_method->get_id( class  = class_component-clsname     |. add_abap_740 c.
    c = |                                                method = class_component-cmpname ).|. add_abap_740 c.

    c = |          DATA used_id TYPE i.|. add_abap_731 c.
    c = |          used_id = sap_method->get_id( class  = class_component-clsname           |. add_abap_731 c.
    c = |                                        method = class_component-cmpname ).        |. add_abap_731 c.
    add_replace.

    " METHOD _read_all_classes.

    c = |      SELECT clsname AS class FROM seoclass INTO TABLE @existing_classes FOR ALL ENTRIES IN @classes|. add_abap_740 c.
    c = |        WHERE                                                                                       |. add_abap_740 c.
    c = |          clsname = @classes-obj_name.                                                              |. add_abap_740 c.

    c = |      SELECT clsname AS class FROM seoclass INTO TABLE existing_classes FOR ALL ENTRIES IN classes  |. add_abap_731 c.
    c = |        WHERE                                                                                       |. add_abap_731 c.
    c = |          clsname = classes-obj_name.                                                               |. add_abap_731 c.
    add_replace.

    " METHOD _select_requested_components.

    c = |      SELECT SINGLE devclass, parentcl FROM tdevc INTO @first_package WHERE devclass = @package_to_analyze.|. add_abap_740 c.

    c = |      SELECT SINGLE devclass parentcl FROM tdevc INTO first_package WHERE devclass = package_to_analyze.   |. add_abap_731 c.
    add_replace.

    c = |          SELECT obj_name, object, devclass FROM tadir INTO @DATA(tadir_component) FOR ALL ENTRIES IN @processed_packages|. add_abap_740 c.
    c = |            WHERE pgmid = 'R3TR'                                                                                         |. add_abap_740 c.
    c = |              AND object = @object                                                                                       |. add_abap_740 c.
    c = |              AND devclass = @processed_packages-devclass.                                                               |. add_abap_740 c.

    c = |          DATA: BEGIN OF tadir_component,                                                                                |. add_abap_731 c.
    c = |                   obj_name LIKE tadir-obj_name,                                                                         |. add_abap_731 c.
    c = |                   object   LIKE tadir-object,                                                                           |. add_abap_731 c.
    c = |                   devclass LIKE tadir-devclass,                                                                         |. add_abap_731 c.
    c = |                END OF tadir_component.                                                                                  |. add_abap_731 c.
    c = |          SELECT obj_name object devclass FROM tadir INTO tadir_component FOR ALL ENTRIES IN processed_packages          |. add_abap_731 c.
    c = |            WHERE pgmid = 'R3TR'                                                                                         |. add_abap_731 c.
    c = |              AND object = object                                                                                        |. add_abap_731 c.
    c = |              AND devclass = processed_packages-devclass.                                                                |. add_abap_731 c.
    add_replace.

    only_once. " Because otherwise the data declarations would be doubled
    c = |            components_infos = VALUE #( BASE components_infos ( component = g_tadir_components_mapping[ object = tadir_component-object ]-component|. add_abap_740 c.
    c = |                                                                component_name = tadir_component-obj_name                                          |. add_abap_740 c.
    c = |                                                                package = tadir_component-devclass ) ).                                            |. add_abap_740 c.

    c = |            DATA ls_component_info LIKE LINE OF components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion                 |. add_abap_731 c.
    c = |            DATA ls_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion                  |. add_abap_731 c.
    c = |                                                                                                                                                   |. add_abap_731 c.
    c = |            READ TABLE g_tadir_components_mapping INTO ls_map WITH TABLE KEY object = tadir_component-object.                                      |. add_abap_731 c.
    c = |            ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing                     |. add_abap_731 c.
    c = |                                                                                                                                                   |. add_abap_731 c.
    c = |            CLEAR ls_component_info.                                                                                                               |. add_abap_731 c.
    c = |            ls_component_info-component = ls_map-component.                                                                                        |. add_abap_731 c.
    c = |            ls_component_info-component_name = tadir_component-obj_name.                                                                           |. add_abap_731 c.
    c = |            ls_component_info-package = tadir_component-devclass.                                                                                  |. add_abap_731 c.
    c = |            INSERT ls_component_info INTO TABLE components_infos.                                                                                  |. add_abap_731 c.
    add_replace.

    c = |          SELECT obj_name, object, devclass FROM tadir INTO @tadir_component|. add_abap_740 c.
    c = |            WHERE pgmid = 'R3TR'                                            |. add_abap_740 c.
    c = |              AND object = @object                                          |. add_abap_740 c.
    c = |              AND obj_name IN @s_compsn                                     |. add_abap_740 c.
    c = |              AND devclass IN @s_pack.                                      |. add_abap_740 c.

    c = |          SELECT obj_name object devclass FROM tadir INTO tadir_component   |. add_abap_731 c.
    c = |            WHERE pgmid = 'R3TR'                                            |. add_abap_731 c.
    c = |              AND object = object                                           |. add_abap_731 c.
    c = |              AND obj_name IN s_compsn                                      |. add_abap_731 c.
    c = |              AND devclass IN s_pack.                                       |. add_abap_731 c.
    add_replace.

    c = |            components_infos = VALUE #( BASE components_infos ( component = g_tadir_components_mapping[ object = tadir_component-object ]-component|. add_abap_740 c.
    c = |                                                                component_name = tadir_component-obj_name                                          |. add_abap_740 c.
    c = |                                                                package = tadir_component-devclass ) ).                                            |. add_abap_740 c.

    c = |            DATA ls_map_2 LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion                |. add_abap_731 c.
    c = |            READ TABLE g_tadir_components_mapping INTO ls_map_2 WITH TABLE KEY object = tadir_component-object.                                    |. add_abap_731 c.
    c = |            ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing                     |. add_abap_731 c.
    c = |                                                                                                                                                   |. add_abap_731 c.
    c = |            CLEAR ls_component_info.                                                                                                               |. add_abap_731 c.
    c = |            ls_component_info-component = ls_map_2-component.                                                                                      |. add_abap_731 c.
    c = |            ls_component_info-component_name = tadir_component-obj_name.                                                                           |. add_abap_731 c.
    c = |            ls_component_info-package = tadir_component-devclass.                                                                                  |. add_abap_731 c.
    c = |            INSERT ls_component_info INTO TABLE components_infos.                                                                                  |. add_abap_731 c.
    add_replace.

    " START-OF-SELECTION.

    c = |    DATA(sap_extractor) = NEW cl_extract_sap( ).  |. add_abap_740 c.

    c = |    DATA sap_extractor TYPE REF TO cl_extract_sap.|. add_abap_731 c.
    c = |    CREATE OBJECT sap_extractor.                  |. add_abap_731 c.
    add_replace.

    c = |    DATA(nothing_done) = sap_extractor->extract( IMPORTING mse_model = mse_model ).  |. add_abap_740 c.

    c = |    DATA nothing_done TYPE boolean.                                                  |. add_abap_731 c.
    c = |    sap_extractor->extract( IMPORTING mse_model    = mse_model                       |. add_abap_731 c.
    c = |                                      nothing_done = nothing_done ).                 |. add_abap_731 c.
    add_replace.

    c = |  DATA(model_outputer) = NEW cl_output_model( ).  |. add_abap_740 c.

    c = |  DATA model_outputer TYPE REF TO cl_output_model.|. add_abap_731 c.
    c = |  CREATE OBJECT model_outputer.                   |. add_abap_731 c.
    add_replace.






  ENDMETHOD.

  METHOD get_conversion.

    replaces = g_replaces.

  ENDMETHOD.

ENDCLASS.

CLASS cl_convert DEFINITION.
  PUBLIC SECTION.
    DATA: g_conversion TYPE REF TO cl_conversion.
    METHODS constructor.
    METHODS do
      CHANGING source TYPE stringtable.
  PRIVATE SECTION.

    METHODS _read_first_line_to_compare
      IMPORTING
        replace           TYPE cl_conversion=>replace_type
      EXPORTING
        code_index        TYPE i
        abap_740_codeline TYPE cl_conversion=>codeline_type.
ENDCLASS.

CLASS cl_convert IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT g_conversion.

  ENDMETHOD.

  METHOD do.

    DATA: replaces    TYPE g_conversion->replaces_type,
          replace     TYPE g_conversion->replace_type,
          codeline    TYPE g_conversion->codeline_type,
          "! All code lines to be replaced
          codelines   TYPE g_conversion->codelines_type,
          "! The code lines after processing a scan for a single replacement
          codelines_2 TYPE g_conversion->codelines_type,
          "! A temporary list for code lines where at least part of the lines fit to the actual searched replace
          codelines_3 TYPE g_conversion->codelines_type,
          line        TYPE string,
          code_index  TYPE i.

    replaces = g_conversion->get_conversion( ).

    LOOP AT source INTO line.

      codeline-code = line.
      codeline-condensed = line.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed TO UPPER CASE.
      APPEND codeline TO codelines.

    ENDLOOP.

    " The replace is now done the following way.

    " general, compared are always the condensed lines, replaced is the not condensed line.

    " For each entry in table replaces:

    " Loop over code lines and transfer line by line into codelines_2
    "   but only if a line is not equal to the first line in replace-abap_740

    "   if it is equal it will be transfered to codelines_3
    "   in that case all lines of replace-abap_740 have to be found
    "   if not all lines are equal to replace-abap_740, codelines_3 is appended to codelines_2 and scan is proceeded normally
    "   if all lines are equal, replace-abap_730 is appended to codelines_2 and codelines_3 is cleared than scan is proceeded normally


    LOOP AT replaces ASSIGNING FIELD-SYMBOL(<replace>).

      DATA: abap_740_codeline TYPE g_conversion->codeline_type,
            line_is_equal     TYPE bool,
            "! True if comparison for a replacement started. This is the case if the first line was found in the code.
            first_line_equal  TYPE bool.

      _read_first_line_to_compare( EXPORTING replace           = <replace>
                                   IMPORTING code_index        = code_index
                                             abap_740_codeline = abap_740_codeline ).

      codelines_2 = VALUE #( ).

      LOOP AT codelines INTO codeline.

        IF first_line_equal EQ true.
          ADD 1 TO code_index.
          READ TABLE <replace>-abap_740 INTO abap_740_codeline INDEX code_index.
          IF sy-subrc EQ 0. " Row is found

          ELSEIF sy-subrc EQ 4. " Row is not found
            codelines_2 = VALUE #( BASE codelines_2 ( LINES OF <replace>-abap_731 ) ).
            ADD 1 TO <replace>-replaced.
            codelines_3 = VALUE #( ).
            first_line_equal = false.

            _read_first_line_to_compare( EXPORTING replace           = <replace>
                                         IMPORTING code_index        = code_index
                                                   abap_740_codeline = abap_740_codeline ).


          ELSE. " Occurs if comparing statement or binary search is used, not supported here
            ASSERT 1 = 2.
          ENDIF.
        ENDIF.

        IF codeline-condensed EQ abap_740_codeline-condensed
           AND NOT ( <replace>-replaced >= 1 AND <replace>-only_once EQ true ).

          IF code_index EQ 1.
            first_line_equal = true.
            codelines_3 = VALUE #( ( codeline ) ).
          ELSE.
            codelines_3 = VALUE #( BASE codelines_3 ( codeline ) ).
          ENDIF.

          line_is_equal = true.
        ELSE.

          IF first_line_equal EQ false.

            codelines_2 = VALUE #( BASE codelines_2 ( codeline ) ).

          ELSE.

            codelines_2 = VALUE #( BASE codelines_2 ( LINES OF codelines_3 ) ). " Add the remembered lines
            codelines_2 = VALUE #( BASE codelines_2 ( codeline ) ). " Add the actual line
            codelines_3 = VALUE #( ).
            first_line_equal = false.

            _read_first_line_to_compare( EXPORTING replace           = <replace>
                                         IMPORTING code_index        = code_index
                                                   abap_740_codeline = abap_740_codeline ).

          ENDIF.

          line_is_equal = false.
        ENDIF.

        " TBD finalize logic

      ENDLOOP.

      codelines_2 = VALUE #( BASE codelines_2 ( LINES OF codelines_3 ) ).

      codelines = codelines_2.

    ENDLOOP.

    source = VALUE #( ).

    LOOP AT codelines INTO codeline.

      APPEND codeline-code TO source.

    ENDLOOP.

    " List replaces

    LOOP AT replaces INTO replace.
      WRITE: / replace-replace_id.
      IF replace-replaced EQ 0.
        FORMAT COLOR COL_TOTAL.
      ELSEIF replace-replaced > 1.
        FORMAT COLOR COL_HEADING.
      ELSE.
        FORMAT COLOR COL_BACKGROUND.
      ENDIF.
      WRITE: replace-replaced, replace-abap_740[ 1 ]-code.
      FORMAT COLOR COL_BACKGROUND.
    ENDLOOP.

  ENDMETHOD.


  METHOD _read_first_line_to_compare.

    code_index = 1.
    READ TABLE replace-abap_740 INTO abap_740_codeline INDEX code_index.
    ASSERT sy-subrc EQ 0. " Row has to be found

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  DATA(read) = NEW cl_read( ).
  DATA(convert) = NEW cl_convert( ).
  DATA(download) = NEW cl_download( ).

  DATA(source) = read->do( ).
  convert->do( CHANGING source = source ).
  download->do( source = source ).