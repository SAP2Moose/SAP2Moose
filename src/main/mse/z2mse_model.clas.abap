
" Begin Model
"! Specifies a model.
"! Create an instance only once, otherwise there will be multiple models each containing only part of the informations.
CLASS z2mse_model DEFINITION
  PUBLIC
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
        attribute            TYPE z2mse_model=>attribute_type
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



CLASS z2mse_model IMPLEMENTATION.


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
