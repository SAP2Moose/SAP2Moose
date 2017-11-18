

CLASS z2mse_famix_attribute DEFINITION INHERITING FROM z2mse_famix_named_entity
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
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

CLASS z2mse_famix_attribute IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Attribute'.
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
  METHOD add.
    g_model->add_entity(
               EXPORTING elementname = g_elementname
                         is_named_entity = abap_true
                         can_be_referenced_by_name = abap_false
                         name = name
               IMPORTING processed_id = id ).
    g_last_used_id = id.
  ENDMETHOD.


  METHOD store_id.
    DATA ls_attribute_id LIKE LINE OF g_attribute_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_attribute_id.
    ls_attribute_id-id = g_last_used_id.
    ls_attribute_id-class = class.
    ls_attribute_id-attribute = attribute.
    INSERT ls_attribute_id INTO TABLE g_attribute_ids.
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

ENDCLASS.
