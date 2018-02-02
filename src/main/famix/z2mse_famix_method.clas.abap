

CLASS z2mse_famix_method DEFINITION INHERITING FROM Z2MSE_famix_behavioural_entty
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
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

CLASS z2mse_famix_method IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Method'.
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
    DATA ls_method_id LIKE LINE OF g_method_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_method_id.
    ls_method_id-id = g_last_used_id.
    ls_method_id-class_name_group = class_name_group.
    ls_method_id-class = class.
    ls_method_id-method_name_group = method_name_group.
    ls_method_id-method = method.
    INSERT ls_method_id INTO TABLE g_method_ids.
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

ENDCLASS.
