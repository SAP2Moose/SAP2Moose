

CLASS z2mse_famix_access DEFINITION INHERITING FROM Z2MSE_famix_association
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
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

CLASS z2mse_famix_access IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Access'.
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

  METHOD is_new_access.
    READ TABLE g_accessor_variable_ids TRANSPORTING NO FIELDS WITH TABLE KEY accessor_id = accessor_id variable_id = variable_id.
    IF sy-subrc <> 0. "OK
      is_new = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
