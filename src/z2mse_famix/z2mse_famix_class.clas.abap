

CLASS z2mse_famix_class DEFINITION INHERITING FROM Z2MSE_famix_container_entity
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
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

CLASS z2mse_famix_class IMPLEMENTATION.

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
