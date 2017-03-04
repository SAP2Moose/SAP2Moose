

CLASS z2mse_famix_behavioural_entty DEFINITION INHERITING FROM Z2MSE_famix_container_entity ABSTRACT
  PUBLIC
  CREATE PUBLIC.
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

CLASS z2mse_famix_behavioural_entty IMPLEMENTATION.

  METHOD set_signature.
    g_model->add_string( EXPORTING element_id = element_id
                                   element_type = element_type
                                   element_name_group = element_name_group
                                   element_name = element_name
                                   attribute_name = 'signature'
                                   string         = signature ).
  ENDMETHOD.

ENDCLASS.
