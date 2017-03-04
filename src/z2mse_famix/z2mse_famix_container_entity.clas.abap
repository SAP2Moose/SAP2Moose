

CLASS z2mse_famix_container_entity DEFINITION INHERITING FROM z2mse_famix_named_entity ABSTRACT
  PUBLIC
  CREATE PUBLIC.
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

CLASS z2mse_famix_container_entity IMPLEMENTATION.

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
