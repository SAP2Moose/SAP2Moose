
CLASS z2mse_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM z2mse_famix_entity
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! Declare source language
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter source_language_element | the FAMIX element of the source language
    "! @parameter source_language_name | the name of the source language
    METHODS set_declared_source_language
      IMPORTING
        element_id              TYPE i
        element_type            TYPE clike OPTIONAL
        element_name_group      TYPE clike OPTIONAL
        element_name            TYPE clike OPTIONAL
        source_language_element TYPE clike
        source_language_name    TYPE clike.
ENDCLASS.



CLASS Z2MSE_FAMIX_SOURCED_ENTITY IMPLEMENTATION.


  METHOD set_declared_source_language.
    g_model->add_reference_by_name( EXPORTING element_id = element_id
                                              element_type = element_type
                                              element_name_group = element_name_group
                                              element_name = element_name
                                              attribute_name    = 'declaredSourceLanguage'
                                              type_of_reference       = source_language_element
                                              name_of_reference = source_language_name ).
  ENDMETHOD.
ENDCLASS.
