

CLASS z2mse_famix_named_entity DEFINITION INHERITING FROM z2mse_famix_sourced_entity ABSTRACT
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.

    "! Call once to create a new named entity
    "! @parameter exists_already_with_id | Contains the id if entry already existed.
    "! @parameter id | The id in model either if just created or already existing.
    "! @parameter modifiers | A list of modifiers separated by blank. This attribute is marked by an asterisk in the Moose Meta Browser, which may be the sign of this. Will be an Ordered Collection in Moose.
    METHODS add
      IMPORTING name_group                    TYPE clike OPTIONAL
                name                          TYPE clike
                modifiers                     TYPE clike OPTIONAL
      EXPORTING VALUE(exists_already_with_id) TYPE i
                VALUE(id)                     TYPE i.
    "! Call once to set the parent package
    "! Provide either ID or type and name of element
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter elemenent_type | the element type of the element (not needed if ID is provided)
    "! @parameter element_name_group | the name group of the element where the ID shall be added
    "! @parameter element_name | the name of the element
    "! @parameter parent_package | the name of an element of type FAMIX.Package
    METHODS set_parent_package IMPORTING element_id         TYPE i
                                         element_type       TYPE clike OPTIONAL
                                         element_name_group TYPE clike OPTIONAL
                                         element_name       TYPE clike OPTIONAL
                                         parent_package     TYPE clike.

  PROTECTED SECTION.

ENDCLASS.

CLASS z2mse_famix_named_entity IMPLEMENTATION.

  METHOD add.
    g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = abap_true
                                        can_be_referenced_by_name = abap_true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id
                                        processed_id = id ).
    IF modifiers IS SUPPLIED.
      g_model->add_string( EXPORTING element_id     = id
                                     attribute_name = 'modifiers'
                                     string         = modifiers ).
    ENDIF.
    g_last_used_id = id.
  ENDMETHOD.

  METHOD set_parent_package.
    g_model->add_reference_by_name( element_id = element_id
                                    element_type = element_type
                                    element_name_group = element_name_group
                                    element_name = element_name type_of_reference       = 'FAMIX.Package'
                                    name_of_reference = parent_package
                                    attribute_name    = 'parentPackage' ).
  ENDMETHOD.

ENDCLASS.
