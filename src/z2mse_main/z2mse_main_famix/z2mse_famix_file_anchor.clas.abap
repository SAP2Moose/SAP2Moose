class Z2MSE_FAMIX_FILE_ANCHOR definition
  public
  inheriting from Z2MSE_FAMIX_ENTITY
  create public .

public section.

    "! Call once to create a new file anchor entiry
    "! @parameter element_id | The ID of the element for which a source is specified
    "! @parameter file_name | The path or link to the source
  methods ADD
    importing
      !ELEMENT_ID type I
      !FILE_NAME type CLIKE
    exporting
      value(ID) type I .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2MSE_FAMIX_FILE_ANCHOR IMPLEMENTATION.


  METHOD add.
    g_model->add_entity( EXPORTING elementname = |FAMIX.FileAnchor|
                                   name_group = |FILE_ANCHOR|
                                   is_named_entity = abap_false
                                   is_id_required            = abap_true
                                   can_be_referenced_by_name = abap_false
                         IMPORTING processed_id = id ).

    g_model->add_reference_by_id( EXPORTING element_id         = id
                                            attribute_name     = 'element'
                                            reference_id       = element_id ).

    g_model->add_string( EXPORTING element_id     = id
                                   attribute_name = 'fileName'
                                   string         = file_name ).

    g_last_used_id = id.
  ENDMETHOD.
ENDCLASS.
