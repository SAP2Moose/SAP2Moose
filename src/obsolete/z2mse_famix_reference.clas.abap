

CLASS z2mse_famix_reference DEFINITION INHERITING FROM Z2MSE_famix_association
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    "! defines an inheritance
    "! @parameter element_id | the ID of the element where the ID shall be added
    "! @parameter target_id | the FAMIX id of the target element
    "! @parameter source_id | the FAMIX id of the source element
    METHODS set_target_source
      IMPORTING
        element_id TYPE i
        target_id  TYPE i
        source_id  TYPE i.
ENDCLASS.

CLASS z2mse_famix_reference IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Reference'.
  ENDMETHOD.

  METHOD set_target_source.

    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name    = 'target'
                                            reference_id      = target_id ).
    g_model->add_reference_by_id( EXPORTING element_id = element_id
                                            attribute_name    = 'source'
                                            reference_id       = source_id ).
  ENDMETHOD.

ENDCLASS.
