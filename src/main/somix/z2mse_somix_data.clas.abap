class Z2MSE_SOMIX_DATA definition
  public
  inheriting from Z2MSE_SOMIX_COMPONENT
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !MODEL type ref to Z2MSE_MODEL .
protected section.
private section.

  data IS_PERSISTENT type ABAP_BOOL .
ENDCLASS.



CLASS Z2MSE_SOMIX_DATA IMPLEMENTATION.


  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Data'.
  ENDMETHOD.
ENDCLASS.
