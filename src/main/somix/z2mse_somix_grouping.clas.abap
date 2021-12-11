class Z2MSE_SOMIX_GROUPING definition
  public
  inheriting from Z2MSE_SOMIX_ELEMENT
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !MODEL type ref to Z2MSE_MODEL .
protected section.
private section.
ENDCLASS.



CLASS Z2MSE_SOMIX_GROUPING IMPLEMENTATION.


  method CONSTRUCTOR.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Grouping'.
  endmethod.
ENDCLASS.
