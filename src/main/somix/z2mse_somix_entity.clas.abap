class Z2MSE_SOMIX_ENTITY definition
  public
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !MODEL type ref to Z2MSE_MODEL .
protected section.

  data G_MODEL type ref to Z2MSE_MODEL .
  data G_ELEMENTNAME type STRING .
  data G_LAST_USED_ID type I .
private section.
ENDCLASS.



CLASS Z2MSE_SOMIX_ENTITY IMPLEMENTATION.


  METHOD constructor.
    g_model = model.
  ENDMETHOD.
ENDCLASS.
