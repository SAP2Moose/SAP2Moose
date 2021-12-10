class Z2MSE_SOMIX_ACCESS definition
  public
  inheriting from Z2MSE_SOMIX_COUPLING
  final
  create public .

public section.
protected section.
private section.

  data ACCESSOR type ref to Z2MSE_SOMIX_COMPONENT .
  data ACCESSED type ref to Z2MSE_SOMIX_DATA .
  data IS_WRITE type ABAP_BOOL .
  data IS_READ type ABAP_BOOL .
  data ID_DEPENDENT type ABAP_BOOL .
ENDCLASS.



CLASS Z2MSE_SOMIX_ACCESS IMPLEMENTATION.
ENDCLASS.
