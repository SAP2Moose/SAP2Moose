"! I describe an association between elements.
"! I have sub classes that specify concrete types of associations.
CLASS z2mse_extr3_association DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3.

  PUBLIC SECTION.

    DATA type TYPE c LENGTH 30 READ-ONLY.

    CONSTANTS: parent_package_ass LIKE type VALUE 'parent_package',
               class_comp_ass like type VALUE 'class_components'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3_association IMPLEMENTATION.
ENDCLASS.
