"! Used in Unit Tests to simplify checking for correct Moose models
"! Will not be delivered as local class, thus use new ABAP statements
CLASS z2mse_mse_harmonize DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES mse            TYPE TABLE OF string WITH DEFAULT KEY.
    TYPES harmonized_mse TYPE TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS:
      mse_2_harmonized
        IMPORTING mse                             TYPE mse
        RETURNING VALUE(equalized_harmonized_mse) TYPE harmonized_mse,
      equalize_harmonized
        IMPORTING harmonized_mse                  TYPE harmonized_mse
        RETURNING VALUE(equalized_harmonized_mse) TYPE harmonized_mse.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_mse_harmonize IMPLEMENTATION.

  METHOD mse_2_harmonized.

  ENDMETHOD.

  METHOD equalize_harmonized.

    equalized_harmonized_mse = harmonized_mse.
    SORT equalized_harmonized_mse.
    LOOP AT equalized_harmonized_mse ASSIGNING FIELD-SYMBOL(<eq>).
      CONDENSE <eq>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
