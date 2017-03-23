CLASS z2mse_extr_include DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_include,
             include TYPE programm,
           END OF ty_include.
    TYPES: ty_includes_hashed TYPE HASHED TABLE OF ty_include WITH UNIQUE KEY include.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr_include IMPLEMENTATION.
ENDCLASS.
