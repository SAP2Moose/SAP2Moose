CLASS z2mse_extr_object_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF object_list_type,
           "! A unique identifier for each object extracted
            obj_id TYPE i,
            "! The level where an element is first found. Needed to stop searching as specified.
            "! Also required to determine whether an upward or downward search will be done.
            found_in_level TYPE i,
           END OF object_list_type.
ENDCLASS.



CLASS z2mse_extr_object_manager IMPLEMENTATION.
ENDCLASS.
