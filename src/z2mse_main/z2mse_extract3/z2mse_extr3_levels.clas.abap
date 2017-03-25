"! I know the level where an element was added to the model.
"! I know whether it was find in upward or downward search.
CLASS z2mse_extr3_levels DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF found_in_level_type,
             obj_id                    TYPE z2mse_extr3_element_manager=>element_id_type,
             "! The level where an element is first found. Needed to stop searching as specified.
             "! Also required to determine whether an upward or downward search will be done.
             found_in_level_upsearch   TYPE i,
             found_in_level_downsearch TYPE i,
           END OF found_in_level_type.
    TYPES found_in_levels_type TYPE HASHED TABLE OF found_in_level_type WITH UNIQUE KEY obj_id.

    DATA: found_in_levels TYPE found_in_levels_type.
ENDCLASS.



CLASS z2mse_extr3_levels IMPLEMENTATION.
ENDCLASS.
