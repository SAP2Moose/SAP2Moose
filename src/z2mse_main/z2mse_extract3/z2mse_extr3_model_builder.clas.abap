"! I know the level where an element was added to the model.
"! I know whether it was found in upward or downward search.
CLASS z2mse_extr3_model_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS search.
    "! I am called once to notify that the initial selection of elements is started.
    "! All elements added to the model before my method search is called belong to the level 0
    METHODS initial_selection_started.
    "! Called whenever a new element ID was added to the model.
    METHODS new_element_id
      IMPORTING
        i_element_id TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF found_in_level_type,
             element_id                 TYPE z2mse_extr3_element_manager=>element_id_type,
             "! A flag to mark all elements that are part of the initial selection
             found_in_initial_selection TYPE abap_bool,
             "! The level where an element is first found. Needed to stop searching as specified.
             "! Also required to determine whether an upward or downward search will be done.
             found_in_level_upsearch    TYPE i,
             found_in_level_downsearch  TYPE i,
           END OF found_in_level_type.
    TYPES found_in_levels_type TYPE HASHED TABLE OF found_in_level_type WITH UNIQUE KEY element_id.

    DATA: found_in_levels      TYPE found_in_levels_type,
          is_initial_selection TYPE abap_bool.
ENDCLASS.



CLASS z2mse_extr3_model_builder IMPLEMENTATION.


  METHOD initial_selection_started.
    is_initial_selection = abap_true.
  ENDMETHOD.


  METHOD search.
    is_initial_selection = abap_false.
  ENDMETHOD.

  METHOD new_element_id.
    DATA found_in_level TYPE found_in_level_type.

    IF is_initial_selection EQ abap_true.
      found_in_level-element_id = i_element_id.
      found_in_level-found_in_initial_selection = abap_true.
      INSERT found_in_level INTO TABLE found_in_levels.
    ELSE.
      ASSERT 1 = 2.
      "Unfinished coding
    ENDIF.

  ENDMETHOD.

ENDCLASS.
