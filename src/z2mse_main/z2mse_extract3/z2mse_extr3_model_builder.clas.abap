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
    METHODS initialize
      IMPORTING element_manager TYPE REF TO z2mse_extr3_element_manager.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF found_in_level_type,
             element_id                  TYPE z2mse_extr3_element_manager=>element_id_type,
             "! A flag to mark all elements that are part of the initial selection
             found_in_initial_selection  TYPE abap_bool,
             "! Marks that an initially selected element is analyzed for lower and higher levels as requested
             initially_selected_analyzed TYPE abap_bool,
             "! The level where an element is first found. Needed to stop searching as specified.
             "! Also required to determine whether an upward or downward search will be done.
             found_in_level_upsearch     TYPE i,
             found_in_level_downsearch   TYPE i,
           END OF found_in_level_type.
    TYPES found_in_levels_type TYPE HASHED TABLE OF found_in_level_type WITH UNIQUE KEY element_id.

    DATA: found_in_levels      TYPE found_in_levels_type,
          is_initial_selection TYPE abap_bool.

    TYPES: BEGIN OF builder_type,
             association_builder TYPE REF TO z2mse_extr3_association_build,
           END OF builder_type.
    DATA: association_builders TYPE STANDARD TABLE OF builder_type.

    DATA: tadir_builder TYPE REF TO z2mse_extr3_tadir_builder.

ENDCLASS.



CLASS z2mse_extr3_model_builder IMPLEMENTATION.

  METHOD initialize.

    DATA association_builder TYPE builder_type.

    CREATE OBJECT tadir_builder EXPORTING i_element_manager = element_manager.

    association_builder-association_builder = tadir_builder.
    INSERT association_builder INTO TABLE association_builders.

  ENDMETHOD.


  METHOD initial_selection_started.
    is_initial_selection = abap_true.
  ENDMETHOD.


  METHOD search.
    DATA: found_in_level         TYPE found_in_level_type,
          first_initial_elements TYPE found_in_levels_type.
    FIELD-SYMBOLS: <found_in_level>         TYPE found_in_level_type.

    " found_in_levels will be updated in this method, so add this elements to a new temporary table.

    LOOP AT found_in_levels ASSIGNING <found_in_level> WHERE found_in_initial_selection EQ abap_true.

      <found_in_level>-initially_selected_analyzed = abap_true.

      INSERT found_in_level INTO TABLE first_initial_elements.

    ENDLOOP.

    DATA association_builder TYPE builder_type.

    LOOP AT association_builders INTO association_builder.

      LOOP AT first_initial_elements INTO found_in_level.

        association_builder-association_builder->search_down( element_id = found_in_level-element_id ).

      ENDLOOP.

    ENDLOOP.

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
