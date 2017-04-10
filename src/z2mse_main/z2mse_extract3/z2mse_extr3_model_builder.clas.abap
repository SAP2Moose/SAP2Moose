"! I know the level where an element was added to the model.
"! I know whether it was found in upward or downward search.
CLASS z2mse_extr3_model_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS search
      IMPORTING
        i_search_up   TYPE i
        i_search_down TYPE i.
    "! I am called once to notify that the initial selection of elements is started.
    "! All elements added to the model before my method search is called belong to the level 0
    METHODS initial_selection_started.
    "! Called whenever a new element ID was added to the model.
    "! @parameters i_is_specific | set to true if the element is added due to a specific search. It is for instance to be false, if all components of a class are added.
    METHODS new_element_id
      IMPORTING
        i_element_id  TYPE i
        i_is_specific TYPE abap_bool.
    METHODS initialize
      IMPORTING i_element_manager TYPE REF TO z2mse_extr3_element_manager.
    METHODS write_found_elements.
    METHODS usage_of_single_element.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF found_in_level_type,
             element_id                  TYPE z2mse_extr3_element_manager=>element_id_type,
             "! A flag to mark all elements that are part of the initial selection
             found_in_initial_selection  TYPE abap_bool,
             "! Elements that where added when the main search is finished
             found_in_post_selection     TYPE abap_bool,
             "! Marks that an initially selected element is analyzed for lower and higher levels as requested
             initially_selected_analyzed TYPE abap_bool,
             "! The level where an element is first found. Needed to stop searching as specified.
             "! Also required to determine whether an upward or downward search will be done.
             found_in_level_upsearch     TYPE i,
             found_in_level_downsearch   TYPE i,
             "! Used to analyze usages of a single element.
             "! Marks an element that is specifically marked.
             "! In case a specific search is done, only elements with this flag are used for an where used analysis
             specific                    TYPE abap_bool,
           END OF found_in_level_type.
    TYPES found_in_levels_type TYPE HASHED TABLE OF found_in_level_type WITH UNIQUE KEY element_id.

    DATA: found_in_levels               TYPE found_in_levels_type,
          is_initial_selection          TYPE abap_bool,
          is_up_search                  TYPE abap_bool,
          "! Add newly found elements during upsearch in this level
          level_for_found_in_upsearch   TYPE i,
          is_down_search                TYPE abap_bool,
          "! Add newly found elements during downsearch in this level
          level_for_found_in_downsearch TYPE i,
          is_post_selection             TYPE abap_bool.

    TYPES: BEGIN OF builder_type,
             association_builder TYPE REF TO z2mse_extr3_association_build,
           END OF builder_type.
    DATA element_manager TYPE REF TO z2mse_extr3_element_manager.
    "! Use for initial search
    DATA association_builders_init TYPE STANDARD TABLE OF builder_type.
    "! Use for final search
    DATA association_builders_post TYPE STANDARD TABLE OF builder_type.
    "! Use for search
    DATA association_builders TYPE STANDARD TABLE OF builder_type.

    DATA: tadir_builder      TYPE REF TO z2mse_extr3_tadir_builder,
          where_used_builder TYPE REF TO z2mse_extr3_where_used_builder.
    "! A single element is analyzed of usage and using
    DATA is_usage_of_single_element TYPE abap_bool.

ENDCLASS.



CLASS z2mse_extr3_model_builder IMPLEMENTATION.


  METHOD initialize.

    element_manager = i_element_manager.

    DATA association_builder_init TYPE builder_type.

    CREATE OBJECT tadir_builder EXPORTING i_element_manager = i_element_manager.

    association_builder_init-association_builder = tadir_builder.
    INSERT association_builder_init INTO TABLE association_builders_init.

    DATA association_builder_post TYPE builder_type.

    CREATE OBJECT tadir_builder EXPORTING i_element_manager = i_element_manager.

    association_builder_post-association_builder = tadir_builder.
    INSERT association_builder_post INTO TABLE association_builders_post.

    DATA association_builder TYPE builder_type.

    CREATE OBJECT where_used_builder EXPORTING i_element_manager = i_element_manager.

    association_builder-association_builder = where_used_builder.
    INSERT association_builder INTO TABLE association_builders.

  ENDMETHOD.
  METHOD usage_of_single_element.
    is_usage_of_single_element = abap_true.
  ENDMETHOD.

  METHOD initial_selection_started.
    is_initial_selection = abap_true.
  ENDMETHOD.


  METHOD new_element_id.

    DATA: found_in_level TYPE found_in_level_type,
          is_new_line    TYPE abap_bool.
    FIELD-SYMBOLS <found_in_level> TYPE found_in_level_type.

    READ TABLE found_in_levels ASSIGNING <found_in_level> WITH TABLE KEY element_id = i_element_id.
    IF sy-subrc <> 0.

      is_new_line = abap_true.
      ASSIGN found_in_level TO <found_in_level>.
      <found_in_level>-element_id = i_element_id.

    ENDIF.

    IF is_initial_selection EQ abap_true.

      <found_in_level>-found_in_initial_selection = abap_true.
      IF i_is_specific EQ abap_true.

        <found_in_level>-specific = abap_true.

      ENDIF.

    ELSEIF is_up_search EQ abap_true.

      IF <found_in_level>-found_in_level_upsearch IS INITIAL.

        <found_in_level>-found_in_level_upsearch = level_for_found_in_upsearch.

      ENDIF.

      IF     <found_in_level>-found_in_level_upsearch EQ level_for_found_in_upsearch
         AND i_is_specific EQ abap_true.
        <found_in_level>-specific = abap_true.
      ENDIF.

    ELSEIF is_down_search EQ abap_true.

      IF <found_in_level>-found_in_level_downsearch IS INITIAL.

        <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch.

      ENDIF.

      IF     <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch
         AND i_is_specific EQ abap_true.

        <found_in_level>-specific = abap_true.

      ENDIF.

    ELSEIF is_post_selection EQ abap_true.

      <found_in_level>-found_in_post_selection = abap_true.

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

    IF is_new_line EQ abap_true.

      INSERT <found_in_level> INTO TABLE found_in_levels.
      ASSERT sy-subrc EQ 0.

    ENDIF.

  ENDMETHOD.


  METHOD search.

    " Initial search

    DATA: found_in_level         TYPE found_in_level_type,
          first_initial_elements TYPE found_in_levels_type.
    FIELD-SYMBOLS: <found_in_level>         TYPE found_in_level_type.


    " found_in_levels will be updated in this method, so add this elements to a new temporary table.

    LOOP AT found_in_levels ASSIGNING <found_in_level> WHERE found_in_initial_selection EQ abap_true.

      <found_in_level>-initially_selected_analyzed = abap_true.

      INSERT <found_in_level> INTO TABLE first_initial_elements.

    ENDLOOP.

    DATA association_builder TYPE builder_type.

    LOOP AT association_builders_init INTO association_builder.

      LOOP AT first_initial_elements INTO found_in_level.

        IF     is_usage_of_single_element EQ abap_true
           AND found_in_level-specific EQ abap_false.
          CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
        ENDIF.

        association_builder-association_builder->search_down( element_id = found_in_level-element_id ).

      ENDLOOP.

    ENDLOOP.

    is_initial_selection = abap_false.

    " Search up

    is_up_search = abap_true.

    DATA: level_to_search_up      TYPE i,
          something_to_be_done_up TYPE abap_bool.

    IF i_search_up <> 0.

      something_to_be_done_up = abap_true.

      WHILE something_to_be_done_up EQ abap_true.

        level_to_search_up = level_for_found_in_upsearch.
        ADD 1 TO level_for_found_in_upsearch.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Search up for level { level_to_search_up }|.

        something_to_be_done_up = abap_false.
        DATA workload TYPE found_in_levels_type.
        CLEAR workload.
        LOOP AT found_in_levels INTO found_in_level WHERE found_in_level_upsearch = level_to_search_up.

          IF     is_usage_of_single_element EQ abap_true
             AND found_in_level-specific EQ abap_false.
            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
          ENDIF.

          INSERT found_in_level INTO TABLE workload.
        ENDLOOP.


        LOOP AT workload INTO found_in_level.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_up( element_id = found_in_level-element_id ).

          ENDLOOP.

          something_to_be_done_up = abap_true.
        ENDLOOP.

        IF i_search_up >= 0.

          IF i_search_up >= level_for_found_in_upsearch.

            something_to_be_done_up = abap_false.

          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDIF.

    is_up_search = abap_false.

    " Search down

    is_down_search = abap_true.

    DATA: level_to_search_down      TYPE i,
          something_to_be_done_down TYPE abap_bool.

    IF i_search_down <> 0.

      something_to_be_done_down = abap_true.

      WHILE something_to_be_done_down EQ abap_true.

        level_to_search_down = level_for_found_in_downsearch.
        ADD 1 TO level_for_found_in_downsearch.

        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Search up for level { level_to_search_down }|.

        something_to_be_done_down = abap_false.
        CLEAR workload.
        LOOP AT found_in_levels INTO found_in_level WHERE found_in_level_downsearch = level_to_search_down.
          INSERT found_in_level INTO TABLE workload.
        ENDLOOP.
        LOOP AT workload INTO found_in_level.

          IF     is_usage_of_single_element EQ abap_true
             AND found_in_level-specific EQ abap_false.
            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
          ENDIF.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_down( element_id = found_in_level-element_id ).

          ENDLOOP.

          something_to_be_done_down = abap_true.
        ENDLOOP.

        IF i_search_down >= 0.

          IF i_search_down >= level_for_found_in_downsearch.

            something_to_be_done_down = abap_false.

          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDIF.

    is_down_search = abap_false.

    " Post search

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = |Final actions of search|.

    is_post_selection = abap_true.

    DATA all_elements TYPE found_in_levels_type.

    all_elements = found_in_levels.

    LOOP AT association_builders_post INTO association_builder.

      LOOP AT all_elements INTO found_in_level.

        IF     is_usage_of_single_element EQ abap_true
           AND found_in_level-specific EQ abap_false.
          CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
        ENDIF.

        association_builder-association_builder->search_up( element_id = found_in_level-element_id ).

      ENDLOOP.

    ENDLOOP.

    is_post_selection = abap_false.

  ENDMETHOD.


  METHOD write_found_elements.

    DATA found_in_level TYPE found_in_level_type.

    TYPES: BEGIN OF temp_type,
             where           TYPE c LENGTH 1,
             level           TYPE i,
             "! Not zero in case an element is found in up and down search. If filled this is the level for downsearch
             alternate_level TYPE i,
             element_type    TYPE string,
             parent_name     TYPE string,
             name            TYPE string,
             specific        TYPE abap_bool,
           END OF temp_type.
    DATA: temp  TYPE temp_type,
          temps TYPE STANDARD TABLE OF temp_type.

    LOOP AT found_in_levels INTO found_in_level.
      CLEAR temp.
      temp-specific = found_in_level-specific.
      IF found_in_level-found_in_initial_selection EQ abap_true.
        temp-where = |I|.
        temp-level = 0.

      ELSEIF found_in_level-found_in_post_selection EQ abap_true.
        temp-where = |P|.
        temp-level = 0.

      ELSEIF found_in_level-found_in_level_upsearch <> 0
         AND found_in_level-found_in_level_downsearch <> 0.

        temp-where = |S|.
        temp-level = found_in_level-found_in_level_upsearch.
        temp-alternate_level = -1 * found_in_level-found_in_level_downsearch.

      ELSEIF found_in_level-found_in_level_upsearch <> 0.

        temp-where = |S|.
        temp-level = found_in_level-found_in_level_upsearch.

      ELSEIF found_in_level-found_in_level_downsearch <> 0.

        temp-where = |S|.
        temp-level = -1 * found_in_level-found_in_level_downsearch.

      ELSE.
        ASSERT 1 = 2.
      ENDIF.

      DATA element TYPE REF TO z2mse_extr3_elements.

      element = element_manager->get_element( i_element_id = found_in_level-element_id ).

      element->name( EXPORTING element_id   = found_in_level-element_id
                     IMPORTING element_type = temp-element_type
                               parent_name  = temp-parent_name
                               name         = temp-name ).

      INSERT temp INTO TABLE temps.

    ENDLOOP.

    SORT temps BY where level alternate_level element_type parent_name name.

    WRITE: / 'Legend:'.
    WRITE: / 'W - "I" Found in initial search "P" Found in final search (packages that where not initially selected) "S" Found in regular search'.
    WRITE: / 'L - Level where an element is found'.
    WRITE: / 'AL - In case an element is found in down and up search, this is the level where it is found in down search'.
    WRITE: /.
    FORMAT COLOR COL_HEADING.
    WRITE: /(1) 'W',
           (3) 'L',
           (3) 'AL',
           (30) 'Type',
           (30) 'Name of Parent',
           (30) 'Name'.
    FORMAT COLOR COL_BACKGROUND.

    LOOP AT temps INTO temp.
      IF temp-specific EQ abap_true.
        FORMAT COLOR COL_POSITIVE.
      ELSE.
        FORMAT COLOR COL_BACKGROUND.
      ENDIF.
      NEW-LINE.
      WRITE: /(1) temp-where,
              (3) temp-level,
              (3) temp-alternate_level,
              (30) temp-element_type,
              (30) temp-parent_name,
              (30) temp-name.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
