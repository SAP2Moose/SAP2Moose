"! I know the level where an element was added to the model.
"! I know whether it was found in upward or downward search.
CLASS z2mse_extr3_model_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF found_element_type,
             where            TYPE c LENGTH 1,
             level            TYPE i,
             "! Not zero in case an element is found in up and down search. If filled this is the level for downsearch
             alternate_level  TYPE i,
             element_type     TYPE string,
             parent_name      TYPE string,
             name             TYPE string,
             specific         TYPE abap_bool,
             up_search_done   TYPE abap_bool,
             down_search_done TYPE abap_bool,
           END OF found_element_type.
    TYPES: found_elements_type TYPE STANDARD TABLE OF found_element_type.
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
      IMPORTING i_element_manager TYPE REF TO z2mse_extr3_element_manager
                i_dynamic_read    TYPE string OPTIONAL.
    METHODS write_found_elements
      IMPORTING
        write TYPE abap_bool OPTIONAL
      EXPORTING
        fes   TYPE found_elements_type.
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
    METHODS _post_search.
    METHODS _initial_search.
    METHODS _search_up
      IMPORTING
        i_search_up TYPE i.
    METHODS _search_down
      IMPORTING
        i_search_down TYPE i.

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
    where_used_builder->set_dynamic_read( i_dynamic_read = i_dynamic_read ).

    association_builder-association_builder = where_used_builder.
    INSERT association_builder INTO TABLE association_builders.

  ENDMETHOD.


  METHOD initial_selection_started.
    is_initial_selection = abap_true.
  ENDMETHOD.


  METHOD new_element_id.

    DATA: found_in_level TYPE found_in_level_type,
          is_new_line    TYPE abap_bool.
    FIELD-SYMBOLS <found_in_level> TYPE found_in_level_type.

    ASSERT i_element_id IS NOT INITIAL.

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

      IF i_is_specific EQ abap_true AND <found_in_level>-specific EQ abap_false.

        <found_in_level>-found_in_level_upsearch = level_for_found_in_upsearch.
        <found_in_level>-specific = abap_true.

      ENDIF.

*      IF     <found_in_level>-found_in_level_upsearch EQ level_for_found_in_upsearch
*         AND i_is_specific EQ abap_true.
*
*        <found_in_level>-specific = abap_true.
*
*      ENDIF.

    ELSEIF is_down_search EQ abap_true.

      IF <found_in_level>-found_in_level_downsearch IS INITIAL.

        <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch.

      ENDIF.

      IF i_is_specific EQ abap_true AND <found_in_level>-specific EQ abap_false.

        <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch.
        <found_in_level>-specific = abap_true.

      ENDIF.

*      IF     <found_in_level>-found_in_level_downsearch = level_for_found_in_downsearch
*         AND i_is_specific EQ abap_true.
*
*        <found_in_level>-specific = abap_true.
*
*      ENDIF.

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

    _initial_search( ).

    _search_up( i_search_up ).

    _search_down( i_search_down ).

    _post_search( ).

  ENDMETHOD.


  METHOD usage_of_single_element.
    is_usage_of_single_element = abap_true.
  ENDMETHOD.


  METHOD write_found_elements.

    DATA fil TYPE found_in_level_type.
    DATA: fe    TYPE found_element_type.

    LOOP AT found_in_levels INTO fil.
      CLEAR fe.
      fe-specific = fil-specific.
      IF fil-found_in_initial_selection EQ abap_true.
        fe-where = |I|.

      ELSEIF fil-found_in_post_selection EQ abap_true.
        fe-where = |P|.

      ELSE.
        fe-where = |S|.
      ENDIF.

      IF fil-found_in_level_upsearch <> 0
         AND fil-found_in_level_downsearch <> 0.

        fe-level = fil-found_in_level_upsearch.
        fe-alternate_level = -1 * fil-found_in_level_downsearch.

      ELSEIF fil-found_in_level_upsearch <> 0.

        fe-level = fil-found_in_level_upsearch.

      ELSEIF fil-found_in_level_downsearch <> 0.

        fe-level = -1 * fil-found_in_level_downsearch.

      ELSE.
        fe-level = 0.
      ENDIF.

      DATA element TYPE REF TO z2mse_extr3_elements.

      element = element_manager->get_element( i_element_id = fil-element_id ).

      element->name( EXPORTING element_id   = fil-element_id
                     IMPORTING element_type = fe-element_type
                               parent_name  = fe-parent_name
                               name         = fe-name ).

      IF      is_usage_of_single_element EQ abap_true
          AND fe-specific EQ abap_false.

        CONTINUE.

      ENDIF.

      INSERT fe INTO TABLE fes.

    ENDLOOP.
    IF is_usage_of_single_element EQ abap_true.
      SORT fes BY level alternate_level element_type parent_name name.
    ELSE.
      SORT fes BY where level alternate_level element_type parent_name name.
    ENDIF.

    IF write EQ abap_true.

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
             (61) 'Name'.
      FORMAT COLOR COL_BACKGROUND.

      LOOP AT fes INTO fe.
        NEW-LINE.
        WRITE: /(1) fe-where,
                (3) fe-level,
                (3) fe-alternate_level,
                (30) fe-element_type,
                (30) fe-parent_name,
                (61) fe-name. " Interface method and attributes can yield to very long names. So 2 times 30 plus 1 should be enough.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD _post_search.

    DATA found_in_level TYPE z2mse_extr3_model_builder=>found_in_level_type.
    DATA association_builder TYPE z2mse_extr3_model_builder=>builder_type.

    " Post search

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = 'Final actions of search'.

    is_post_selection = abap_true.

    DATA all_elements TYPE found_in_levels_type.

    all_elements = found_in_levels.

    LOOP AT association_builders_post INTO association_builder.

      LOOP AT all_elements INTO found_in_level.

*        IF     is_usage_of_single_element EQ abap_true
*           AND found_in_level-specific EQ abap_false.
*          CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
*        ENDIF.

        association_builder-association_builder->search_up( element_id = found_in_level-element_id ).

      ENDLOOP.

    ENDLOOP.

    is_post_selection = abap_false.

  ENDMETHOD.


  METHOD _initial_search.

    " Initial search

    DATA: found_in_level         TYPE found_in_level_type,
          first_initial_elements TYPE found_in_levels_type,
          association_builder    TYPE z2mse_extr3_model_builder=>builder_type.

    " found_in_levels will be updated in this method, so add this elements to a new temporary table.
    IF is_usage_of_single_element EQ abap_false.

      FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
      LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_initial_selection EQ abap_true.

        <fil>-initially_selected_analyzed = abap_true.

        INSERT <fil> INTO TABLE first_initial_elements.

      ENDLOOP.

      LOOP AT association_builders_init INTO association_builder.

        LOOP AT first_initial_elements INTO found_in_level.

          association_builder-association_builder->search_down( element_id = found_in_level-element_id ).

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    is_initial_selection = abap_false.

    IF is_usage_of_single_element EQ abap_false.

      " All initially selected elements are marked as specific so that they are correctly searched

      FIELD-SYMBOLS: <fil2>         TYPE found_in_level_type.
      LOOP AT found_in_levels ASSIGNING <fil2>.
        <fil2>-specific = abap_true.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD _search_up.

    " Search up

    is_up_search = abap_true.

    DATA: level_to_search_up      TYPE i,
          something_to_be_done_up TYPE abap_bool.

    IF i_search_up <> 0.

      something_to_be_done_up = abap_true.

      WHILE something_to_be_done_up EQ abap_true.

        DATA temp TYPE string.
        temp = |Search up for level { level_to_search_up }|. "To be 7.02 compatible
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = temp.

        something_to_be_done_up = abap_false.

        FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
        LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_level_upsearch = level_to_search_up.

          IF <fil>-specific EQ abap_false.

            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis

          ENDIF.

          level_for_found_in_upsearch = <fil>-found_in_level_upsearch + 1.

          DATA association_builder TYPE z2mse_extr3_model_builder=>builder_type.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_up( element_id = <fil>-element_id ).

          ENDLOOP.

          something_to_be_done_up = abap_true.

        ENDLOOP.

        ADD 1 TO level_to_search_up.

        IF i_search_up >= 0 AND i_search_up <= level_to_search_up.

          something_to_be_done_up = abap_false.

        ENDIF.

      ENDWHILE.

    ENDIF.

    " SAP_2_FAMIX_68        When more than a single level is searched up, the up search is not done for elements that where found in the search down
    " Fullfilled because the search down starts here

    is_up_search = abap_false.

  ENDMETHOD.


  METHOD _search_down.

    " Search down

    is_down_search = abap_true.

    DATA: level_to_search_down      TYPE i,
          something_to_be_done_down TYPE abap_bool.

    IF i_search_down <> 0.

      something_to_be_done_down = abap_true.

      WHILE something_to_be_done_down EQ abap_true.

        DATA temp TYPE string.
        temp = |Search down for level { level_to_search_down }|."To be 7.02 compatible
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR' EXPORTING text = temp.

        something_to_be_done_down = abap_false.

        FIELD-SYMBOLS: <fil>         TYPE found_in_level_type.
        LOOP AT found_in_levels ASSIGNING <fil> WHERE found_in_level_downsearch = level_to_search_down.
          IF
*          is_usage_of_single_element EQ abap_true
*             AND
             <fil>-specific EQ abap_false.
            CONTINUE. " Only a single element is analyzed, include only specific elements into where used analysis
          ENDIF.
          IF level_to_search_down EQ 0.
            IF <fil>-found_in_initial_selection EQ abap_false.
              " SAP_2_FAMIX_69      When more than a single level is searched down, the down search is not done for elements that where found in the search up
              CONTINUE. "Start searching down with the elements found in the initial selection. Ignore all that was found in upsearch
            ELSE.
              IF is_usage_of_single_element EQ abap_true AND (
                 <fil>-specific EQ abap_false OR
                 <fil>-found_in_level_upsearch > 0 ).
                CONTINUE. " No downsearch for elements that are in initially selected classes but are not initially selected.
                " They are not initially selected when found in level upsearch is greater than zero
              ENDIF.
            ENDIF.

          ENDIF.

          level_for_found_in_downsearch = <fil>-found_in_level_downsearch + 1.

          DATA association_builder TYPE z2mse_extr3_model_builder=>builder_type.

          LOOP AT association_builders INTO association_builder.

            association_builder-association_builder->search_down( element_id = <fil>-element_id ).

          ENDLOOP.

          something_to_be_done_down = abap_true.

        ENDLOOP.

        ADD 1 TO level_to_search_down.

        IF i_search_down >= 0 AND i_search_down <= level_to_search_down.

          something_to_be_done_down = abap_false.

        ENDIF.

      ENDWHILE.

    ENDIF.

    is_down_search = abap_false.

  ENDMETHOD.

ENDCLASS.
