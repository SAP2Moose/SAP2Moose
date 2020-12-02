"! I build all initial elements that are the starting point for searching further elements.
CLASS z2mse_extr3_initial_elements DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_package,
             package       TYPE devclass,
             parentpackage TYPE parentcl,
           END OF ty_package.
    TYPES ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.

    CONSTANTS: select_class_method TYPE string VALUE 'Class',
               select_table        TYPE string VALUE 'Table',
               select_program      TYPE string VALUE 'Program',
               select_function     TYPE string VALUE 'Function'.

    TYPES: ty_s_pack TYPE RANGE OF tadir-devclass .
    TYPES:
      BEGIN OF ty_tdevc_test,
        devclass TYPE devclass,
        parentcl TYPE parentcl,
      END OF ty_tdevc_test.
    TYPES ty_t_tdevc_test TYPE HASHED TABLE OF ty_tdevc_test WITH UNIQUE KEY devclass.
    METHODS select_packages
      IMPORTING
        !top_packages           TYPE ty_s_pack
        !sub_packages_filter    TYPE ty_s_pack OPTIONAL
        !including_sub_packages TYPE abap_bool DEFAULT abap_false.
    TYPES: ty_filter TYPE string.
    METHODS select_specific
      IMPORTING
        model_builder         TYPE REF TO z2mse_extr3_model_builder
        element_manager       TYPE REF TO z2mse_extr3_element_manager
        i_element_type_filter TYPE ty_filter
        i_parent_name_filter  TYPE ty_filter
        i_name_filter         TYPE ty_filter.
    METHODS get_selected
      RETURNING VALUE(r_packages) TYPE ty_packages.

    "! @parameter tdevc_test | provide test data for table TDEVC during unit tests.
    METHODS constructor
      IMPORTING
        !tdevc_test TYPE ty_t_tdevc_test OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_package_store,
             package           TYPE devclass,
             parentpackage     TYPE parentcl,
             subclass_searched TYPE abap_bool,
             is_to_be_returned TYPE abap_bool,
           END OF ty_package_store.
    TYPES ty_packages_store TYPE HASHED TABLE OF ty_package_store WITH UNIQUE KEY package.

    DATA g_selected_packages TYPE ty_packages.
    "! Select packages according to filter transfered by report
    "! @parameter top_packages | Select packages
    "! @parameter sub_packages_filter | Optional: Include sub packages only if they are filtered by this filter
    "! @parameter including_sub_packages | Default false: Search sub packages
    "! Filled during tests
    DATA g_tdevc_test TYPE ty_t_tdevc_test.
    DATA g_is_test TYPE abap_bool.
    METHODS _select_top_packages
      IMPORTING
        i_top_packages    TYPE z2mse_extr3_packages=>ty_s_pack
      RETURNING
        VALUE(r_packages) TYPE z2mse_extr3_packages=>ty_packages.
    TYPES:
      ty_packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    METHODS _select_sub_packages
      IMPORTING
        i_packages_to_search_sub TYPE ty_packages_to_search_sub
      RETURNING
        VALUE(r_packages)        TYPE z2mse_extr3_packages=>ty_packages.
    METHODS _select_class
      IMPORTING
        name_filter           TYPE z2mse_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_class_method
      IMPORTING
        name_filter           TYPE z2mse_extr3_initial_elements=>ty_filter
        parent_name_filter    TYPE z2mse_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_table
      IMPORTING
        name_filter           TYPE z2mse_extr3_initial_elements=>ty_filter
        element_manager       TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_program
      IMPORTING
        element_manager       TYPE REF TO z2mse_extr3_element_manager
        name_filter           TYPE z2mse_extr3_initial_elements=>ty_filter
      RETURNING
        VALUE(new_element_id) TYPE i.
    METHODS _select_function
      IMPORTING
        element_manager       TYPE REF TO z2mse_extr3_element_manager
        name_filter           TYPE z2mse_extr3_initial_elements=>ty_filter
      RETURNING
        VALUE(new_element_id) TYPE i.
ENDCLASS.



CLASS z2mse_extr3_initial_elements IMPLEMENTATION.


  METHOD constructor.
    IF tdevc_test IS SUPPLIED.
      g_tdevc_test = tdevc_test.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_selected.

    r_packages = g_selected_packages.

  ENDMETHOD.


  METHOD select_packages.

    DATA: package  TYPE ty_package,
          packages TYPE ty_packages.

    DATA: package_store  TYPE ty_package_store,
          packages_store TYPE ty_packages_store.

    FIELD-SYMBOLS <packages_store> TYPE ty_package_store.

    CLEAR g_selected_packages.

    packages = _select_top_packages( top_packages ).

    LOOP AT packages INTO package.

      CLEAR package_store.
      package_store-package = package-package.
      package_store-is_to_be_returned = abap_true.
      INSERT package_store INTO TABLE packages_store.

    ENDLOOP.

    IF including_sub_packages EQ abap_true.

      DATA: packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package,
            something_to_search    TYPE abap_bool.

      something_to_search = abap_true.

      WHILE something_to_search EQ abap_true.

        CLEAR packages_to_search_sub.

        LOOP AT packages_store ASSIGNING <packages_store> WHERE subclass_searched EQ abap_false.

          <packages_store>-subclass_searched = abap_true.
          package-package = <packages_store>-package.
          INSERT package INTO TABLE packages_to_search_sub.

        ENDLOOP.
        IF packages_to_search_sub IS NOT INITIAL.

          packages = _select_sub_packages( i_packages_to_search_sub = packages_to_search_sub ).

          LOOP AT packages INTO package.
            CLEAR package_store.
            package_store-package = package-package.
            package_store-parentpackage = package-parentpackage.
            IF package-package IN sub_packages_filter.
              package_store-is_to_be_returned = abap_true.
            ENDIF.
            INSERT package_store INTO TABLE packages_store.
          ENDLOOP.

        ELSE.
          something_to_search = abap_false.
        ENDIF.
      ENDWHILE.

    ENDIF.

    LOOP AT packages_store INTO package_store WHERE is_to_be_returned = abap_true.

      package-package = package_store-package.
      package-parentpackage = package_store-parentpackage.
      INSERT package INTO TABLE g_selected_packages.

    ENDLOOP.

  ENDMETHOD.


  METHOD select_specific.

    DATA new_element_id TYPE i.

    model_builder->initial_selection_started( ).
    model_builder->usage_of_single_element( ).

    CASE i_element_type_filter.
      WHEN z2mse_extr3_initial_elements=>select_class_method.

        IF i_parent_name_filter IS INITIAL.

          new_element_id = _select_class( name_filter        = i_name_filter
                                          element_manager    = element_manager ).

        ELSE.

          new_element_id = _select_class_method( name_filter        = i_name_filter
                                                 parent_name_filter = i_parent_name_filter
                                                 element_manager    = element_manager ).

        ENDIF.

      WHEN z2mse_extr3_initial_elements=>select_table.

        new_element_id = _select_table( name_filter     = i_name_filter
                                        element_manager = element_manager ).

      WHEN z2mse_extr3_initial_elements=>select_program.

        new_element_id = _select_program( element_manager = element_manager
                                          name_filter     = i_name_filter ).

      WHEN z2mse_extr3_initial_elements=>select_function.

        new_element_id = _select_function( element_manager = element_manager
                                           name_filter     = i_name_filter ).

      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    model_builder->new_element_id( EXPORTING i_element_id  = new_element_id
                                             i_is_specific = abap_true ).

  ENDMETHOD.


  METHOD _select_sub_packages.

    CLEAR r_packages.

    IF g_is_test EQ abap_false.

      SELECT devclass AS package parentcl AS parentpackage FROM tdevc INTO TABLE r_packages
        FOR ALL ENTRIES IN i_packages_to_search_sub
        WHERE parentcl = i_packages_to_search_sub-package.

    ELSE.

      DATA: package      TYPE ty_package,
            package_test TYPE ty_tdevc_test.

      LOOP AT g_tdevc_test INTO package_test.
        READ TABLE i_packages_to_search_sub TRANSPORTING NO FIELDS WITH TABLE KEY package = package_test-parentcl.
        IF sy-subrc EQ 0.
          package-package = package_test-devclass.
          package-parentpackage = package_test-parentcl.
          INSERT package INTO TABLE r_packages.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD _select_top_packages.

    CLEAR r_packages.

    IF g_is_test EQ abap_false.
      SELECT devclass AS package parentcl AS parentpackage FROM tdevc INTO TABLE r_packages WHERE devclass IN i_top_packages
        ORDER BY devclass.
    ELSE.
      DATA package TYPE ty_package.
      LOOP AT g_tdevc_test INTO package WHERE devclass IN i_top_packages.
        INSERT package INTO TABLE r_packages.
      ENDLOOP.
      SORT r_packages BY package.
    ENDIF.

  ENDMETHOD.

  METHOD _select_class_method.

    " Select class method

    DATA classes TYPE REF TO z2mse_extr3_classes.
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
    classes->add_component( EXPORTING clsname        = parent_name_filter
                                      cmpname        = name_filter
                                      is_specific    = abap_false
                            IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.

  METHOD _select_class.

    DATA: classes          TYPE REF TO z2mse_extr3_classes,
          class_components TYPE z2mse_extr3_classes=>ty_class_components,
          cc               TYPE z2mse_extr3_classes=>ty_class_component.
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
    classes->add( EXPORTING class            = name_filter
                            is_specific      = abap_true
                  IMPORTING new_element_id   = new_element_id
                            class_components = class_components ).

    LOOP AT class_components INTO cc.
      classes->add_component(
        EXPORTING
          clsname        = cc-clsname
          cmpname        = cc-cmpname
          is_specific    = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD _select_table.

    " Select table

    DATA tables TYPE REF TO z2mse_extr3_tables.
    tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
    tables->add( EXPORTING table          = name_filter
                 IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.


  METHOD _select_program.

    " Select program

    DATA programname TYPE program.
    programname = name_filter.

    DATA programs TYPE REF TO z2mse_extr3_programs.
    programs = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).
    programs->add( EXPORTING program        = programname
                   IMPORTING new_element_id = new_element_id
    ).

  ENDMETHOD.


  METHOD _select_function.

    " Select function

    DATA programs2 TYPE REF TO z2mse_extr3_programs.
    programs2 = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).
    programs2->add_function( EXPORTING function       = name_filter
                             IMPORTING new_element_id = new_element_id ).

  ENDMETHOD.

ENDCLASS.
