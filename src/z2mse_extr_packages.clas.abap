"! Selects packages in case objects to be analyzed are selected by packages.
"! Will be tested by transferring test data to the constructor.
"! Has Unit Tests.
CLASS z2mse_extr_packages DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_s_pack                         TYPE RANGE OF tadir-devclass.

    TYPES: BEGIN OF ty_package,
             package       TYPE devclass,
             parentpackage TYPE parentcl,
           END OF ty_package.
    TYPES ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    TYPES:
      BEGIN OF ty_tdevc_test,
        devclass TYPE devclass,
        parentcl TYPE parentcl,
      END OF ty_tdevc_test.
    TYPES ty_t_tdevc_test TYPE HASHED TABLE OF ty_tdevc_test WITH UNIQUE KEY devclass.

    DATA g_selected_packages TYPE ty_packages READ-ONLY.

    "! @parameter tdevc_test | provide test data for table TDEVC during unit tests.
    METHODS constructor
      IMPORTING
        !tdevc_test TYPE ty_t_tdevc_test OPTIONAL.

    "! Select packages
    "! @parameter top_packages | Select packages
    "! @parameter sub_packages_filter | Optional: Include sub packages only if they are filtered by this filter
    "! @parameter including_sub_packages | Default false: Search sub packages
    METHODS select_packages
      IMPORTING
        !top_packages           TYPE ty_s_pack
        !sub_packages_filter    TYPE ty_s_pack OPTIONAL
        !including_sub_packages TYPE abap_bool DEFAULT abap_false.

    METHODS add_selected_packages_to_model
      IMPORTING
        sap_package TYPE REF TO z2mse_sap_package.
    METHODS add_selected_packages_to_mode2
      IMPORTING
        famix_package TYPE REF TO z2mse_famix_package.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_package_store,
             package           TYPE devclass,
             parentpackage     TYPE parentcl,
             subclass_searched TYPE abap_bool,
             is_to_be_returned TYPE abap_bool,
           END OF ty_package_store.
    TYPES ty_packages_store TYPE HASHED TABLE OF ty_package_store WITH UNIQUE KEY package.
    "! Filled during tests
    DATA g_tdevc_test TYPE ty_t_tdevc_test.
    DATA g_is_test TYPE abap_bool.
    METHODS _select_top_packages
      IMPORTING
        i_top_packages    TYPE z2mse_extr_packages=>ty_s_pack
      RETURNING
        VALUE(r_packages) TYPE z2mse_extr_packages=>ty_packages.
    TYPES:
      ty_packages_to_search_sub TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY package.
    METHODS _select_sub_packages
      IMPORTING
        i_packages_to_search_sub TYPE ty_packages_to_search_sub
      RETURNING
        VALUE(r_packages)        TYPE z2mse_extr_packages=>ty_packages.
ENDCLASS.



CLASS z2mse_extr_packages IMPLEMENTATION.

  METHOD constructor.
    IF tdevc_test IS SUPPLIED.
      g_tdevc_test = tdevc_test.
      g_is_test = abap_true.
    ENDIF.
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

  METHOD add_selected_packages_to_model.

    DATA: selected_package  TYPE z2mse_extr_packages=>ty_package.

    LOOP AT g_selected_packages INTO selected_package.

      sap_package->add( name = selected_package-package ).

      IF selected_package-parentpackage IS NOT INITIAL.

        sap_package->add( name = selected_package-parentpackage ).
        sap_package->set_parent_package( EXPORTING this_package   = selected_package-package
                                                   parent_package = selected_package-parentpackage ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_selected_packages_to_mode2.

    DATA: selected_package  TYPE z2mse_extr_packages=>ty_package.

    LOOP AT g_selected_packages INTO selected_package.

      famix_package->add( name = selected_package-package ).

      IF selected_package-parentpackage IS NOT INITIAL.

        famix_package->add( name = selected_package-parentpackage ).
        famix_package->set_parent_package( element_id = 0
                                     element_type = 'FAMIX.Package'
                                     element_name_group = ''
                                     element_name = selected_package-package
                                     parent_package = selected_package-parentpackage ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
