CLASS z2mse_extract_sap2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_s_pack TYPE RANGE OF tadir-devclass .
    CONSTANTS modifier_abapglobalclass TYPE string VALUE 'ABAPGlobalClass' ##NO_TEXT.
    CONSTANTS modifier_abapglobalinterface TYPE string VALUE 'ABAPGlobalInterface' ##NO_TEXT.
    CONSTANTS modifier_webdynpro_component TYPE string VALUE 'ABAPWebDynproComponent'.
    CONSTANTS modifier_dbtable TYPE string VALUE 'DBTable' ##NO_TEXT.
    METHODS constructor .
    METHODS extract
      IMPORTING
        !i_top_packages        TYPE ty_s_pack
        !i_sub_packages_filter TYPE ty_s_pack
        !i_search_sub_packages TYPE abap_bool
      EXPORTING
        !mse_model             TYPE z2mse_model=>lines_type
        VALUE(nothing_done)    TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA model            TYPE REF TO z2mse_model.
    DATA famix_package     TYPE REF TO z2mse_famix_package.
    DATA famix_class     TYPE REF TO Z2MSE_famix_class.
    DATA famix_method     TYPE REF TO z2mse_famix_method.
    DATA famix_attribute     TYPE REF TO Z2MSE_famix_attribute.
    DATA famix_invocation     TYPE REF TO z2mse_famix_invocation.
    DATA famix_access     TYPE REF TO Z2MSE_famix_access.
    METHODS _add_all_to_model_and_make_mse
      IMPORTING
        i_extract_packages       TYPE REF TO z2mse_extr_packages
        i_extract_classes        TYPE REF TO z2mse_extr_classes
        i_extract_where_used_sap TYPE REF TO z2mse_extr_where_used_sap
      RETURNING
        VALUE(r_mse_model) TYPE z2mse_model=>lines_type.
    METHODS _initial_selections_by_filter
      IMPORTING
        i_top_packages        TYPE z2mse_extract_sap2=>ty_s_pack
        i_sub_packages_filter TYPE z2mse_extract_sap2=>ty_s_pack
        i_search_sub_packages TYPE abap_bool
        i_extract_packages    TYPE REF TO z2mse_extr_packages
        i_extract_classes     TYPE REF TO z2mse_extr_classes.
    METHODS _get_using_elements
      IMPORTING
        i_extract_classes        TYPE REF TO z2mse_extr_classes
        i_extract_where_used_sap TYPE REF TO z2mse_extr_where_used_sap.
ENDCLASS.



CLASS Z2MSE_EXTRACT_SAP2 IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT model.

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.

  ENDMETHOD.


  METHOD extract.
    DATA: extract_packages TYPE REF TO z2mse_extr_packages.

    TEST-SEAM creator_packages.
      CREATE OBJECT extract_packages.
    END-TEST-SEAM.

    DATA extract_classes TYPE REF TO z2mse_extr_classes.

    TEST-SEAM creator_classes.
      CREATE OBJECT extract_classes.
    END-TEST-SEAM.

    DATA extract_where_used_sap TYPE REF TO z2mse_extr_where_used_sap.

    TEST-SEAM creator_where_used_sap.
      CREATE OBJECT extract_where_used_sap.
    END-TEST-SEAM.

    _initial_selections_by_filter( i_top_packages        = i_top_packages
                                   i_sub_packages_filter = i_sub_packages_filter
                                   i_search_sub_packages = i_search_sub_packages
                                   i_extract_packages    = extract_packages
                                   i_extract_classes     = extract_classes ).

    _get_using_elements( i_extract_classes        = extract_classes
                         i_extract_where_used_sap = extract_where_used_sap ).

    mse_model = _add_all_to_model_and_make_mse( i_extract_packages       = extract_packages
                                                i_extract_classes        = extract_classes
                                                i_extract_where_used_sap = extract_where_used_sap ).

  ENDMETHOD.

  METHOD _add_all_to_model_and_make_mse.

    " Add all to model and make mse

    i_extract_packages->add_selected_packages_to_mode2( famix_package = famix_package ).

    i_extract_classes->add_to_model2( EXPORTING famix_package   = famix_package
                                                              famix_class     = famix_class
                                                              famix_method    = famix_method
                                                              famix_attribute = famix_attribute ).

    i_extract_where_used_sap->add_usage_to_model( EXPORTING famix_method    = famix_method
                                                          famix_attribute = famix_attribute
                                                          famix_invocation = famix_invocation
                                                          famix_access     = famix_access ).

    model->make_mse( IMPORTING mse_model = r_mse_model ).

  ENDMETHOD.


  METHOD _initial_selections_by_filter.

    " Initial selections due to filter of report

    i_extract_packages->select_packages( EXPORTING top_packages           = i_top_packages
                                                 sub_packages_filter    = i_sub_packages_filter
                                                 including_sub_packages = i_search_sub_packages  ).

    i_extract_classes->select_classes_by_packages( packages = i_extract_packages->g_selected_packages ).

  ENDMETHOD.


  METHOD _get_using_elements.

    " Get using elements

    i_extract_where_used_sap->used_by_class_component( class_components = i_extract_classes->get_comp_to_do_where_used( ) ).

    i_extract_classes->select_classes_by_components( components = i_extract_where_used_sap->get_components_where_used( ) ).

  ENDMETHOD.

ENDCLASS.
