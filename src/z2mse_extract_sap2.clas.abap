CLASS z2mse_extract_sap2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_s_pack                         TYPE RANGE OF tadir-devclass .
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
    DATA g_extract_packages TYPE REF TO z2mse_extr_packages.
ENDCLASS.



CLASS z2mse_extract_sap2 IMPLEMENTATION.


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

    TEST-SEAM creator_packages.
      CREATE OBJECT g_extract_packages.
    END-TEST-SEAM.

    g_extract_packages->select_packages( EXPORTING top_packages           = i_top_packages
                                                 sub_packages_filter    = i_sub_packages_filter
                                                 including_sub_packages = i_search_sub_packages  ).


    DATA extract_classes TYPE REF TO z2mse_extr_classes.

    TEST-SEAM creator_classes.
      CREATE OBJECT extract_classes.
    END-TEST-SEAM.

    extract_classes->select_classes_by_packages( packages = g_extract_packages->g_selected_packages ).

    DATA extract_where_used_sap TYPE REF TO z2mse_extr_where_used_sap.

    TEST-SEAM creator_where_used_sap.
      CREATE OBJECT extract_where_used_sap.
    END-TEST-SEAM.

    extract_where_used_sap->used_by_class_component( class_components = extract_classes->get_comp_to_do_where_used( ) ).

    extract_classes->select_classes_by_components( components = extract_where_used_sap->get_components_where_used( ) ).

    g_extract_packages->add_selected_packages_to_mode2( famix_package = famix_package ).
    extract_classes->add_to_model2( EXPORTING famix_package   = famix_package
                                                              famix_class     = famix_class
                                                              famix_method    = famix_method
                                                              famix_attribute = famix_attribute ).

    extract_where_used_sap->add_usage_to_model( EXPORTING famix_method    = famix_method
                                                          famix_attribute = famix_attribute
                                                          famix_invocation = famix_invocation
                                                          famix_access     = famix_access ).

    model->make_mse( IMPORTING mse_model = mse_model ).

  ENDMETHOD.


ENDCLASS.
