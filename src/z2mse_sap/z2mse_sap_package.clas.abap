

CLASS z2mse_sap_package DEFINITION INHERITING FROM z2mse_sap
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES packages_type TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    METHODS constructor IMPORTING model TYPE REF TO z2mse_model.
    METHODS add
      IMPORTING
        name TYPE clike.
    "! Call once to set the parent package
    METHODS set_parent_package
      IMPORTING
        this_package   TYPE clike
        parent_package TYPE clike.
    "! Returns all packages that are stored up to this time
    METHODS get_all_packages
      RETURNING VALUE(packages) TYPE packages_type.
  PRIVATE SECTION.
    DATA: g_famix_package TYPE REF TO z2mse_famix_package.
    DATA: g_added_packages TYPE STANDARD TABLE OF devclass.
ENDCLASS.

CLASS z2mse_sap_package IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT g_famix_package EXPORTING model = model.
  ENDMETHOD.

  METHOD add.
    DATA name_typed TYPE devclass.
    g_famix_package->add( name = name ).
    name_typed = name.
    INSERT name_typed INTO TABLE g_added_packages.
  ENDMETHOD.

  METHOD set_parent_package.
    g_famix_package->set_parent_package( element_id = 0
                                         element_type = 'FAMIX.Package'
                                         element_name_group = ''
                                         element_name = this_package
                                         parent_package = parent_package ).
  ENDMETHOD.

  METHOD get_all_packages.
    SORT g_added_packages.
    DELETE ADJACENT DUPLICATES FROM g_added_packages.
    packages = g_added_packages.
  ENDMETHOD.

ENDCLASS.
