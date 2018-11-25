CLASS z2mse_mse_harmonize_maker DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA to_change TYPE z2mse_mse_harmonize=>harmonized_mse.
    METHODS constructor.
    METHODS add_package
      IMPORTING package TYPE string.
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_db_table
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_class
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_method
      IMPORTING class   TYPE string
                method  TYPE string
                at_line TYPE i OPTIONAL.
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_interface_method
      IMPORTING interface   TYPE string
                method  TYPE string
                at_line TYPE i OPTIONAL.
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_interface
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    METHODS add_web_dynpro_component
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_program
      IMPORTING name          TYPE string
                parentpackage TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_mse_harmonize_maker IMPLEMENTATION.


  METHOD add_class.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } parentPackage { parentpackage }| ) ).

    ENDIF.

    DATA(low_name) = name.
    TRANSLATE low_name TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } modifiers ABAPGlobalClass| )
                                        ( |FAMIX.FileAnchor { name } fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/{ low_name }/source/main| )
                                        ( |FAMIX.Class { name } sourceAnchor| )  ).

  ENDMETHOD.

  METHOD add_method.

    DATA(low_class) = class.
    TRANSLATE low_class TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Method { class }>>{ method } signature { method }| ) ).

    IF at_line IS SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Method { class }>>{ method } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { method } fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/{ low_class }/source/main#start={ at_line },1| ) ).

    ENDIF.

  ENDMETHOD.

  METHOD add_interface_method.

    DATA(low_interface) = interface.
    TRANSLATE low_interface TO LOWER CASE.

    to_change = VALUE #( BASE to_change  ( |FAMIX.Method { interface }>>{ method } signature { method }| ) ).

    IF at_line IS SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Method { interface }>>{ method } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { method } fileName adt://{ sy-sysid }/sap/bc/adt/oo/interfaces/{ low_interface }/source/main#start={ at_line },1| ) ).

    ENDIF.

  ENDMETHOD.


  METHOD add_db_table.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name  } parentPackage {  parentpackage  }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name  } modifiers DBTable| )
                                        ( |FAMIX.Attribute { name  }>>{ name  }| )  ).

  ENDMETHOD.


  METHOD add_interface.

    DATA(low_name) = name.
    TRANSLATE low_name TO LOWER CASE.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } parentPackage { parentpackage }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } isInterface true| )
                                        ( |FAMIX.Class { name } modifiers ABAPGlobalInterface| )
                                        ( |FAMIX.FileAnchor { name } fileName adt://{ sy-sysid }/sap/bc/adt/oo/interfaces/{ low_name }/source/main| )
                                        ( |FAMIX.Class { name } sourceAnchor| )  ).

  ENDMETHOD.


  METHOD add_package.

    to_change = VALUE #( BASE to_change ( |FAMIX.Package { package }| ) ).

  ENDMETHOD.


  METHOD add_program.

    DATA(low_name) = name.
    TRANSLATE low_name TO LOWER CASE.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } parentPackage { parentpackage }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } modifiers ABAPProgram| )
                                        ( |FAMIX.Method { name }>>{ name } sourceAnchor| )
                                        ( |FAMIX.FileAnchor { name } fileName adt://{ sy-sysid }/sap/bc/adt/programs/programs/{ low_name }| )
                                        ( |FAMIX.Method { name }>>{ name } signature { name }| ) ).

  ENDMETHOD.


  METHOD add_web_dynpro_component.

    DATA(low_name) = name.
    TRANSLATE low_name TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } modifiers ABAPWebDynproComponent| )
                                        ( |FAMIX.Class { name } parentPackage { parentpackage }| ) ).

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.
ENDCLASS.
