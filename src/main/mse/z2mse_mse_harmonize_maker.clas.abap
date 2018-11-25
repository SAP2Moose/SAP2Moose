CLASS z2mse_mse_harmonize_maker DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA to_change TYPE z2mse_mse_harmonize=>harmonized_mse.
    METHODS constructor.
    METHODS add_package
      IMPORTING package TYPE string.
    METHODS add_db_table
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    METHODS add_class
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    METHODS add_interface
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    METHODS add_web_dynpro_component
      IMPORTING name          TYPE string
                parentpackage TYPE string.
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
