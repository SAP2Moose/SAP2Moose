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
    "! @parameter name | The class will be available as last_grouping in further calls
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_class
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter name | The function_group will be available as last_grouping with prefix FGR- in further calls
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_function_group
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter function_group | Will be filled from last_grouping when not supplied
    "! @parameter function | The function will be available as last_element with prefix F- in further calls
    METHODS add_function
      IMPORTING function_group TYPE string OPTIONAL
                function       TYPE string OPTIONAL.
    "! @parameter function_group | Will be filled from last_grouping when not supplied
    "! @parameter include | The include will be available as last_element in further calls
    METHODS add_function_group_include
      IMPORTING function_group TYPE string OPTIONAL
                include        TYPE string OPTIONAL.
    "! @parameter class | Will be filled from last_grouping when not supplied
    "! @parameter method | The method will be available as last_element in further calls
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_method
      IMPORTING class   TYPE string OPTIONAL
                method  TYPE string
                at_line TYPE i OPTIONAL.
    "! @parameter class | Will be filled from last_grouping when not supplied
    "! @parameter method | The attribute will be available as last_element in further calls
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_attribute
      IMPORTING class     TYPE string OPTIONAL
                attribute TYPE string
                at_line   TYPE i OPTIONAL.
    "! @parameter interface | Will be filled from last_grouping when not supplied
    "! @parameter method | The method will be available as last_element in further calls
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_interface_method
      IMPORTING interface TYPE string OPTIONAL
                method    TYPE string
                at_line   TYPE i OPTIONAL.
    "! @parameter interface | Will be filled from last_grouping when not supplied
    "! @parameter method | The attribute will be available as last_element in further calls
    "! @parameter at_line | leave empty when link to source is not given
    METHODS add_interface_attribute
      IMPORTING interface TYPE string OPTIONAL
                attribute TYPE string
                at_line   TYPE i OPTIONAL.
    "! @parameter name | The interface will be available as last_grouping in further calls
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_interface
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter name | The interface will be available as last_grouping in further calls
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_web_dynpro_component
      IMPORTING wda_name      TYPE string
                parentpackage TYPE string.
    "! @parameter wda_name | Will be filled from last_grouping when not supplied
    "! @parameter view | The view will be available as last_element in further calls
    METHODS add_web_dynpro_component_view
      IMPORTING wda_name TYPE string OPTIONAL
                view     TYPE string OPTIONAL.
    "! @parameter name | Fills last_grouping and last_element
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_program
      IMPORTING name          TYPE string
                parentpackage TYPE string.
    "! @parameter bw_transformation | Fills last_grouping and last_element
    "! @parameter parentpackage | leave empty when parent package is not written to model
    METHODS add_bw_transformation
      IMPORTING bw_transformation TYPE string
                at_line           TYPE i OPTIONAL.
    "! A coding uses another coding.
    "! @parameter using_group | Class in case of classes. Do not provide when there is no grouping (Programs, Transformations,...). Is filled with last_grouping if using is not supplied (In case of function groups FGR- is needed as prefix).
    "! @parameter using | The using element. Is filled with last_element if not supplied (In case of functions F- is needed as prefix).
    "! @parameter used_group | Class in case of classes. Do not provide when there is no grouping (Programs, Transformations,...) (In case of function groups FGR- is needed as prefix).
    "! @parameter used | The using element (In case of functions F- is needed as prefix).
    METHODS usage
      IMPORTING using_group TYPE string OPTIONAL
                using       TYPE string OPTIONAL
                used_group  TYPE string DEFAULT ''
                used        TYPE string.
    "! A method or coding accesses an attribute or data.
    "! @parameter using_group | Class in case of classes. Do not provide when there is no grouping (Programs, Transformations,...). Is filled with last_grouping if using is not supplied.
    "! @parameter using | The using element. Is filled with last_element if not supplied.
    "! @parameter used_group | Class in case of classes. Do not provide when there is no grouping (Programs, Transformations,...). In case of function groups FGR- is needed as prefix.
    "! @parameter used | The using element. In case of functions F- is needed as prefix.
    METHODS access
      IMPORTING using_group TYPE string OPTIONAL
                using       TYPE string OPTIONAL
                used_group  TYPE string DEFAULT ''
                used        TYPE string.
    METHODS add_custom_source_language
      IMPORTING language TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA last_grouping TYPE string.
    DATA last_element TYPE string.
ENDCLASS.



CLASS z2mse_mse_harmonize_maker IMPLEMENTATION.


  METHOD access.

    IF using IS NOT SUPPLIED.
      DATA(ung) = last_grouping.
      DATA(my_using) = last_element.
    ELSE.
      ung = using_group.
      my_using = using.
    ENDIF.

    IF ung IS INITIAL.
      ung = my_using.
    ELSE.
      ung = ung.
    ENDIF.

    IF used_group IS INITIAL.
      DATA(udg) = used.
    ELSE.
      udg = used_group.
    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Access accessor { ung }>>{ my_using } variable { udg }>>{ used }| ) ).

  ENDMETHOD.


  METHOD add_attribute.
    IF class IS SUPPLIED.
      DATA(my_class) = class.
    ELSE.
      my_class = last_grouping.
    ENDIF.

    DATA(low_class) = my_class.
    TRANSLATE low_class TO LOWER CASE.

    IF at_line IS NOT SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Attribute { my_class }>>{ attribute }| ) ).

    ELSE.

      to_change = VALUE #( BASE to_change ( |FAMIX.Attribute { my_class }>>{ attribute } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { attribute } fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/{ low_class }/source/main#start={ at_line },1| ) ).

    ENDIF.

    last_element = attribute.

  ENDMETHOD.


  METHOD add_bw_transformation.

    DATA(low_name) = bw_transformation.
    TRANSLATE low_name TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { bw_transformation } modifiers BWTransformation| )
                                        ( |FAMIX.Method { bw_transformation }>>{ bw_transformation } signature { bw_transformation }| ) ).

    IF at_line IS SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Method { bw_transformation }>>{ bw_transformation } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { bw_transformation } fileName bwmt://{ sy-sysid }/sap/bw/modeling/trfn/{ at_line }| ) ).

    ENDIF.

    last_grouping = bw_transformation.
    last_element = bw_transformation.

  ENDMETHOD.


  METHOD add_class.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } parentPackage { parentpackage }| ) ).

    ENDIF.

    DATA(low_name) = name.
    TRANSLATE low_name TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name } modifiers ABAPGlobalClass| )
                                        ( |FAMIX.FileAnchor { name } fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/{ low_name }/source/main| )
                                        ( |FAMIX.Class { name } sourceAnchor| )  ).

    last_grouping = name.

  ENDMETHOD.


  METHOD add_db_table.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { name  } parentPackage {  parentpackage  }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { name  } modifiers DBTable| )
                                        ( |FAMIX.Attribute { name  }>>{ name  }| )  ).

  ENDMETHOD.


  METHOD add_function.

    IF function_group IS SUPPLIED.
      DATA(my_function_group) = |FGR-{ function_group }|.
    ELSE.
      my_function_group = last_grouping.
    ENDIF.

    DATA(low_function_group) = my_function_group.
    SHIFT low_function_group BY 4 PLACES LEFT.
    TRANSLATE low_function_group TO LOWER CASE.

    DATA(low_function) = function.
    TRANSLATE low_function TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_function_group }>>F-{ function } signature F-{ function }| )
                                        ( |FAMIX.Method { my_function_group }>>F-{ function } sourceAnchor| )
                                        ( |FAMIX.FileAnchor F-{ function } fileName adt://{ sy-sysid }/sap/bc/adt/functions/groups/{ low_function_group }/fmodules/{ low_function }| ) ).


    last_element = |F-{ function }|.

  ENDMETHOD.


  METHOD add_function_group.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class FGR-{ name } parentPackage { parentpackage }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class FGR-{ name } modifiers ABAPFunktionGroup| ) ).

    last_grouping = |FGR-{ name }|.

  ENDMETHOD.


  METHOD add_function_group_include.

    IF function_group IS SUPPLIED.
      DATA(my_function_group) = |FGR-{ function_group }|.
    ELSE.
      my_function_group = last_grouping.
    ENDIF.

    DATA(low_function_group) = my_function_group.
    SHIFT low_function_group BY 4 PLACES LEFT.
    TRANSLATE low_function_group TO LOWER CASE.

    DATA(low_include) = include.
    TRANSLATE low_include TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_function_group }>>{ include } signature { include }| )
                                        ( |FAMIX.Method { my_function_group }>>{ include } sourceAnchor| )
                                        ( |FAMIX.FileAnchor { include } fileName adt://{ sy-sysid }/sap/bc/adt/functions/groups/{ low_function_group }/includes/{ low_include }| ) ).


    last_element = include.

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

    last_grouping = name.

  ENDMETHOD.


  METHOD add_interface_attribute.
    IF interface IS SUPPLIED.
      DATA(my_interface) = interface.
    ELSE.
      my_interface = last_grouping.
    ENDIF.

    DATA(low_interface) = my_interface.
    TRANSLATE low_interface TO LOWER CASE.

    IF at_line IS NOT SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Attribute { my_interface }>>{ attribute }| ) ).

    ELSE.

      to_change = VALUE #( BASE to_change ( |FAMIX.Attribute { my_interface }>>{ attribute } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { attribute } fileName adt://{ sy-sysid }/sap/bc/adt/oo/interfaces/{ low_interface }/source/main#start=5,1| ) ).

    ENDIF.

    last_element = attribute.

  ENDMETHOD.


  METHOD add_interface_method.
    IF interface IS SUPPLIED.
      DATA(my_interface) = interface.
    ELSE.
      my_interface = last_grouping.
    ENDIF.

    DATA(low_interface) = my_interface.
    TRANSLATE low_interface TO LOWER CASE.

    to_change = VALUE #( BASE to_change  ( |FAMIX.Method { my_interface }>>{ method } signature { method }| ) ).

    IF at_line IS SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_interface }>>{ method } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { method } fileName adt://{ sy-sysid }/sap/bc/adt/oo/interfaces/{ low_interface }/source/main#start={ at_line },1| ) ).

    ENDIF.

    last_element = method.

  ENDMETHOD.


  METHOD add_method.
    IF class IS SUPPLIED.
      DATA(my_class) = class.
    ELSE.
      my_class = last_grouping.
    ENDIF.

    DATA(low_class) = my_class.
    TRANSLATE low_class TO LOWER CASE.

    to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_class }>>{ method } signature { method }| ) ).

    IF at_line IS SUPPLIED.

      to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_class }>>{ method } sourceAnchor| )
                                          ( |FAMIX.FileAnchor { method } fileName adt://{ sy-sysid }/sap/bc/adt/oo/classes/{ low_class }/source/main#start={ at_line },1| ) ).

    ENDIF.

    last_element = method.

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

    last_grouping = name.
    last_element = name.

  ENDMETHOD.


  METHOD add_web_dynpro_component.

    DATA(low_name) = wda_name.
    TRANSLATE low_name TO LOWER CASE.

    IF parentpackage IS NOT INITIAL.

      to_change = VALUE #( BASE to_change ( |FAMIX.Class { wda_name } parentPackage { parentpackage }| ) ).

    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Class { wda_name } modifiers ABAPWebDynproComponent| ) ).

    last_grouping = wda_name.

  ENDMETHOD.


  METHOD add_web_dynpro_component_view.

    IF wda_name IS SUPPLIED.
      DATA(my_wda_name) = wda_name.
    ELSE.
      my_wda_name = last_grouping.
    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Method { my_wda_name }>>{ view } signature { view }| ) ).

    last_element = view.

  ENDMETHOD.


  METHOD constructor.
  ENDMETHOD.


  METHOD usage.

    IF using IS NOT SUPPLIED.
      DATA(ung) = last_grouping.
      DATA(my_using) = last_element.
    ELSE.
      ung = using_group.
      my_using = using.
    ENDIF.

    IF ung IS INITIAL.
      ung = my_using.
    ELSE.
      ung = ung.
    ENDIF.

    IF used_group IS INITIAL.
      DATA(udg) = used.
    ELSE.
      udg = used_group.
    ENDIF.

    to_change = VALUE #( BASE to_change ( |FAMIX.Invocation sender { ung }>>{ my_using } candidates { udg }>>{ used } signature DUMMY| ) ).

  ENDMETHOD.
  METHOD add_custom_source_language.
    to_change = VALUE #( BASE to_change ( |FAMIX.CustomSourceLanguage { language }| ) ).
  ENDMETHOD.

ENDCLASS.
