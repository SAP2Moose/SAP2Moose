*The MIT License (MIT)
*
*Copyright (c) 2016 Rainer Winkler, CubeServ
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.

"! This is an experimental prototype, that may have errors
"! The program follows the naming conventions proposed in the ABAP Programming Guidelines from 2016.
"! Prefixes are omitted if the reading is simplified
"! Short abbreviations are used if only locally used, in that case an ABAP Doc comment explains the variable
REPORT yrw1_moose_extractor.

PARAMETERS: p_sap AS CHECKBOX DEFAULT 'X'.
"! Extract from SAP
DATA parameter_extract_from_sap TYPE abap_bool.
parameter_extract_from_sap = p_sap.

PARAMETERS: p_pack TYPE parentcl DEFAULT 'YRW1'.
"! Package to be analyzed
DATA parameter_package_to_analyze TYPE parentcl.
parameter_package_to_analyze = p_pack.

PARAMETERS: p_list AS CHECKBOX DEFAULT 'X'.
"! List Tokens of selected programs
DATA parameter_list_tokens TYPE abap_bool.
parameter_list_tokens = p_list.

PARAMETERS: p_dm AS CHECKBOX DEFAULT 'X'.
"! Usages outside package grouped
DATA parameter_usage_outpack_groupd TYPE abap_bool.

include z_mse.

include z_famix.

TYPES: BEGIN OF ty_class_component,
         clsname TYPE seocompo-clsname,
         cmpname TYPE seocompo-cmpname,
         cmptype TYPE seocompo-cmptype,
       END OF ty_class_component.

TYPES: BEGIN OF ty_tadir_object,
          obj_name TYPE sobj_name,
          object   TYPE trobjtype,
          devclass TYPE devclass,
       END OF ty_tadir_object.

TYPES: BEGIN OF ty_classes_interfaces,
        obj_name TYPE seoclsname,
      END OF ty_classes_interfaces.

TYPES: BEGIN OF ty_program,
         program TYPE string,
       END OF ty_program.

TYPES:BEGIN OF ty_class,
        class TYPE seoclsname,
      END OF ty_class.

TYPES: BEGIN OF ty_inheritances,
         clsname    TYPE seometarel-clsname,
         refclsname TYPE seometarel-refclsname,
         reltype    TYPE seometarel-reltype,
       END OF ty_inheritances.

CLASS cl_extract_sap DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS extract
      EXPORTING
        et_model TYPE cl_model=>ty_lines.
  PRIVATE SECTION.

    CONSTANTS c_cmptype_attribute TYPE seocmptype VALUE '0'.
    CONSTANTS c_cmptype_method TYPE seocmptype VALUE '1'.

    CLASS-METHODS _determine_usages
      IMPORTING
        i_famix_class      TYPE REF TO cl_famix_class
        is_class_component TYPE ty_class_component
        i_famix_method     TYPE REF TO cl_famix_method
        i_famix_invocation TYPE REF TO cl_famix_invocation
        i_famix_access     TYPE REF TO cl_famix_access
        iv_used_id         TYPE i.

    CLASS-METHODS _set_default_language
      IMPORTING
        i_model TYPE REF TO cl_model.

    TYPES: BEGIN OF ty_devclass,
          devclass TYPE DEVCLASS,
          END OF ty_devclass.
    TYPES:
      ty_processed_dev_classes TYPE HASHED TABLE OF ty_devclass WITH UNIQUE KEY devclass.
    CLASS-METHODS _determine_packages_to_analyze
      IMPORTING
        i_model                        TYPE REF TO cl_model
        is_devclass_first              TYPE tdevc
      RETURNING
        VALUE(r_processed_dev_classes) TYPE ty_processed_dev_classes.
    TYPES:
      ty_lt_tadir_objects TYPE HASHED TABLE OF ty_tadir_object WITH UNIQUE KEY obj_name,
      ty_lt_classes_2     TYPE STANDARD TABLE OF ty_classes_interfaces WITH DEFAULT KEY,
      ty_lt_programs      TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY.
    CLASS-METHODS _objects_to_analyze_by_tadir
      IMPORTING
        it_tadir_objects TYPE ty_lt_tadir_objects
      EXPORTING
        et_classes_2     TYPE ty_lt_classes_2
        et_programs      TYPE ty_lt_programs.
    TYPES:
      ty_lt_tadir_objects_1 TYPE HASHED TABLE OF ty_tadir_object WITH UNIQUE KEY obj_name,
      ty_lt_programs_1      TYPE STANDARD TABLE OF ty_program WITH DEFAULT KEY.
    CLASS-METHODS _read_all_programs
      IMPORTING
        it_tadir_objects TYPE ty_lt_tadir_objects_1
        it_programs      TYPE ty_lt_programs_1
      CHANGING
        c_model          TYPE REF TO cl_model.
    TYPES:
      ty_lt_tadir_objects_2  TYPE HASHED TABLE OF ty_tadir_object WITH UNIQUE KEY obj_name,
      ty_lt_existing_classes TYPE HASHED TABLE OF ty_class WITH UNIQUE KEY class.
    CLASS-METHODS _add_to_model
      IMPORTING
        i_model              TYPE REF TO cl_model
        it_tadir_objects     TYPE ty_lt_tadir_objects_2
        it_existing_classes  TYPE ty_lt_existing_classes
      RETURNING
        VALUE(r_famix_class) TYPE REF TO cl_famix_class.

ENDCLASS.

TYPES: BEGIN OF ty_indexed_token,
         index TYPE i,
         str   TYPE string,
         row   TYPE token_row,
         col   TYPE token_col,
         type  TYPE token_type,
       END OF ty_indexed_token.

TYPES ty_sorted_tokens TYPE SORTED TABLE OF ty_indexed_token WITH UNIQUE KEY index.

"! Analyze ABAP Statement of type K (Other ABAP key word)
CLASS cl_ep_analyze_other_keyword DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_sorted_tokens TYPE ty_sorted_tokens.
    METHODS analyze
      IMPORTING
        i_statement TYPE sstmnt.
    TYPES ty_statement_type TYPE c LENGTH 2.
    CONSTANTS:

      start_class_definition      TYPE ty_statement_type VALUE 'CD',
      start_class_implementation  TYPE ty_statement_type VALUE 'CI',
      end_class                   TYPE ty_statement_type VALUE 'CE',
      method_definition           TYPE ty_statement_type VALUE 'MD',
      start_method_implementation TYPE ty_statement_type VALUE 'MI',
      end_method_implementation   TYPE ty_statement_type VALUE 'ME',
      attribute_definition        TYPE ty_statement_type VALUE 'AD',
      start_public                TYPE ty_statement_type VALUE 'PU',
      start_protected             TYPE ty_statement_type VALUE 'PO',
      start_private               TYPE ty_statement_type VALUE 'PR'.


    TYPES: BEGIN OF ty_info,
             statement_type      TYPE ty_statement_type,
             is_class_stmnt_info TYPE bool,
             class_is_inheriting TYPE bool,
             class_inherits_from TYPE string,
             is_static           TYPE bool,
             name                TYPE string,
           END OF ty_info.
    DATA: info TYPE ty_info READ-ONLY.
  PRIVATE SECTION.
    DATA sorted_tokens TYPE ty_sorted_tokens.
ENDCLASS.

CLASS cl_ep_analyze_other_keyword IMPLEMENTATION.

  METHOD constructor.
    sorted_tokens = it_sorted_tokens.
  ENDMETHOD.

  METHOD analyze.
    ASSERT i_statement-type EQ 'K'.
    info = VALUE #( ).

    " First Run, what is the keyword
    READ TABLE sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_token>) WITH TABLE KEY index = i_statement-from.
    IF sy-subrc <> ok.
      " TBD Error handling
      " In the moment ignore
      RETURN.
    ENDIF.

    CASE <ls_token>-str.
      WHEN 'CLASS'.
        info-is_class_stmnt_info = true.

      WHEN 'ENDCLASS'.
        info-statement_type = end_class.
      WHEN 'PUBLIC'.
        info-statement_type = start_public.
      WHEN 'PROTECTED'.
        info-statement_type = start_protected.
      WHEN 'PRIVATE'.
        info-statement_type = start_private.
      WHEN 'METHODS'.
        " info-is_method_stmnt = true.
        info-statement_type = method_definition.
      WHEN 'CLASS-METHODS'.
        info-statement_type = method_definition.
        info-is_static = true.
      WHEN 'METHOD'.
        info-statement_type = start_method_implementation.
      WHEN 'ENDMETHOD'.
        info-statement_type = end_method_implementation.

      WHEN 'DATA'.
        info-statement_type = attribute_definition.
      WHEN 'CLASS-DATA'.
        info-statement_type = attribute_definition.
        info-is_static = true.
      WHEN OTHERS.
        " TBD
        " Add further, in the moment ignore
        RETURN.
    ENDCASE.

    " Second Run, what is the name
    IF info-is_class_stmnt_info EQ true
    OR info-statement_type EQ method_definition
    OR info-statement_type EQ start_method_implementation
    OR info-statement_type EQ attribute_definition.

      DATA(lv_position_of_name) = i_statement-from + 1.
      READ TABLE sorted_tokens ASSIGNING <ls_token> WITH TABLE KEY index = lv_position_of_name.
      IF sy-subrc <> ok.
        " TBD Error handling
        " In the moment ignore
        RETURN.
      ENDIF.

      info-name = <ls_token>-str.

      " Third run, further keywords
      IF info-is_class_stmnt_info EQ true.
        LOOP AT sorted_tokens ASSIGNING <ls_token> WHERE index > lv_position_of_name
                                                                      AND index <= i_statement-to.
          CASE <ls_token>-str.
            WHEN 'DEFINITION'.
              info-statement_type = start_class_definition.
            WHEN 'IMPLEMENTATION'.
              info-statement_type = start_class_implementation.
            WHEN 'INHERITING'.
              info-class_is_inheriting = true.
              DATA(superclass_is_at) = sy-tabix + 2.
              READ TABLE sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_superclass_token>) WITH TABLE KEY index = superclass_is_at.
              IF sy-subrc EQ ok.
                info-class_inherits_from = <ls_superclass_token>-str.
              ELSE.
                " TBD Error handling
                " In the moment ignore
                RETURN.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS cl_extract_program DEFINITION.
  PUBLIC SECTION.
    METHODS extract
      IMPORTING
        i_module_reference TYPE i
        i_program          TYPE progname
      CHANGING
        cr_model           TYPE REF TO cl_model.

ENDCLASS.

CLASS cl_extract_program IMPLEMENTATION.

  METHOD extract.
    DATA source TYPE TABLE OF string.
    READ REPORT i_program INTO source.

    DATA: lt_tokens TYPE STANDARD TABLE OF stokes.



    DATA: lt_sorted_tokens TYPE ty_sorted_tokens.

    DATA lt_statements TYPE STANDARD TABLE OF sstmnt.

    SCAN ABAP-SOURCE source TOKENS INTO lt_tokens STATEMENTS INTO lt_statements.

    LOOP AT lt_tokens ASSIGNING FIELD-SYMBOL(<ls_token_2>).
      lt_sorted_tokens = VALUE #( BASE lt_sorted_tokens ( index = sy-tabix
                                                          str = <ls_token_2>-str
                                                          row = <ls_token_2>-row
                                                          col = <ls_token_2>-col
                                                          type = <ls_token_2>-type ) ).
    ENDLOOP.


    SORT lt_statements BY from.


    IF parameter_list_tokens EQ true.
      WRITE: /.
      WRITE: / i_program.


*      LOOP AT lt_statements ASSIGNING FIELD-SYMBOL(<ls_statement_2>).
*        WRITE: / <ls_statement_2>-type.
*        LOOP AT lt_sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_token_3>) WHERE
*            index >= <ls_statement_2>-from
*        AND index <= <ls_statement_2>-to.
*          WRITE: '|', <ls_token_3>-type, <ls_token_3>-str.
*        ENDLOOP.
*      ENDLOOP.

    ENDIF.

    TYPES: ty_statement_type TYPE c LENGTH 1.
    TYPES ty_section_type TYPE c LENGTH 1.

    CONSTANTS: "! Is in public section
      public    TYPE ty_section_type VALUE '1',
      "! Is in protected section
      protected TYPE ty_section_type VALUE '2',
      "! Is in private section
      private   TYPE ty_section_type VALUE '3',
      "! Not in a section
      none      TYPE ty_section_type VALUE ' '.

    TYPES: BEGIN OF ty_codecontext,
             in_section               TYPE ty_section_type,
             in_class_definition      TYPE bool,
             implementation_of_class  TYPE string,
             implementation_of_method TYPE string,
           END OF ty_codecontext.

    "! Context of statement in the code
    DATA context TYPE ty_codecontext.

    TYPES: BEGIN OF ty_class,
             classname   TYPE string,
             id_in_model TYPE i,
           END OF ty_class.

    DATA: lt_classes      TYPE HASHED TABLE OF ty_class WITH UNIQUE KEY classname,
          ls_actual_class TYPE ty_class.

    TYPES: BEGIN OF ty_method,
             classname          TYPE string,
             class_id_in_model  TYPE i,
             methodname         TYPE string,
             method_id_in_model TYPE i,
             in_section         TYPE ty_section_type,
             instanciable       TYPE bool,
           END OF ty_method.

    DATA: lt_methods       TYPE STANDARD TABLE OF ty_method,
          ls_actual_method TYPE ty_method.

    TYPES: BEGIN OF ty_inheritance,
             subclass   TYPE string,
             superclass TYPE string,
           END OF ty_inheritance.

    DATA: lt_inheritances TYPE STANDARD TABLE OF ty_inheritance.

    DATA lv_token_number TYPE i.

    "! Instance that analyzes other ABAP Keywords
    DATA aok TYPE REF TO cl_ep_analyze_other_keyword.
    aok = NEW cl_ep_analyze_other_keyword( it_sorted_tokens = lt_sorted_tokens ).

    LOOP AT lt_statements ASSIGNING FIELD-SYMBOL(<ls_statement>).



      lv_token_number = 0.
      CASE <ls_statement>-type.
        WHEN 'K'.

          aok->analyze( i_statement = <ls_statement> ).
          CASE aok->info-statement_type.
            WHEN aok->start_class_definition.
              " SAP_2_FAMIX_28        Determine local classes in programs
              context-in_class_definition = true.
              ls_actual_class-classname = aok->info-name.
              lt_classes = VALUE #( BASE lt_classes ( ls_actual_class ) ).
              IF aok->info-class_is_inheriting EQ true.
                " SAP_2_FAMIX_37        Determine local inheritances of classes
                lt_inheritances = VALUE #( BASE lt_inheritances ( subclass = ls_actual_class-classname
                                                                  superclass = aok->info-class_inherits_from ) ).
              ENDIF.
            WHEN aok->start_public.
              context-in_section = public.
            WHEN aok->start_protected.
              context-in_section = protected.
            WHEN aok->start_private.
              context-in_section = private.
            WHEN aok->end_class.
              context-in_section = none.
              context-in_class_definition = false.
              context-implementation_of_class = VALUE #( ).
            WHEN aok->method_definition.
              " SAP_2_FAMIX_29      Determine local class methods in programs
              IF aok->info-is_static EQ true.
                ls_actual_method = VALUE #( classname = ls_actual_class-classname
                                            in_section = context-in_section ).
              ELSE.
                ls_actual_method = VALUE #( classname = ls_actual_class-classname
                                            in_section = context-in_section
                                            instanciable = true ).
              ENDIF.
              ls_actual_method-methodname = aok->info-name.
            WHEN aok->start_class_implementation.
              context-implementation_of_class = aok->info-name.
            WHEN aok->start_method_implementation.
              context-implementation_of_method = aok->info-name.
              IF parameter_list_tokens EQ abap_true.
                FORMAT COLOR COL_GROUP.
              ENDIF.
            WHEN aok->end_method_implementation.
              context-implementation_of_method = VALUE #( ).
              IF parameter_list_tokens EQ abap_true.
                FORMAT COLOR COL_BACKGROUND.
              ENDIF.
            WHEN OTHERS.

          ENDCASE.

        WHEN OTHERS.

      ENDCASE.

      IF parameter_list_tokens EQ abap_true.
        WRITE: / <ls_statement>-type.
        LOOP AT lt_sorted_tokens ASSIGNING FIELD-SYMBOL(<ls_token_3>) WHERE
            index >= <ls_statement>-from
        AND index <= <ls_statement>-to.
          WRITE: '|', <ls_token_3>-type, <ls_token_3>-str.
        ENDLOOP.
      ENDIF.

    ENDLOOP.


    " Add local classes to model

    DATA(famix_class) = NEW cl_famix_class( cr_model ).

    LOOP AT lt_classes ASSIGNING FIELD-SYMBOL(<ls_class>).
      " SAP_2_FAMIX_30        Map local classes of programs to FAMIX.Class

      <ls_class>-id_in_model = famix_class->add( EXPORTING i_name_group = CONV string( i_program )
                                                           i_name                   = <ls_class>-classname ).

      " SAP_2_FAMIX_31     Assign local classes to a container of type FAMIX.Module with the name of the program

      famix_class->set_container( EXPORTING i_container_element = 'FAMIX.Module'
                                            i_parent_container  = CONV string( i_program ) ).


    ENDLOOP.

    " Add local methods to model

    DATA(famix_method) = NEW cl_famix_method( cr_model ).

    LOOP AT lt_methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      READ TABLE lt_classes ASSIGNING FIELD-SYMBOL(<ls_classes_2>) WITH TABLE KEY classname = <ls_method>-classname.
      <ls_method>-class_id_in_model = <ls_classes_2>-id_in_model.

      " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method

      <ls_method>-method_id_in_model = famix_method->add( EXPORTING i_name_group = <ls_classes_2>-classname
                                                                    i_name       = <ls_method>-methodname ).
      " SAP_2_FAMIX_43        Fill the attribut signature of FAMIX.METHOD with the name of the method
      famix_method->set_signature( i_signature = <ls_method>-methodname ).

      " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class


      famix_method->set_parent_type( EXPORTING i_parent_element = 'FAMIX.Class'
                                               i_parent_id      =  <ls_classes_2>-id_in_model ).
    ENDLOOP.

    " Add local inheritances to model

    DATA(famix_inheritance) = NEW cl_famix_inheritance( cr_model ).
    LOOP AT lt_inheritances INTO DATA(ls_inheritance).
      " SAP_2_FAMIX_38        Map local inheritances of classes to FAMIX.Inheritance
      famix_inheritance->add( ).
      famix_inheritance->set_sub_and_super_class( EXPORTING i_subclass_element      = 'FAMIX.Class'
                                                            i_subclass_name_group   = CONV #( i_program )
                                                            i_subclass_name         = ls_inheritance-subclass
                                                            i_superclass_element    = 'FAMIX.Class'
                                                            i_superclass_name_group = CONV #( i_program )
                                                            i_superclass_name       = ls_inheritance-superclass ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS cl_extract_sap IMPLEMENTATION.

  METHOD extract.

    TYPES:BEGIN OF ty_devclass,
            devclass TYPE devclass,
          END OF ty_devclass.

    DATA(model) = NEW cl_model( ).

    _set_default_language( model ).

    " SAP_2_FAMIX_3     Select all Objects in a package and the sub packages of this package

    DATA ls_devclass_first type tdevc.

    SELECT SINGLE devclass, parentcl FROM tdevc INTO @ls_devclass_first WHERE devclass = @parameter_package_to_analyze.
    IF sy-subrc <> ok.
      WRITE: 'Package does not exist: ', parameter_package_to_analyze.
      RETURN.
    ENDIF.

    DATA processed_dev_classes TYPE ty_processed_dev_classes.

    processed_dev_classes = _determine_packages_to_analyze( i_model           = model
                                                            is_devclass_first = ls_devclass_first ).

    DATA: lt_tadir_objects TYPE HASHED TABLE OF ty_tadir_object WITH UNIQUE KEY obj_name.
    DATA: lt_classes_2 TYPE STANDARD TABLE OF ty_classes_interfaces.
    DATA: lt_programs TYPE STANDARD TABLE OF ty_program.

    IF processed_dev_classes IS NOT INITIAL.
      SELECT obj_name, object, devclass FROM tadir INTO TABLE @lt_tadir_objects FOR ALL ENTRIES IN @processed_dev_classes
        WHERE
          pgmid = 'R3TR'
          AND ( object = 'CLAS' OR object = 'INTF' OR object = 'PROG' )
          AND devclass = @processed_dev_classes-devclass.
    ENDIF.

   _objects_to_analyze_by_tadir( EXPORTING it_tadir_objects = lt_tadir_objects
                                 IMPORTING et_classes_2 = lt_classes_2
                                           et_programs  = lt_programs ).

   _read_all_programs( EXPORTING it_tadir_objects = lt_tadir_objects
                                 it_programs      = lt_programs
                        CHANGING c_model = model ).

    " Read all classes

    DATA: lt_existing_classes TYPE HASHED TABLE OF ty_class WITH UNIQUE KEY class.

    " Read from SEOCLASS
    IF lt_classes_2 IS NOT INITIAL.
      SELECT clsname AS class FROM seoclass INTO TABLE @lt_existing_classes FOR ALL ENTRIES IN @lt_classes_2
        WHERE
          clsname = @lt_classes_2-obj_name.
    ENDIF.

    DATA famix_class TYPE REF TO cl_famix_class.


    famix_class = _add_to_model( i_model             = model
                                 it_tadir_objects    = lt_tadir_objects
                                 it_existing_classes = lt_existing_classes ).

    " Determine inheritances between selected classes

    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).

    DATA: lt_inheritances TYPE STANDARD TABLE OF  ty_inheritances.

    IF lt_existing_classes IS NOT INITIAL.
      SELECT clsname, refclsname, reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE @lt_inheritances
        FOR ALL ENTRIES IN @lt_existing_classes WHERE clsname = @lt_existing_classes-class
                                                 AND version = 1.
    ENDIF.

    " Delete all inheritances where superclass is not in selected packages
    LOOP AT lt_inheritances INTO DATA(ls_inheritance_2).
      READ TABLE lt_existing_classes TRANSPORTING NO FIELDS WITH TABLE KEY class = ls_inheritance_2-refclsname.
      IF sy-subrc <> ok.
        DELETE lt_inheritances.
      ENDIF.
    ENDLOOP.

    " Add inheritances to model
    LOOP AT lt_inheritances INTO DATA(ls_inheritance).
      CASE ls_inheritance-reltype.
        WHEN 2.
          " Inheritance
          " SAP_2_FAMIX_39     Map all inheritances between classes in selected packages to FAMIX.Inheritance
          famix_inheritance->add( ).
          famix_inheritance->set_sub_and_super_class( EXPORTING i_subclass_element      = 'FAMIX.Class'
                                                                i_subclass_name_group   = ''
                                                                i_subclass_name         = CONV #( ls_inheritance-clsname )
                                                                i_superclass_element    = 'FAMIX.Class'
                                                                i_superclass_name_group = ''
                                                                i_superclass_name       = CONV #( ls_inheritance-refclsname ) ).
        WHEN 1.
          " Interface implementation
          " SAP_2_FAMIX_40        Map all interface implementations of interfaces in selected packages by classes of selected packages by FAMIX.Inheritance

          famix_inheritance->add( ).
          famix_inheritance->set_sub_and_super_class( EXPORTING i_subclass_element      = 'FAMIX.Class'
                                                                i_subclass_name_group   = ''
                                                                i_subclass_name         = CONV #( ls_inheritance-clsname )
                                                                i_superclass_element    = 'FAMIX.Class'
                                                                i_superclass_name_group = ''
                                                                i_superclass_name       = CONV #( ls_inheritance-refclsname ) ).

        WHEN 0.
          " Interface composition     (i COMPRISING i_ref)
          " TBD
        WHEN 5.
          " Enhancement            ( c enhances c_ref)
          " TBD
      ENDCASE.
    ENDLOOP.

    " Determine all methods

    DATA: ls_class_component  TYPE ty_class_component,
          lt_class_components TYPE HASHED TABLE OF ty_class_component WITH UNIQUE KEY clsname cmpname.

    " SAP_2_FAMIX_9     Extract methods of classes
    " SAP_2_FAMIX_10        Extract methods of interfaces
    " SAP_2_FAMIX_11        Extract attributes of classes
    " SAP_2_FAMIX_12        Extract attributes of interfaces

    IF lt_existing_classes IS NOT INITIAL.
      "
      SELECT clsname, cmpname, cmptype FROM seocompo INTO TABLE @lt_class_components
        FOR ALL ENTRIES IN @lt_existing_classes
        WHERE
          clsname = @lt_existing_classes-class.

    ENDIF.

    " Add to model

    DATA(famix_method) = NEW cl_famix_method( model ).
    DATA(famix_attribute) = NEW cl_famix_attribute( model ).

    LOOP AT lt_class_components INTO ls_class_component.

      CASE ls_class_component-cmptype.
        WHEN c_cmptype_attribute. "Attribute

          " SAP_2_FAMIX_13        Mapp attributes of classes to FAMIX.Attribute
          " SAP_2_FAMIX_14        Mapp attributes of interfaces to FAMIX.Attribute

          famix_attribute->add( i_name = CONV string( ls_class_component-cmpname ) ).
          famix_attribute->set_parent_type(
            EXPORTING
              i_parent_element = 'FAMIX.Class'
              i_parent_name    = CONV string( ls_class_component-clsname ) ).
          famix_attribute->store_id( EXPORTING i_class     = CONV string( ls_class_component-clsname )
                                               i_attribute = CONV string( ls_class_component-cmpname ) ).

        WHEN c_cmptype_method. "Method

          " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
          " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method

          famix_method->add( i_name = CONV string( ls_class_component-cmpname ) ).
          " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
          " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
          famix_method->set_signature( i_signature = CONV string( ls_class_component-cmpname ) ).
          famix_method->set_parent_type(
            EXPORTING
              i_parent_element = 'FAMIX.Class'
              i_parent_name    = CONV string( ls_class_component-clsname ) ).
          famix_method->store_id( EXPORTING i_class  = CONV string( ls_class_component-clsname )
                                            i_method = CONV string( ls_class_component-cmpname ) ).
        WHEN 2. "Event
        WHEN 3. "Type
        WHEN OTHERS.
          " TBD Warn

      ENDCASE.

    ENDLOOP.

    " Determine usage of methods

    DATA(famix_invocation) = NEW cl_famix_invocation( model ).
    DATA(famix_access) = NEW cl_famix_access( model ).

    LOOP AT lt_class_components INTO ls_class_component WHERE cmptype = c_cmptype_attribute  " Methods
                                                           OR cmptype = c_cmptype_method. "Attributes

      CASE ls_class_component-cmptype.
        WHEN c_cmptype_method.
          DATA(lv_used_id) = famix_method->get_id( i_class  = CONV string( ls_class_component-clsname )
                                                   i_method = CONV string( ls_class_component-cmpname ) ).

        WHEN c_cmptype_attribute.
          lv_used_id = famix_attribute->get_id( i_class     = CONV string( ls_class_component-clsname )
                                                i_attribute = CONV string( ls_class_component-cmpname ) ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      _determine_usages( i_famix_class      = famix_class
                         is_class_component = ls_class_component
                         i_famix_method     = famix_method
                         i_famix_invocation = famix_invocation
                         i_famix_access     = famix_access
                         iv_used_id         = lv_used_id ).

    ENDLOOP.

    model->make_mse( IMPORTING et_mse = et_model ).

  ENDMETHOD.


  METHOD _determine_usages.

    DATA lv_where_used_name TYPE eu_lname.
    CASE is_class_component-cmptype.
      WHEN c_cmptype_method.

        " SAP_2_FAMIX_17      Determine usage of class methods by programs and classes
        " SAP_2_FAMIX_18      Determine usage of interface methods by programs and classes

        lv_where_used_name = is_class_component-clsname && |\\ME:| && is_class_component-cmpname.
        SELECT * FROM wbcrossgt INTO TABLE @DATA(lt_where_used) WHERE otype = 'ME' AND name = @lv_where_used_name.
      WHEN c_cmptype_attribute.

        " SAP_2_FAMIX_19      Determine usage of class attributes by programs and classes
        " SAP_2_FAMIX_20      Determine usage of interface attributes by programs and classes

        lv_where_used_name = is_class_component-clsname && |\\DA:| && is_class_component-cmpname.
        SELECT * FROM wbcrossgt INTO TABLE @lt_where_used WHERE otype = 'DA' AND name = @lv_where_used_name.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    LOOP AT lt_where_used ASSIGNING FIELD-SYMBOL(<ls_where_used>).
      SELECT SINGLE * FROM ris_prog_tadir INTO @DATA(ls_prog_tadir) WHERE program_name = @<ls_where_used>-include.
      IF sy-subrc EQ ok.
        CASE ls_prog_tadir-object_type.
          WHEN 'CLAS'.
            " Used by method
            DATA: lv_using_method TYPE string.
            IF ls_prog_tadir-method_name IS INITIAL.
              lv_using_method = 'DUMMY'.
            ELSE.
              lv_using_method = CONV string( ls_prog_tadir-method_name ).
            ENDIF.


            DATA(lv_using_method_id) = i_famix_method->get_id( i_class  = CONV string( ls_prog_tadir-object_name )
                                                               i_method = lv_using_method ).
            IF lv_using_method_id EQ 0.

              IF parameter_usage_outpack_groupd EQ false.

                " Method does not exist, create the class
                " SAP_2_FAMIX_21      If an object is used by a class that is not selected, add this class to the model
                " SAP_2_FAMIX_22      Do not assign classes that included due to usage to a package

                i_famix_class->add( EXPORTING i_name = CONV string( ls_prog_tadir-object_name )
                                  IMPORTING e_exists_already_with_id = DATA(lv_exists_already_with_id) ).

              ELSE.
                " SAP_2_FAMIX_35        Add a usage to a single dummy class "OTHER_SAP_CLASS" if required by a parameter

                i_famix_class->add( EXPORTING i_name = 'OTHER_SAP_CLASS'
                                  IMPORTING e_exists_already_with_id = lv_exists_already_with_id ).

              ENDIF.

              " Now there is a class, but no duplicate class

              IF parameter_usage_outpack_groupd EQ false.
                lv_using_method_id = i_famix_method->get_id( i_class  = CONV string( ls_prog_tadir-object_name )
                                                                i_method = lv_using_method ).
              ELSE.
                lv_using_method_id = i_famix_method->get_id( i_class  = 'OTHER_SAP_CLASS'
                                                             i_method = 'OTHER_SAP_METHOD' ).
              ENDIF.


              IF lv_using_method_id EQ 0.
                IF parameter_usage_outpack_groupd EQ false.
                  " Now also the method is to be created
                  " SAP_2_FAMIX_23        If an object is used by a class that is not selected, add the using methods to the model

                  lv_using_method_id = i_famix_method->add( EXPORTING i_name = lv_using_method ).
                  " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
                  i_famix_method->set_signature( i_signature = lv_using_method ).
                  i_famix_method->set_parent_type( EXPORTING i_parent_element = 'FAMIX.Class'
                                                           i_parent_name    = CONV string( ls_prog_tadir-object_name ) ).
                  i_famix_method->store_id( i_class = CONV string( ls_prog_tadir-object_name )
                                            i_method = lv_using_method ).
                ELSE.

                  " SAP_2_FAMIX_36        Add a usage to a single dummy method "OTHER_SAP_METHOD" if required by a parameter

                  lv_using_method_id = i_famix_method->add( EXPORTING i_name = 'OTHER_SAP_METHOD' ).
                  " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
                  i_famix_method->set_signature( i_signature = 'OTHER_SAP_METHOD' ).
                  i_famix_method->set_parent_type( EXPORTING i_parent_element = 'FAMIX.Class'
                                                           i_parent_name    = 'OTHER_SAP_CLASS' ).
                  i_famix_method->store_id( i_class = 'OTHER_SAP_CLASS'
                                            i_method = 'OTHER_SAP_METHOD' ).
                ENDIF.
              ENDIF.

            ENDIF.

            CASE is_class_component-cmptype.
              WHEN c_cmptype_method.

                " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
                " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
                IF i_famix_invocation->is_new_invocation_to_candidate( i_sender_id   = lv_using_method_id
                                                                       i_candidates_id = iv_used_id ).
                  i_famix_invocation->add( ).
                  i_famix_invocation->set_invocation_by_reference( EXPORTING i_sender_id   = lv_using_method_id
                                                                           i_candidates_id = iv_used_id
                                                                           i_signature = 'DUMMY' ).
                ENDIF.
              WHEN c_cmptype_attribute.
                " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
                " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

                IF i_famix_access->is_new_access( i_accessor_id = lv_using_method_id
                                                  i_variable_id = iv_used_id ).
                  i_famix_access->add( ).
                  i_famix_access->set_accessor_variable_relation( EXPORTING i_accessor_id = lv_using_method_id
                                                                            i_variable_id = iv_used_id ).
                ENDIF.
              WHEN OTHERS.
                ASSERT 1 = 2.
            ENDCASE.


          WHEN OTHERS.
            " TBD Implement other usages
        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_default_language.

    " Set default language

    DATA(famix_custom_source_language) = NEW cl_famix_custom_source_lang( i_model ).

    famix_custom_source_language->add( i_name = 'ABAP' ).

    " Do not assign any entities to ABAP, because otherwise this will not be the default language anymore
    " So do not do this for ABAP, but maybe for another language
    " famix_package->set_declared_source_language( EXPORTING i_source_language_element = 'FAMIX.CustomSourceLanguage'
    "                                                        i_source_language_name    = 'ABAP' ).

  ENDMETHOD.


  METHOD _determine_packages_to_analyze.

    TYPES devclass TYPE devclass.
    DATA ty_devclass TYPE ty_devclass.

    " Determine packages to analyze

    DATA(famix_package) = NEW cl_famix_package( i_model ).

    "! This are all packages and sub packages that are selected (package = development class)
    DATA processed_dev_classes TYPE HASHED TABLE OF ty_devclass WITH UNIQUE KEY devclass.

    "! A temporal helper table used to find all packages (development classes) in the selection
    DATA temp_dev_classes_to_search TYPE STANDARD TABLE OF ty_devclass.



    famix_package->add( i_name = CONV string( is_devclass_first-devclass ) ).

    INSERT VALUE ty_devclass( devclass = is_devclass_first-devclass ) INTO TABLE r_processed_dev_classes.

    temp_dev_classes_to_search = VALUE #( ( devclass = parameter_package_to_analyze ) ).
    WHILE temp_dev_classes_to_search IS NOT INITIAL.
      IF temp_dev_classes_to_search IS NOT INITIAL.
        SELECT devclass, parentcl FROM tdevc INTO TABLE @DATA(lt_devclass)
         FOR ALL ENTRIES IN @temp_dev_classes_to_search WHERE parentcl = @temp_dev_classes_to_search-devclass.
      ENDIF.

      temp_dev_classes_to_search = VALUE #( ).

      LOOP AT lt_devclass INTO DATA(ls_devclass).

        INSERT VALUE ty_devclass( devclass = ls_devclass-devclass ) INTO TABLE r_processed_dev_classes.
        IF sy-subrc EQ ok.
          " New devclass
          " Search again
          temp_dev_classes_to_search = VALUE #( BASE temp_dev_classes_to_search ( devclass = ls_devclass-devclass ) ).
          famix_package->add( i_name = CONV string( ls_devclass-devclass ) ).
          famix_package->set_parent_package( i_parent_package = CONV string( ls_devclass-parentcl ) ).
        ENDIF.

      ENDLOOP.

      SORT temp_dev_classes_to_search.
      DELETE ADJACENT DUPLICATES FROM temp_dev_classes_to_search.

    ENDWHILE.

  ENDMETHOD.


  METHOD _objects_to_analyze_by_tadir.

    " Loop over all packages to find classes and programms

    " SAP_2_FAMIX_1     Extract classes from Dictionary
    " SAP_2_FAMIX_2     Extract interfaces as FAMIX.Class with attribute isinterface

    MOVE-CORRESPONDING it_tadir_objects TO et_classes_2.

    LOOP AT it_tadir_objects ASSIGNING FIELD-SYMBOL(<ls_tadir_objects>).

      IF <ls_tadir_objects>-object EQ 'CLAS'
        OR <ls_tadir_objects>-object EQ 'INTF'.

        et_classes_2 = VALUE #( BASE et_classes_2 ( obj_name = <ls_tadir_objects>-obj_name ) ).

      ELSE.

        et_programs = VALUE #( BASE et_programs ( program = <ls_tadir_objects>-obj_name ) ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _read_all_programs.

    " Read all programs

    " SAP_2_FAMIX_4     Extract programs

    DATA(famix_module) = NEW cl_famix_module( c_model ).

    LOOP AT it_programs ASSIGNING FIELD-SYMBOL(<ls_program>).

      " SAP_2_FAMIX_5     Map program to FAMIX.Module
      DATA(lv_module_reference) = famix_module->add( EXPORTING i_name = <ls_program>-program ).

      READ TABLE it_tadir_objects ASSIGNING FIELD-SYMBOL(<ls_tadir_object_2>) WITH TABLE KEY obj_name = <ls_program>-program.
      ASSERT sy-subrc EQ ok.

      famix_module->set_parent_package( i_parent_package = CONV string( <ls_tadir_object_2>-devclass ) ).

      DATA(lv_program_analyzer) = NEW cl_extract_program( ).

      lv_program_analyzer->extract( EXPORTING i_module_reference = lv_module_reference
                                              i_program          = CONV #( <ls_program>-program )
                                     CHANGING cr_model           = c_model ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _add_to_model.

    " Add to model
    LOOP AT it_existing_classes INTO DATA(ls_existing_class).
      r_famix_class  = NEW cl_famix_class( i_model ).

      " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
      " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
      r_famix_class->add( i_name = CONV string( ls_existing_class-class ) ).

      READ TABLE it_tadir_objects ASSIGNING FIELD-SYMBOL(<ls_tadir_object>) WITH TABLE KEY obj_name = ls_existing_class-class.
      ASSERT sy-subrc EQ ok.

      r_famix_class->set_parent_package( i_parent_package = CONV string( <ls_tadir_object>-devclass ) ).
      IF <ls_tadir_object>-object EQ 'INTF'.
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        r_famix_class->is_interface( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  DATA: lt_model TYPE cl_model=>ty_lines.
  IF parameter_extract_from_sap EQ false.
    cl_make_demo_model=>make( IMPORTING et_model = lt_model ).
  ELSE.
    cl_extract_sap=>extract( IMPORTING et_model = lt_model ).
  ENDIF.

  cl_output_model=>make( it_model = lt_model ).