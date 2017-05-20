"! Analyzes local objects of ABAP programs
"! Is not yet completely implemented
CLASS z2mse_program_analyzer DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS extract
      IMPORTING
        module_reference TYPE i
        program          TYPE char30
      CHANGING
        model            TYPE REF TO z2mse_model.
  PRIVATE SECTION.
    CONSTANTS modifiers_abaplocalclass TYPE string VALUE 'ABAPLocalClass' ##NO_TEXT.
    DATA g_parameter_list_tokens TYPE abap_bool.

ENDCLASS.



CLASS Z2MSE_PROGRAM_ANALYZER IMPLEMENTATION.


  METHOD extract.
    " Old coding, only reference on how it was done
*    DATA source TYPE TABLE OF string.
*    READ REPORT program INTO source.
*
*    DATA: tokens TYPE STANDARD TABLE OF stokes.
*
*    DATA: sorted_tokens TYPE z2mse_ep_analyze_other_keywrd=>sorted_tokens_type.
*
*    DATA statements TYPE STANDARD TABLE OF sstmnt.
*
*    SCAN ABAP-SOURCE source TOKENS INTO tokens STATEMENTS INTO statements.
*
*    FIELD-SYMBOLS <ls_token_2> LIKE LINE OF tokens.
*    LOOP AT tokens ASSIGNING <ls_token_2>.
*      DATA ls_token LIKE LINE OF sorted_tokens. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
*      CLEAR ls_token.
*      ls_token-index = sy-tabix.
*      ls_token-str   = <ls_token_2>-str.
*      ls_token-row   = <ls_token_2>-row.
*      ls_token-col   = <ls_token_2>-col.
*      ls_token-type  = <ls_token_2>-type.
*      INSERT ls_token INTO TABLE sorted_tokens.
*    ENDLOOP.
*
*    SORT statements BY from.
*
*    IF g_parameter_list_tokens EQ abap_true.
*      WRITE: /.
*      WRITE: / program.
*
*    ENDIF.
*
*    TYPES section_type TYPE c LENGTH 1.
*
*    CONSTANTS: "! Is in public section
*      public    TYPE section_type VALUE '1',
*      "! Is in protected section
*      protected TYPE section_type VALUE '2',
*      "! Is in private section
*      private   TYPE section_type VALUE '3',
*      "! Not in a section
*      none      TYPE section_type VALUE ' '.
*
*    TYPES: BEGIN OF codecontext_type,
*             in_section               TYPE section_type,
*             in_class_definition      TYPE abap_bool,
*             implementation_of_class  TYPE string,
*             implementation_of_method TYPE string,
*           END OF codecontext_type.
*
*    "! Context of statement in the code
*    DATA context TYPE codecontext_type.
*
*    TYPES: BEGIN OF class_with_model_id_type,
*             classname   TYPE string,
*             id_in_model TYPE i,
*           END OF class_with_model_id_type.
*
*    DATA: classes_with_model_id      TYPE HASHED TABLE OF class_with_model_id_type WITH UNIQUE KEY classname,
*          actual_class_with_model_id TYPE class_with_model_id_type.
*
*    TYPES: BEGIN OF method_type,
*             classname          TYPE string,
*             class_id_in_model  TYPE i,
*             methodname         TYPE string,
*             method_id_in_model TYPE i,
*             in_section         TYPE section_type,
*             instanciable       TYPE abap_bool,
*           END OF method_type.
*
*    DATA: methods       TYPE STANDARD TABLE OF method_type,
*          actual_method TYPE method_type.
*
*    TYPES: BEGIN OF inheritance_type,
*             subclass   TYPE string,
*             superclass TYPE string,
*           END OF inheritance_type.
*
*    DATA: inheritances TYPE STANDARD TABLE OF inheritance_type.
*
*    DATA token_number TYPE i.
*
*    "! Instance that analyzes other ABAP Keywords
*    DATA aok TYPE REF TO z2mse_ep_analyze_other_keywrd. " So that ABAP Doc Comment is possible
*    CREATE OBJECT aok EXPORTING sorted_tokens = sorted_tokens.
*
*    FIELD-SYMBOLS <statement> LIKE LINE OF statements.
*    LOOP AT statements ASSIGNING <statement>.
*
*      token_number = 0.
*      CASE <statement>-type.
*        WHEN 'K'.
*
*          aok->analyze( statement = <statement> ).
*          CASE aok->g_info-statement_type.
*            WHEN aok->start_class_definition.
*              " SAP_2_FAMIX_28        Determine local classes in programs
*              context-in_class_definition = abap_true.
*              actual_class_with_model_id-classname = aok->g_info-name.
*              INSERT actual_class_with_model_id INTO TABLE classes_with_model_id.
*              IF aok->g_info-class_is_inheriting EQ abap_true.
*                " SAP_2_FAMIX_37        Determine local inheritances of classes
*                DATA ls_inheritance_2 LIKE LINE OF inheritances. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
*                CLEAR ls_inheritance_2.
*                ls_inheritance_2-subclass = actual_class_with_model_id-classname.
*                ls_inheritance_2-superclass = aok->g_info-class_inherits_from.
*                INSERT ls_inheritance_2 INTO TABLE inheritances.
*              ENDIF.
*            WHEN aok->start_public.
*              context-in_section = public.
*            WHEN aok->start_protected.
*              context-in_section = protected.
*            WHEN aok->start_private.
*              context-in_section = private.
*            WHEN aok->end_class.
*              context-in_section = none.
*              context-in_class_definition = abap_false.
*              CLEAR context-implementation_of_class.
*            WHEN aok->method_definition.
*              " SAP_2_FAMIX_29      Determine local class methods in programs
*              IF aok->g_info-is_static EQ abap_true.
*                CLEAR actual_method.
*                actual_method-classname = actual_class_with_model_id-classname.
*                actual_method-in_section = context-in_section.
*              ELSE.
*                CLEAR actual_method.
*                actual_method-classname = actual_class_with_model_id-classname.
*                actual_method-in_section = context-in_section.
*                actual_method-instanciable = abap_true.
*              ENDIF.
*              actual_method-methodname = aok->g_info-name.
*            WHEN aok->start_class_implementation.
*              context-implementation_of_class = aok->g_info-name.
*            WHEN aok->start_method_implementation.
*              context-implementation_of_method = aok->g_info-name.
*              IF g_parameter_list_tokens EQ abap_true.
*                FORMAT COLOR COL_GROUP.
*              ENDIF.
*            WHEN aok->end_method_implementation.
*              CLEAR context-implementation_of_method.
*              IF g_parameter_list_tokens EQ abap_true.
*                FORMAT COLOR COL_BACKGROUND.
*              ENDIF.
*            WHEN OTHERS.
*
*          ENDCASE.
*
*        WHEN OTHERS.
*
*      ENDCASE.
*
*      IF g_parameter_list_tokens EQ abap_true.
*        WRITE: / <statement>-type.
*        FIELD-SYMBOLS <token> LIKE LINE OF sorted_tokens.
*        LOOP AT sorted_tokens ASSIGNING <token> WHERE
*            index >= <statement>-from
*        AND index <= <statement>-to.
*          WRITE: '|', <token>-type, <token>-str.
*        ENDLOOP.
*      ENDIF.
*
*    ENDLOOP.
*
*    " Add local classes to model
*
*    DATA sap_class TYPE REF TO Z2MSE_sap_class.
*    CREATE OBJECT sap_class EXPORTING model = model.
*
*    FIELD-SYMBOLS <class> LIKE LINE OF classes_with_model_id.
*    LOOP AT classes_with_model_id ASSIGNING <class>.
*      " SAP_2_FAMIX_30        Map local classes of programs to FAMIX.Class
*      " SAP_2_FAMIX_61      Mark the FAMIX Class with the attribute modifiers = 'ABAPLocalClass'
*      <class>-id_in_model = sap_class->add_local( program   = program
*                                                  name      = <class>-classname
*                                                  modifiers = modifiers_abaplocalclass ).
*
*      sap_class->set_parent_program( element_id = <class>-id_in_model
*                                     sap_program = program ).
*
*
*    ENDLOOP.
*
*    " Add local methods to model
*
*    DATA sap_method TYPE REF TO z2mse_sap_method.
*    CREATE OBJECT sap_method EXPORTING model = model.
*
*    FIELD-SYMBOLS <method> LIKE LINE OF methods.
*    LOOP AT methods ASSIGNING <method>.
*      FIELD-SYMBOLS <class_2> LIKE LINE OF classes_with_model_id.
*      READ TABLE classes_with_model_id ASSIGNING <class_2> WITH TABLE KEY classname = <method>-classname.
*      <method>-class_id_in_model = <class_2>-id_in_model.
*
*      <method>-method_id_in_model = sap_method->add_local_method( class_name  = <class_2>-classname
*                                                                  class_id    = <class_2>-id_in_model
*                                                                  method_name = <method>-methodname ).
*
**      " SAP_2_FAMIX_32      Map local methods to the FAMIX.Method
**
**      <method>-method_id_in_model = famix_method->add( EXPORTING name_group = <class_2>-classname " Why classname in name_group?
**                                                                 name       = <method>-methodname ).
**      " SAP_2_FAMIX_43        Fill the attribute signature of FAMIX.METHOD with the name of the method
**      famix_method->set_signature( signature = <method>-methodname ).
**
**      " SAP_2_FAMIX_33      Set the attribute parentType of FAMIX.Method for local methods to the name of the local class
**
**
**      famix_method->set_parent_type( EXPORTING parent_element = 'FAMIX.Class'
**                                               parent_id      =  <class_2>-id_in_model ).
*    ENDLOOP.
*
*    " Add local inheritances to model
*
*    DATA sap_inheritance TYPE REF TO Z2MSE_sap_inheritance.
*    CREATE OBJECT sap_inheritance EXPORTING model = model.
*    DATA inheritance LIKE LINE OF inheritances.
*    LOOP AT inheritances INTO inheritance.
*      DATA last_used_id TYPE i.
*      last_used_id = sap_inheritance->add( ).
*      sap_inheritance->set_local_sub_and_super_class( EXPORTING element_id = last_used_id
*                                                                program = program
*                                                                subclass_name   = inheritance-subclass
*                                                                superclass_name = inheritance-superclass ).
*
*    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
