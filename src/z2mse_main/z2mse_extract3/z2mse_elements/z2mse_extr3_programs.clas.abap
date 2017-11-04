CLASS z2mse_extr3_programs DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
        i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_programs.
    METHODS add
      IMPORTING
        program               TYPE progname
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS program_name
      IMPORTING
        i_element_id                 TYPE i
      EXPORTING
        VALUE(program)               TYPE progname
        VALUE(external_program_name) TYPE string
        VALUE(subc)                  TYPE subc.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_programs.
    TYPES: BEGIN OF element_type,
             element_id            TYPE z2mse_extr3_element_manager=>element_id_type,
             program               TYPE progname,
             external_program_name TYPE string,
             subc                  TYPE subc,
             program_type          TYPE string,
             program_attribute_1   TYPE string,
             adt_or_bwmt_link      TYPE string,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_program TYPE HASHED TABLE OF element_type WITH UNIQUE KEY program.
    METHODS _convert_program_2_ext_name
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        program_type TYPE string
        program_attribute_1 TYPE string
      RETURNING
        VALUE(r_result)   TYPE string.
    METHODS _extract_function_name
      IMPORTING
        i_element_program TYPE progname
      RETURNING
        VALUE(r_result)   TYPE string.
    METHODS _extract_sap_bw_logic
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        tranid            TYPE rstranid
      RETURNING
        VALUE(r_result)   TYPE string.
ENDCLASS.



CLASS z2mse_extr3_programs IMPLEMENTATION.

  METHOD clear.
    CLEAR instance.
  ENDMETHOD.

  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_program INTO element WITH KEY program = program.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does the program exist?
      DATA found_program TYPE progname.
      DATA found_subc    TYPE subc.
      TEST-SEAM progdir.
        " No blank between ( and found... to be 7.02 compatible
        SELECT SINGLE name subc FROM progdir INTO (found_program, found_subc ) WHERE name = program.
      END-TEST-SEAM.
      IF found_program IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_true ).
        element-element_id = new_element_id.
        element-program = found_program.
        element-external_program_name = _convert_program_2_ext_name( EXPORTING i_element_program = found_program
                                                                     IMPORTING program_type = element-program_type
                                                                               program_attribute_1 = element-program_attribute_1 ).
        element-subc = found_subc.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = program_type.
    r_instance = instance.
  ENDMETHOD.


  METHOD collect_infos.

    FIELD-SYMBOLS: <p> TYPE element_type,
                   <e> TYPE element_type.

    LOOP AT elements_program ASSIGNING <p>.

      IF <p>-program_type EQ 'BW_TRAN'.

        CONCATENATE 'bwmt://' sysid '/sap/bw/modeling/trfn/' <p>-program_attribute_1 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-adt_or_bwmt_link IS NOT INITIAL.

        READ TABLE elements_element_id ASSIGNING <e> WITH TABLE KEY element_id = <p>-element_id.
        ASSERT sy-subrc EQ 0.

        <e>-adt_or_bwmt_link = <p>-adt_or_bwmt_link.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA: last_id        TYPE i,
          file_anchor_id TYPE i.
*      famix_package->add( name = table-devclass ).

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
    element_manager->famix_class->add( EXPORTING name_group             = 'ABAP_PROGRAM'
                                                 name                   = element-external_program_name
                                                 modifiers              = z2mse_extract3=>modifier_program
                                       IMPORTING id         = last_id ).
    DATA association TYPE z2mse_extr3_element_manager=>association_type.
    LOOP AT associations INTO association WHERE element_id1 = element_id
                                            AND association->type = z2mse_extr3_association=>parent_package_ass.
      DATA package TYPE REF TO z2mse_extr3_packages.
      package ?= element_manager->get_element( i_element_id = association-element_id2 ).

      element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id
                                                 parent_package     = package->devclass( i_element_id = association-element_id2 ) ).

    ENDLOOP.

    DATA dummy_method_id TYPE i.

    element_manager->famix_method->add( EXPORTING name = element-external_program_name
                                        IMPORTING id   = dummy_method_id ).

    element_manager->famix_method->set_signature( element_id = dummy_method_id
                                                   signature = element-external_program_name ).

    element_manager->famix_method->set_parent_type( EXPORTING element_id        = dummy_method_id
                                                              parent_element    = 'FAMIX.Class'
                                                              parent_name_group = 'ABAP_PROGRAM'
                                                              parent_name       = element-external_program_name ).


    element_manager->famix_method->store_id( EXPORTING class  = element-external_program_name
                                                       method = element-external_program_name ).

    IF element-adt_or_bwmt_link IS NOT INITIAL.

      element_manager->famix_file_anchor->add( EXPORTING element_id = dummy_method_id " Required for Moose 6.1
                                                         file_name  = element-adt_or_bwmt_link
                                                 IMPORTING id         = file_anchor_id ).

      IF file_anchor_id IS NOT INITIAL.
        element_manager->famix_method->set_source_anchor_by_id(
          EXPORTING
            element_id         = dummy_method_id
            source_anchor_id   = file_anchor_id
        ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD program_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    program = element-program.
    external_program_name = element-external_program_name.
    subc = element-subc.

  ENDMETHOD.


  METHOD _convert_program_2_ext_name.

    clear program_type.
    clear program_attribute_1.

    data tranid type RSTRANID.

    IF i_element_program+0(1) EQ |L|.

      r_result = _extract_function_name( i_element_program ).

    ELSEIF sy-sysid EQ 'NPL' AND i_element_program+0(3) EQ |ZGP|. "Only on test system, currently no SAP BW working there

      r_result = _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                                        IMPORTING tranid = tranid ).

      program_type = |BW_TRAN|.
      program_attribute_1 = tranid.

    ELSEIF i_element_program+0(2) EQ |GP|.

      r_result = _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                                        IMPORTING tranid = tranid ).

      program_type = |BW_TRAN|.
      program_attribute_1 = tranid.

    ELSE.

      r_result = i_element_program.

    ENDIF.

  ENDMETHOD.

  METHOD _extract_function_name.

    " Extract function name

    DATA: length                TYPE i,
          postfix_position      TYPE i,
          include_type_position TYPE i,
          include_type          TYPE string,
          include               TYPE includenr,
          temp                  TYPE string,
          pname                 TYPE pname,
          funcname              TYPE rs38l_fnam.

    length = strlen( i_element_program ).

    IF length < 4.

      r_result = i_element_program.

    ELSE.

      postfix_position = length - 2.

      include_type_position = length - 3.

      include = i_element_program+postfix_position(2).

      include_type = i_element_program+include_type_position(1).

      IF include_type <> |U|.

        r_result = i_element_program.

      ELSE.

        postfix_position = length - 3.

        temp = i_element_program+0(postfix_position).

        CONCATENATE 'SAP' temp INTO pname.

        TEST-SEAM tfdir.

          SELECT SINGLE funcname FROM tfdir INTO funcname WHERE pname = pname
                                                            AND include = include.

        END-TEST-SEAM.

        IF sy-subrc <> 0.
          r_result = i_element_program.
        ELSE.
          r_result = |F-| && funcname.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _extract_sap_bw_logic.

    " Extract SAP BW logic

    DATA: element_program         TYPE progname,
          transformation_progr_id TYPE rstran_progid,
          length                  TYPE i,
          id_length               TYPE i,
          transformation          TYPE rstran.

    CLEAR tranid.

    element_program = i_element_program.

    IF sy-sysid EQ 'NPL' AND element_program+0(3) = 'ZGP'.
      SHIFT element_program LEFT BY 1 PLACES.
    ENDIF.

    length = strlen( i_element_program ).
    id_length = length - 2.

    transformation_progr_id = i_element_program+2(id_length).

    IF sy-sysid EQ 'NPL' AND element_program = 'GP003N8S45LS1FG375G2BN69Q4G'.
      CLEAR transformation.
      transformation-tranid = |123|.
      transformation-sourcetype = |ODSO|.
      transformation-sourcename = |Z2MSET001|.
      transformation-targettype = |CUBE|.
      transformation-targetname = |Z2MSET002|.
    ELSE.

      SELECT SINGLE * FROM rstran INTO transformation WHERE objvers = 'A'
                                                        AND tranprog = transformation_progr_id.

    ENDIF.

    IF sy-subrc <> 0.
      r_result = i_element_program.
    ELSE.
      r_result = |BW-| && transformation-sourcetype && |-|
                       && transformation-sourcename && |-|
                       && transformation-targettype && |-|
                       && transformation-targetname.
      " In case of InfoSources there are multiple blanks in the field SOURCENAME
      " Remove all but a single blank
      CONDENSE r_result.

      tranid = transformation-tranid.

    ENDIF.

  ENDMETHOD.

  METHOD name.

    element_type = |ABAPProgramOrFunctionOrSAPBW|.
    program_name( EXPORTING i_element_id          = element_id
                  IMPORTING external_program_name = name ).
    parent_name = ||.

  ENDMETHOD.

ENDCLASS.
