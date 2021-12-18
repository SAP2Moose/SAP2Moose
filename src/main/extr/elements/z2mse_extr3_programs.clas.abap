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
    CONSTANTS: type_function          TYPE string VALUE 'FUNCTION',
               type_function_include  TYPE string VALUE 'FUNCTION_INCLUDE',
               type_program           TYPE string VALUE 'PROGRAM',
               type_bw_transformation TYPE string VALUE 'BW_TRAN'.
    METHODS add
      IMPORTING
        program               TYPE program
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS add_function
      IMPORTING
        function              TYPE string
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS program_name
      IMPORTING
        i_element_id                        TYPE i
      EXPORTING
        VALUE(program_type)                 TYPE string
        VALUE(program)                      TYPE progname
        VALUE(external_program_name_class)  TYPE string
        VALUE(external_program_name_method) TYPE string
        VALUE(program_attribute_1)          TYPE string
        VALUE(program_attribute_2)          TYPE string
        VALUE(subc)                         TYPE subc.
    METHODS add_function_group
      IMPORTING
        fgr            TYPE string
      EXPORTING
        is_added       TYPE abap_bool
        new_element_id TYPE i.

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
             program_attribute_2   TYPE string,
             adt_or_bwmt_link      TYPE string,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_program TYPE HASHED TABLE OF element_type WITH UNIQUE KEY program.
    METHODS _convert_program_2_ext_name
      IMPORTING
        i_element_program   TYPE progname
      EXPORTING
        program_type        TYPE string
        program_attribute_1 TYPE string
        program_attribute_2 TYPE string
        VALUE(r_result)     TYPE string.
    METHODS _extract_function_name
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        function_group    TYPE rs38l_area
        function          TYPE rs38l_fnam
        function_include  TYPE string
        VALUE(r_result)   TYPE string.
    METHODS _extract_sap_bw_logic
      IMPORTING
        i_element_program TYPE progname
      EXPORTING
        tranid            TYPE rstranid
        VALUE(r_result)   TYPE string.
    METHODS _get_names_for_function_groups
      IMPORTING
        i_element                   TYPE z2mse_extr3_programs=>element_type
      RETURNING
        VALUE(name_of_mapped_class) TYPE string.
ENDCLASS.



CLASS Z2MSE_EXTR3_PROGRAMS IMPLEMENTATION.


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
        _convert_program_2_ext_name( EXPORTING i_element_program = found_program
                                     IMPORTING program_type = element-program_type
                                               program_attribute_1 = element-program_attribute_1
                                               program_attribute_2 = element-program_attribute_2
                                               r_result = element-external_program_name ).
        element-subc = found_subc.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD add_function.

    DATA: program_found TYPE progname,
          tf            TYPE tfdir.
    " TBD find a better solution for this
    SELECT SINGLE * FROM tfdir INTO tf WHERE funcname = function .
    IF tf IS NOT INITIAL.
      "TBD handle error
    ENDIF.
    program_found = tf-pname.
    SHIFT program_found LEFT BY 3 PLACES.
    program_found = program_found && |U| && tf-include.

    add( EXPORTING program        = program_found
         IMPORTING is_added       = is_added
                   new_element_id = new_element_id ).

  ENDMETHOD.


  METHOD add_function_group.

    DATA pname TYPE pname.

    TYPES: BEGIN OF ty_function_group,
             include TYPE includenr,
           END OF ty_function_group.

    DATA: fg  TYPE ty_function_group,
          fgs TYPE STANDARD TABLE OF ty_function_group WITH DEFAULT KEY.

    pname = |SAPL| && fgr.

    SELECT include FROM tfdir INTO TABLE fgs WHERE pname = pname.

    LOOP AT fgs INTO fg.

      DATA progname TYPE progname.

      progname = |L| && fgr && |U| && fg-include.

      DATA is_found TYPE abap_bool.

      DATA: fg_new_element_id TYPE z2mse_extr3_element_manager=>element_id_type.

      add( EXPORTING program        = progname
           IMPORTING is_added       = is_found
                     new_element_id = fg_new_element_id ).

    ENDLOOP.

  ENDMETHOD.


  METHOD clear.
    CLEAR instance.
  ENDMETHOD.


  METHOD collect_infos.

    FIELD-SYMBOLS: <p> TYPE element_type,
                   <e> TYPE element_type.

    LOOP AT elements_program ASSIGNING <p>.

      IF <p>-program_type EQ type_program.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/programs/programs/' <p>-program_attribute_1 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_function.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.
        TRANSLATE <p>-program_attribute_2 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/functions/groups/' <p>-program_attribute_1 '/fmodules/' <p>-program_attribute_2 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_function_include.

        TRANSLATE <p>-program_attribute_1 TO LOWER CASE.
        TRANSLATE <p>-program_attribute_2 TO LOWER CASE.

        CONCATENATE 'adt://' sysid '/sap/bc/adt/functions/groups/' <p>-program_attribute_1 '/includes/' <p>-program_attribute_2 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-program_type EQ type_bw_transformation.

        CONCATENATE 'bwmt://' sysid '/sap/bw/modeling/trfn/' <p>-program_attribute_1 INTO <p>-adt_or_bwmt_link.

      ENDIF.

      IF <p>-adt_or_bwmt_link IS NOT INITIAL.

        READ TABLE elements_element_id ASSIGNING <e> WITH TABLE KEY element_id = <p>-element_id.
        ASSERT sy-subrc EQ 0.

        <e>-adt_or_bwmt_link = <p>-adt_or_bwmt_link.

      ENDIF.

    ENDLOOP.

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


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA: last_id              TYPE i,
          file_anchor_id       TYPE i,
          name_group           TYPE string,
          modifier             TYPE string,
          name_of_mapped_class TYPE string.
*      famix_package->add( name = table-devclass ).

    IF element-program_type EQ type_program.
      name_group = 'ABAP_PROGRAM'.
      modifier = z2mse_extract3=>modifier_program.
      name_of_mapped_class = element-external_program_name.
    ELSEIF element-program_type EQ type_bw_transformation.
      name_group = 'BW_TRANSFORMATION'.
      modifier = z2mse_extract3=>modifier_bw_transformation.
      name_of_mapped_class = element-external_program_name.
    ELSEIF element-program_type EQ type_function OR element-program_type = type_function_include.
      name_group = 'ABAP_FUNCTIONGROUP'.
      modifier = z2mse_extract3=>modifier_function_group.
*      name_of_mapped_class = element-external_program_name.
      name_of_mapped_class = _get_names_for_function_groups( element ).

*      " Get parent package for function group
*      DATA devclass TYPE tadir-devclass.
*      SELECT SINGLE devclass FROM tadir INTO devclass WHERE pgmid = 'R3TR' AND object = 'FUGR' AND obj_name = element-program_attribute_1.

    ELSE.
      name_group = 'UNKNOWN'.
      modifier = z2mse_extract3=>modifier_unknown.
      name_of_mapped_class = element-external_program_name.
    ENDIF.

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'

    IF element_manager->use_somix EQ 'X'.

      element_manager->somix_grouping->add( EXPORTING name_group      = name_group
                                                      name            = name_of_mapped_class
                                                      technical_type  = modifier
                                                      link_to_editor  = element-adt_or_bwmt_link
                                            IMPORTING id              = last_id ).

    ELSE.

      element_manager->famix_class->add( EXPORTING name_group             = name_group
                                                   name                   = name_of_mapped_class
                                                   modifiers              = modifier
                                         IMPORTING id         = last_id ).

    ENDIF.

    DATA association TYPE z2mse_extr3_element_manager=>association_type.
*    DATA: package_set TYPE abap_bool.
    LOOP AT associations INTO association WHERE element_id1 = element_id
                                            AND association->type = z2mse_extr3_association=>parent_package_ass.
      DATA package TYPE REF TO z2mse_extr3_packages.
      package ?= element_manager->get_element( i_element_id = association-element_id2 ).

      element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id
                                                                  parent_package     = package->devclass( i_element_id = association-element_id2 )
                                                                  parent_package_name_group = ng_abap_package ).
*      package_set = abap_true.
    ENDLOOP.
*    IF package_set EQ abap_false.
*
*      DATA packages_elements TYPE REF TO z2mse_extr3_packages.
*
*      packages_elements = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).
*
*      packages_elements->add( EXPORTING package = devclass ).
*
*      element_manager->famix_class->set_parent_package( EXPORTING element_id         = last_id
*                                                                  parent_package     = devclass
*                                                                  parent_package_name_group = ng_abap_package ).
*    ENDIF.

    DATA dummy_method_id TYPE i.

    IF element_manager->use_somix EQ 'X'.

      element_manager->somix_code->add( EXPORTING name           = element-external_program_name
                                                  name_group     = z2mse_extr3=>ng_abap_program
                                                  technical_type = z2mse_extract3=>modifier_program
                                                  link_to_editor = element-adt_or_bwmt_link
                                        IMPORTING id             = dummy_method_id ).

    ELSE.

      element_manager->famix_method->add( EXPORTING name = element-external_program_name
                                          IMPORTING id   = dummy_method_id ).

      element_manager->famix_method->set_signature( element_id = dummy_method_id
                                                     signature = element-external_program_name ).

      element_manager->famix_method->set_parent_type( EXPORTING element_id        = dummy_method_id
                                                                parent_element    = 'FAMIX.Class'
                                                                parent_name_group = name_group
                                                                parent_name       = name_of_mapped_class ).

    ENDIF.


    element_manager->famix_method->store_id( EXPORTING class_name_group = ng_abap_program
                                                       class  = name_of_mapped_class
                                                       method_name_group = ng_abap_program
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


  METHOD name.

    element_type = |ABAPProgramOrFunctionOrSAPBW|.
    program_name( EXPORTING i_element_id          = element_id
                  IMPORTING external_program_name_method = name ).
    parent_name = ||.

  ENDMETHOD.


  METHOD program_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.
    program_type = element-program_type.
    program_attribute_1 = element-program_attribute_1.
    program_attribute_2 = element-program_attribute_2.
    program = element-program.
    IF element-program_type EQ type_function OR element-program_type = type_function_include.
      external_program_name_class = _get_names_for_function_groups( i_element = element ).
    ELSE.
      external_program_name_class = element-external_program_name.
    ENDIF.
    external_program_name_method = element-external_program_name.
    subc = element-subc.

  ENDMETHOD.


  METHOD _convert_program_2_ext_name.

    CLEAR program_type.
    CLEAR program_attribute_1.

    DATA: tranid           TYPE rstranid,
          function_group   TYPE rs38l_area,
          function         TYPE rs38l_fnam,
          function_include TYPE string,
          is_tested        TYPE abap_bool.

    CLEAR program_type.
    CLEAR program_attribute_1.
    CLEAR program_attribute_2.

    IF i_element_program+0(3) EQ |ZGP|.
      " Do check only when potentially needed to improve performance
      is_tested = z2mse_extract3=>check_if_tested( ).
    ENDIF.

    IF i_element_program+0(1) EQ |L|.

      _extract_function_name( EXPORTING i_element_program = i_element_program
                              IMPORTING function_group = function_group
                                        function = function
                                        function_include = function_include
                                        r_result = r_result ).

      IF function IS NOT INITIAL.

        program_type = type_function.
        program_attribute_1 = function_group.
        program_attribute_2 = function.

      ELSEIF function_include IS NOT INITIAL.

        program_type = type_function_include.
        program_attribute_1 = function_group.
        program_attribute_2 = function_include.

      ENDIF.

    ELSEIF i_element_program+0(3) EQ |ZGP|. "Only on test system, currently no SAP BW working there
      ##TODO " Find better way to determine Unit Test
      _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                             IMPORTING tranid = tranid
                                       r_result = r_result ).

      program_type = type_bw_transformation.
      program_attribute_1 = tranid.

    ELSEIF i_element_program+0(2) EQ |GP|.

      _extract_sap_bw_logic( EXPORTING i_element_program = i_element_program
                             IMPORTING tranid = tranid
                                       r_result = r_result ).

      program_type = type_bw_transformation.
      program_attribute_1 = tranid.

    ELSE.

      r_result = i_element_program.

      program_type = type_program.
      program_attribute_1 = i_element_program.

    ENDIF.

  ENDMETHOD.


  METHOD _extract_function_name.

    " Extract function name

    DATA: length                TYPE i,
          postfix_position      TYPE i,
          include_type_position TYPE i,
          function_group_length TYPE i,
          include_type          TYPE string,
          include               TYPE includenr,
          temp                  TYPE string,
          pname                 TYPE pname,
          funcname              TYPE rs38l_fnam.

    CLEAR function.
    CLEAR function_group.
    CLEAR function_include.

    length = strlen( i_element_program ).

    IF length < 5.

      r_result = i_element_program.

    ELSE.

      postfix_position = length - 2.

      include_type_position = length - 3.

      function_group_length = length - 4.

      include = i_element_program+postfix_position(2).

      include_type = i_element_program+include_type_position(1).

      temp = i_element_program+0(include_type_position).

      IF include_type EQ |F| OR include_type EQ |U|.

        function_group = temp+1(function_group_length).

      ENDIF.

      IF include_type EQ |F|.

        r_result = i_element_program.

        function_include = i_element_program.

      ELSEIF include_type EQ |U|.

        postfix_position = length - 3.

        CONCATENATE 'SAP' temp INTO pname.

        TEST-SEAM tfdir.

          SELECT SINGLE funcname FROM tfdir INTO funcname WHERE pname = pname
                                                            AND include = include.

        END-TEST-SEAM.

        IF sy-subrc <> 0.
          r_result = i_element_program.
        ELSE.
          r_result = |F-| && funcname.

          function = funcname.

        ENDIF.
      ELSE.

        r_result = i_element_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _extract_sap_bw_logic.

    " Extract SAP BW logic

    DATA: element_program         TYPE progname,
          transformation_progr_id TYPE rstran_progid,
          length                  TYPE i,
          id_length               TYPE i,
          transformation          TYPE rstran,
          is_tested               TYPE abap_bool.

    CLEAR tranid.

    element_program = i_element_program.

    IF element_program+0(3) = 'ZGP'.
      IF element_program = 'ZGP003N8S45LS1FG375G2BN69Q4G'.
        is_tested = z2mse_extract3=>check_if_tested( ).
        IF is_tested = 'X'.
          SHIFT element_program LEFT BY 1 PLACES.
        ENDIF.
      ENDIF.
    ENDIF.

    length = strlen( i_element_program ).
    id_length = length - 2.

    transformation_progr_id = i_element_program+2(id_length).

    IF element_program = 'GP003N8S45LS1FG375G2BN69Q4G' AND is_tested = 'X'.
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


  METHOD _get_names_for_function_groups.

    CONCATENATE 'FGR-' i_element-program_attribute_1 INTO name_of_mapped_class.

  ENDMETHOD.
ENDCLASS.
