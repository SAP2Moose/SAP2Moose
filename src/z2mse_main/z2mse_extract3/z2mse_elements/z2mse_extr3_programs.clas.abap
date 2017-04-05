CLASS z2mse_extr3_programs DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
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
        i_element_id   TYPE i
      EXPORTING
        VALUE(program) TYPE progname
        VALUE(subc)    TYPE subc.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_programs.
    TYPES: BEGIN OF element_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             program    TYPE progname,
             subc       TYPE subc,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_program TYPE HASHED TABLE OF element_type WITH UNIQUE KEY program.
ENDCLASS.



CLASS Z2MSE_EXTR3_PROGRAMS IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = program_type.
    r_instance = instance.
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
        SELECT SINGLE name subc FROM progdir INTO ( found_program, found_subc ) WHERE name = program.
      END-TEST-SEAM.
      IF found_program IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me ).
        element-element_id = new_element_id.
        element-program = found_program.
        element-subc = found_subc.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_program.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA last_id TYPE i.
*      famix_package->add( name = table-devclass ).

    " SAP_2_FAMIX_54        Map database tables to FAMIX Class
    " SAP_2_FAMIX_58        Mark the FAMIX Class with the attribute modifiers = 'DBTable'
    element_manager->famix_class->add( EXPORTING name_group             = 'ABAP_PROGRAM'
                                                 name                   = element-program
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

    DATA dummy_attribute_id TYPE i.
    " SAP_2_FAMIX_56      Add a dummy attribute with the name of the table
    element_manager->famix_attribute->add( EXPORTING name                   = element-program
                                           IMPORTING id                     = dummy_attribute_id ).

    element_manager->famix_attribute->set_parent_type( EXPORTING element_id         = dummy_attribute_id
                                                parent_id          = last_id ).

    element_manager->famix_attribute->store_id( EXPORTING class     = element-program
                                                          attribute = element-program ).

  ENDMETHOD.


  METHOD program_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    program = element-program.
    subc = element-subc.

  ENDMETHOD.
ENDCLASS.
