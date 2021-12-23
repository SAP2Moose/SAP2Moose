CLASS z2mse_extr3_access DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_access_or_invocatn
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO z2mse_extr3_access.
    METHODS add
      IMPORTING
        accessed_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type
        accessing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_access.

    TYPES: BEGIN OF association_type,
             accessed_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type,
             accessing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY accessed_element_id1 accessing_element_id2.
ENDCLASS.



CLASS z2mse_extr3_access IMPLEMENTATION.


  METHOD add.

    DATA association TYPE association_type.

    ASSERT accessed_element_id1 IS NOT INITIAL.
    ASSERT accessing_element_id2 IS NOT INITIAL.

    association-accessed_element_id1 = accessed_element_id1.
    association-accessing_element_id2 = accessing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-accessed_element_id1
                                                element_2   = association-accessing_element_id2
                                                association = me ).

  ENDMETHOD.


  METHOD clear.
    CLEAR instance.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = access_ass..
    r_instance = instance.
  ENDMETHOD.


  METHOD make_model.

    DATA using_method_id TYPE i.
    DATA using_id TYPE i.
    DATA used_id TYPE i.


    IF element_manager->use_somix EQ 'X'.
      _get_somix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_id    = using_id
                                              e_used_id     = used_id ).

      ASSERT using_id IS NOT INITIAL.
      ASSERT used_id IS NOT INITIAL.
    ELSE.
      _get_famix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_method_id = using_method_id
                                              e_used_id         = used_id ).

      ASSERT using_method_id IS NOT INITIAL.
      ASSERT used_id IS NOT INITIAL.
    ENDIF.

    DATA last_id2 TYPE i.
    IF element_manager->use_somix EQ 'X'.
      last_id2 = element_manager->somix_access->add( ).
      element_manager->somix_access->set_accessor_accessed_relation(
        EXPORTING
          element_id   = last_id2
          accessor_id  = using_id
          accessed_id  = used_id
          is_write     = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
          is_read      = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
          is_dependent = 'X' " SAP2Moose cannot differenciate currently, between read, write, and dependency. So set here always.
      ).
    ELSE.
      last_id2 = element_manager->famix_access->add( ).
      element_manager->famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id2
                                                                               accessor_id = using_method_id
                                                                               variable_id = used_id ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
