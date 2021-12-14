CLASS z2mse_extr3_invocation DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_access_or_invocatn
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(r_instance) TYPE REF TO z2mse_extr3_invocation.
    METHODS add
      IMPORTING
        invoced_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type
        invocing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_invocation.

    TYPES: BEGIN OF association_type,
             invoced_element_id1  TYPE z2mse_extr3_element_manager=>element_id_type,
             invocing_element_id2 TYPE z2mse_extr3_element_manager=>element_id_type,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH DEFAULT KEY.
    DATA associations TYPE HASHED TABLE OF association_type WITH UNIQUE KEY invoced_element_id1 invocing_element_id2.

ENDCLASS.



CLASS z2mse_extr3_invocation IMPLEMENTATION.

  METHOD clear.
    CLEAR instance.
  ENDMETHOD.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance->type = invocation_ass.
    r_instance = instance.
  ENDMETHOD.

  METHOD add.

    DATA association TYPE association_type.

    ASSERT invoced_element_id1 IS NOT INITIAL.
    ASSERT invocing_element_id2 IS NOT INITIAL.

    association-invoced_element_id1 = invoced_element_id1.
    association-invocing_element_id2 = invocing_element_id2.
    INSERT association INTO TABLE associations.

    element_manager->add_association( EXPORTING element_1   = association-invoced_element_id1
                                                element_2   = association-invocing_element_id2
                                                association = me ).

  ENDMETHOD.

  METHOD make_model.

    DATA using_id TYPE i.
    DATA used_id TYPE i.



    IF element_manager->use_somix EQ 'X'.
      _get_somix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_id    = using_id
                                              e_used_id     = used_id ).
    ELSE.
      _get_famix_id_used_and_using( EXPORTING i_association = association
                                    IMPORTING e_using_method_id = using_id
                                              e_used_id         = used_id ).
    ENDIF.

    ASSERT using_id IS NOT INITIAL.
    ASSERT used_id IS NOT INITIAL.

    IF element_manager->use_somix EQ 'X'.
      DATA call_id TYPE i.
      call_id = element_manager->somix_access->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id   = call_id
          caller_id  = using_id
          called_id  = used_id ).
    ELSE.
      DATA invocation_id TYPE i.
      invocation_id = element_manager->famix_invocation->add( ).
      element_manager->famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                 sender_id     = using_id
                                                                 candidates_id = used_id
                                                                 signature     = 'DUMMY' ).
    ENDIF.


  ENDMETHOD.




ENDCLASS.
