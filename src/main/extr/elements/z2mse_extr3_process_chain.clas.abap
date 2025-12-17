CLASS z2mse_extr3_process_chain DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_process_chain.
    METHODS plan_seq_used_in_pc_chain
      IMPORTING seqnm    TYPE clike
                plseq_id TYPE i.
    METHODS program_used_in_pc_chain
      IMPORTING program    TYPE clike
                program_id TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_process_chain.
ENDCLASS.



CLASS z2mse_extr3_process_chain IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
    ENDIF.
    r_instance = instance.

  ENDMETHOD.

  METHOD plan_seq_used_in_pc_chain.

    DATA variants TYPE STANDARD TABLE OF rspcvariant WITH DEFAULT KEY.
    DATA variant TYPE rspcvariant.
    DATA pc_chains TYPE STANDARD TABLE OF rspcchain WITH DEFAULT KEY.
    DATA pc_chain TYPE rspcchain.

    SELECT * FROM rspcvariant INTO TABLE variants WHERE type = 'PLSEQ' AND objvers = 'A' AND fnam = 'SEQNM' AND low = seqnm.

    LOOP AT variants INTO variant.

      " Add variant
      DATA unique_name TYPE string.
      unique_name            = |sap.{ variant-variante }|.
      DATA variant_id TYPE i.
      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = 'PROCESS_CHAIN_VARIANT'
          code                   = variant-variante
          technical_type         = 'PROCESS_CHAIN_VARIANT'
          link_to_editor         = ''
        IMPORTING
*                    exists_already_with_id =
          id                     = variant_id
        CHANGING
          unique_name            = unique_name
      ).

      DATA call_id TYPE i.
      call_id = element_manager->somix_call->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id = call_id
          caller_id  = variant_id
          called_id  = plseq_id
      ).

      " Get Process Chains from variant Duplicate 1 / 2
      CLEAR pc_chains.

      SELECT * FROM rspcchain INTO TABLE pc_chains WHERE objvers = 'A' AND type = 'PLSEQ' AND variante = variant-variante.

      LOOP AT pc_chains INTO pc_chain.
        unique_name            = |sap.{ pc_chain-chain_id }|.
        DATA pc_chain_id TYPE i.
        element_manager->somix_code->add(
          EXPORTING
            grouping_name_group    = ''
            grouping               = ''
            code_name_group        = 'PROCESS_CHAIN'
            code                   = pc_chain-chain_id
            technical_type         = 'PROCESS_CHAIN'
            link_to_editor         = ''
          IMPORTING
*                    exists_already_with_id =
            id                     = pc_chain_id
          CHANGING
            unique_name            = unique_name
        ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = pc_chain_id
            called_id  = variant_id
        ).

        " Add selected called logic from the process chain

        DATA pc_chains_2 TYPE STANDARD TABLE OF rspcchain WITH DEFAULT KEY.
        DATA pc_chain_2 TYPE rspcchain.

        CLEAR pc_chains_2.

        " Called logic
        SELECT * FROM rspcchain INTO TABLE pc_chains_2 WHERE chain_id = pc_chain-chain_id AND objvers = 'A' AND type = 'CHAIN'.

        LOOP AT pc_chains_2 INTO pc_chain_2.
          unique_name            = |sap.{ variant-type }_{ pc_chain_2-variante }|.
          DATA pc_chain2_id TYPE i.
          element_manager->somix_code->add(
            EXPORTING
              grouping_name_group    = ''
              grouping               = ''
              code_name_group        = 'PROCESS_CHAIN'
              code                   = pc_chain_2-variante
              technical_type         = 'PROCESS_CHAIN'
              link_to_editor         = ''
            IMPORTING
*                    exists_already_with_id =
              id                     = pc_chain2_id
            CHANGING
              unique_name            = unique_name
          ).

          call_id = element_manager->somix_call->add( ).
          element_manager->somix_call->set_caller_called_relation(
            EXPORTING
              element_id = call_id
              caller_id  = pc_chain_id
              called_id  = pc_chain2_id
          ).


        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD program_used_in_pc_chain.

    DATA variants TYPE STANDARD TABLE OF rspcvariant WITH DEFAULT KEY.
    DATA variant TYPE rspcvariant.
    DATA pc_chains TYPE STANDARD TABLE OF rspcchain WITH DEFAULT KEY.
    DATA pc_chain TYPE rspcchain.

    SELECT * FROM rspcvariant INTO TABLE variants WHERE type = 'ABAP' AND objvers = 'A' AND fnam = 'PROGRAM' AND low = program.

    LOOP AT variants INTO variant.

      " Add variant
      DATA unique_name TYPE string.
      unique_name            = |sap.{ variant-type }_{ variant-variante }|.
      DATA variant_id TYPE i.
      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = 'PROCESS_CHAIN_VARIANT'
          code                   = variant-variante
          technical_type         = 'PROCESS_CHAIN_VARIANT'
          link_to_editor         = ''
        IMPORTING
*                    exists_already_with_id =
          id                     = variant_id
        CHANGING
          unique_name            = unique_name
      ).

      DATA call_id TYPE i.
      call_id = element_manager->somix_call->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id = call_id
          caller_id  = variant_id
          called_id  = program_id
      ).

      " Get Process Chains from variant Duplicate 2 / 2

      CLEAR pc_chains.

      SELECT * FROM rspcchain INTO TABLE pc_chains WHERE objvers = 'A' AND type = 'ABAP' AND variante = variant-variante.

      LOOP AT pc_chains INTO pc_chain.
        unique_name            = |sap.{ pc_chain-chain_id }|.
        DATA pc_chain_id TYPE i.
        element_manager->somix_code->add(
          EXPORTING
            grouping_name_group    = ''
            grouping               = ''
            code_name_group        = 'PROCESS_CHAIN'
            code                   = pc_chain-chain_id
            technical_type         = 'PROCESS_CHAIN'
            link_to_editor         = ''
          IMPORTING
*                    exists_already_with_id =
            id                     = pc_chain_id
          CHANGING
            unique_name            = unique_name
        ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = pc_chain_id
            called_id  = variant_id
        ).

        " Add selected called logic from the process chain

        DATA pc_chains_2 TYPE STANDARD TABLE OF rspcchain WITH DEFAULT KEY.
        DATA pc_chain_2 TYPE rspcchain.

        CLEAR pc_chains_2.

        " Called logic
        SELECT * FROM rspcchain INTO TABLE pc_chains_2 WHERE chain_id = pc_chain-chain_id AND objvers = 'A' AND type = 'CHAIN'.

        LOOP AT pc_chains_2 INTO pc_chain_2.
          unique_name            = |sap.{ pc_chain_2-variante }|.
          DATA pc_chain2_id TYPE i.
          element_manager->somix_code->add(
            EXPORTING
              grouping_name_group    = ''
              grouping               = ''
              code_name_group        = 'PROCESS_CHAIN'
              code                   = pc_chain_2-variante
              technical_type         = 'PROCESS_CHAIN'
              link_to_editor         = ''
            IMPORTING
*                    exists_already_with_id =
              id                     = pc_chain2_id
            CHANGING
              unique_name            = unique_name
          ).

          call_id = element_manager->somix_call->add( ).
          element_manager->somix_call->set_caller_called_relation(
            EXPORTING
              element_id = call_id
              caller_id  = pc_chain_id
              called_id  = pc_chain2_id
          ).


        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
