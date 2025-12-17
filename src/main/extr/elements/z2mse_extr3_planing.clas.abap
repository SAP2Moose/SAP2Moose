CLASS z2mse_extr3_planing DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_planing.
    METHODS class_used_in_planing
      IMPORTING
        clsname   TYPE string
        method_id TYPE i.
    METHODS plan_func_used_in_plan_seq
      IMPORTING
        planfunc    TYPE clike
        planfunc_id TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_planing.
    DATA extr3_process_chain TYPE REF TO z2mse_extr3_process_chain.
    DATA extr3_bw_query TYPE REF TO z2mse_extr3_bw_query.
    DATA extr3_bw_infoprovider TYPE REF TO z2mse_extr3_bw_infoprovider.
ENDCLASS.



CLASS z2mse_extr3_planing IMPLEMENTATION.

  METHOD class_used_in_planing.
    DATA planing_function TYPE rsplf_srv.
    DATA planing_functions TYPE STANDARD TABLE OF rsplf_srv WITH DEFAULT KEY.
    CLEAR planing_functions.
    " Find direct usages of method in planing function
    DATA planing_function_parameters TYPE STANDARD TABLE OF rsplf_srv_p WITH DEFAULT KEY.
    SELECT * FROM rsplf_srv_p INTO TABLE planing_function_parameters WHERE objvers = 'A' AND parnm = 'CLASS' AND value = clsname.
    IF sy-subrc = 0.
      DATA pfp TYPE rsplf_srv_p.
      LOOP AT planing_function_parameters INTO pfp.
        SELECT SINGLE * FROM rsplf_srv INTO planing_function WHERE objvers = 'A' AND srvnm = pfp-srvnm.
        IF sy-subrc = 0.
          APPEND planing_function TO planing_functions.
        ENDIF.
      ENDLOOP.
    ENDIF.
    " Find usages of method in service types
    DATA service_types TYPE STANDARD TABLE OF rsplf_srvtype WITH DEFAULT KEY.
    DATA st TYPE rsplf_srvtype.
    SELECT * FROM rsplf_srvtype INTO TABLE service_types WHERE objvers = 'A' AND classnm = clsname.
    LOOP AT service_types INTO st.

      SELECT * FROM rsplf_srv INTO planing_function WHERE objvers = 'A' AND srvtypenm = st-srvtypenm.
        APPEND planing_function TO planing_functions.
      ENDSELECT.
    ENDLOOP.

    SORT planing_functions.
    DELETE ADJACENT DUPLICATES FROM planing_functions.

    LOOP AT planing_functions INTO planing_function.
      DATA unique_name TYPE string.
      " Keep this to prevent that old diagrams break:
      unique_name            = |sap.{ planing_function-infoprov }.{ planing_function-srvnm }|.
      DATA planfunc_id TYPE i.
      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = 'PLANING_FUNCTION'
          code                   = planing_function-srvnm
          technical_type         = 'PLANING_FUNCTION'
          link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/plse/{ planing_function-srvnm }/a|
        IMPORTING
*                  exists_already_with_id =
          id                     = planfunc_id
        CHANGING
          unique_name            = unique_name
      ).

*                element_manager->somix_parentchild->add(
*                  EXPORTING
*                    parent_id = aggr_id
*                    child_id  = planfunc_id
*                    is_main   = 'X'
**                RECEIVING
**                  id        =
*                ).
      DATA: call_id TYPE i.
      call_id = element_manager->somix_call->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id = call_id
          caller_id  = planfunc_id
          called_id  = method_id
      ).

      IF planing_function-infoprov IS NOT INITIAL.

        " Duplicate 1 / 2 - Aggregation Level
        unique_name            = |sap.{ planing_function-infoprov }|.
        DATA aggr_id TYPE i.
        element_manager->somix_code->add(
          EXPORTING
            grouping_name_group    = ''
            grouping               = ''
            code_name_group        = 'AGGR_LEVEL'
            code                   = planing_function-infoprov
            technical_type         = 'AGGR_LEVEL'
            link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/alvl/{ planing_function-infoprov }/m|
          IMPORTING
*                    exists_already_with_id =
            id                     = aggr_id
          CHANGING
            unique_name            = unique_name
        ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = planfunc_id
            called_id  = aggr_id
        ).

        plan_func_used_in_plan_seq( planfunc    = planing_function-srvnm
                                    planfunc_id = planfunc_id ).

        " Find provider of aggregation level Duplicate 1/2

        DATA: aggregation_level TYPE rspls_alvl.

        CLEAR aggregation_level.

        SELECT SINGLE * FROM rspls_alvl INTO aggregation_level WHERE objvers = 'A' AND aggrlevel = planing_function-infoprov.
        DATA infoprov_id TYPE i.
        DATA infoprovider_is_data TYPE abap_bool.

        extr3_bw_infoprovider->add_infoprovider(
          EXPORTING
            infoprovider    = aggregation_level-infoprov
          IMPORTING
            infoprovider_id = infoprov_id
            is_data         = infoprovider_is_data
        ).
        " In most cases is an infoprovider only a view to data which is modelled as code
        IF infoprovider_is_data = ''.
          call_id = element_manager->somix_call->add( ).
          element_manager->somix_call->set_caller_called_relation(
            EXPORTING
              element_id = call_id
              caller_id  = aggr_id
              called_id  = infoprov_id
          ).

        ELSE.
          DATA access_id TYPE i.
          access_id = element_manager->somix_access->add( ).
          element_manager->somix_access->set_accessor_accessed_relation(
            EXPORTING
              element_id   = access_id
              accessor_id  = aggr_id
              accessed_id  = infoprov_id
              is_write     = 'X'
              is_read      = 'X'
              is_dependent = 'X'
          ).
        ENDIF.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.

  METHOD plan_func_used_in_plan_seq.

    DATA planing_sequence_steps TYPE STANDARD TABLE OF rspls_sequence_s WITH DEFAULT KEY.
    DATA planing_sequence_step TYPE rspls_sequence_s.

    SELECT * FROM rspls_sequence_s INTO TABLE planing_sequence_steps WHERE objvers = 'A' AND srvnm = planfunc.

    LOOP AT planing_sequence_steps INTO planing_sequence_step.

      DATA unique_name TYPE string.
      unique_name            = |sap.{ planing_sequence_step-seqnm }.{ planing_sequence_step-stepid }|.
      DATA plseq_step_id TYPE i.
      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = 'PLANING_SEQUENCE_STEP'
          code                   = |{ planing_sequence_step-seqnm }-{ planing_sequence_step-stepid }|
          technical_type         = 'PLANING_SEQUENCE_STEP'
          link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/plsq/{ planing_sequence_step-seqnm }/a|
        IMPORTING
*                    exists_already_with_id =
          id                     = plseq_step_id
        CHANGING
          unique_name            = unique_name
      ).

      DATA call_id TYPE i.
      call_id = element_manager->somix_call->add( ).
      element_manager->somix_call->set_caller_called_relation(
        EXPORTING
          element_id = call_id
          caller_id  = plseq_step_id
          called_id  = planfunc_id
      ).

      " Add Filter

      IF planing_sequence_step-selobj IS NOT INITIAL.

        DATA bw_filter_id TYPE i.

        bw_filter_id = extr3_bw_query->add_bw_filter( bw_filter    = planing_sequence_step-selobj
                                                      infoprovider = planing_sequence_step-aggrlevel ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = plseq_step_id
            called_id  = bw_filter_id
        ).

        " Add Aggregation Level

        " Duplicate 2 / 2 - Aggregation Level
        unique_name            = |sap.{ planing_sequence_step-aggrlevel }|.
        DATA aggr_id TYPE i.
        element_manager->somix_code->add(
          EXPORTING
            grouping_name_group    = ''
            grouping               = ''
            code_name_group        = 'AGGR_LEVEL'
            code                   = planing_sequence_step-aggrlevel
            technical_type         = 'AGGR_LEVEL'
            link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/alvl/{ planing_sequence_step-aggrlevel }/m|
          IMPORTING
*                    exists_already_with_id =
            id                     = aggr_id
          CHANGING
            unique_name            = unique_name
        ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = plseq_step_id
            called_id  = aggr_id
        ).

        " Add Planing Sequence
        unique_name            = |sap.{ planing_sequence_step-seqnm }|.
        DATA plseq_id TYPE i.
        element_manager->somix_code->add(
          EXPORTING
            grouping_name_group    = ''
            grouping               = ''
            code_name_group        = 'PLANING_SEQUENCE'
            code                   = planing_sequence_step-seqnm
            technical_type         = 'PLANING_SEQUENCE'
            link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/plsq/{ planing_sequence_step-seqnm }/a|
          IMPORTING
*                    exists_already_with_id =
            id                     = plseq_id
          CHANGING
            unique_name            = unique_name
        ).

        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = plseq_id
            called_id  = plseq_step_id
        ).

        extr3_process_chain->plan_seq_used_in_pc_chain(
          EXPORTING
            seqnm    = planing_sequence_step-seqnm
            plseq_id = plseq_id
        ).


      ENDIF.


    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
      instance->extr3_process_chain = z2mse_extr3_process_chain=>get_instance( element_manager = element_manager ).
      instance->extr3_bw_infoprovider = z2mse_extr3_bw_infoprovider=>get_instance( element_manager = element_manager ).
      instance->extr3_bw_query = z2mse_extr3_bw_query=>get_instance( element_manager = element_manager ).
    ENDIF.
    r_instance = instance.
  ENDMETHOD.

ENDCLASS.
