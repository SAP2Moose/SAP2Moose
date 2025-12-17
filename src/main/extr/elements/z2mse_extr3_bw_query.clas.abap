CLASS z2mse_extr3_bw_query DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_bw_query.
    METHODS add_bw_filter
      IMPORTING bw_filter           TYPE clike
                infoprovider        TYPE clike
      RETURNING VALUE(bw_filter_id) TYPE i.
    METHODS add_variable
      IMPORTING variable_eltuid    TYPE clike
      RETURNING VALUE(variable_id) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_bw_query.
    DATA extr3_bw_infoprovider TYPE REF TO z2mse_extr3_bw_infoprovider.
ENDCLASS.



CLASS z2mse_extr3_bw_query IMPLEMENTATION.
  METHOD get_instance.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
      instance->extr3_bw_infoprovider = z2mse_extr3_bw_infoprovider=>get_instance( element_manager = element_manager ).
    ENDIF.
    r_instance = instance.

  ENDMETHOD.

  METHOD add_bw_filter.

    DATA unique_name TYPE string.
    unique_name            = |sap.{ bw_filter }|.
    element_manager->somix_code->add(
      EXPORTING
        grouping_name_group    = ''
        grouping               = ''
        code_name_group        = 'BW_FILTER'
        code                   = |{ bw_filter }|
        technical_type         = 'BW_FILTER'
        link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/filter/{ bw_filter }/a|
      IMPORTING
        id                     = bw_filter_id
      CHANGING
        unique_name            = unique_name
    ).

    " Find filter range

    DATA bw_filter_h_s TYPE STANDARD TABLE OF rsrrepdir WITH DEFAULT KEY.
    DATA bw_filter_h TYPE rsrrepdir.

    SELECT * FROM rsrrepdir INTO TABLE bw_filter_h_s WHERE objvers = 'A' AND infocube = infoprovider AND compid = bw_filter.

    LOOP AT bw_filter_h_s INTO bw_filter_h.

      " Find xref

      DATA eltxrefs TYPE STANDARD TABLE OF rszeltxref WITH DEFAULT KEY.
      DATA eltxref TYPE rszeltxref.

      CLEAR eltxrefs.

      SELECT * FROM rszeltxref INTO TABLE eltxrefs WHERE seltuid = bw_filter_h-compuid AND objvers = 'A'.

      LOOP AT eltxrefs INTO eltxref.

        " Analyze range

        DATA ranges TYPE STANDARD TABLE OF rszrange WITH DEFAULT KEY.
        DATA range TYPE rszrange.

        CLEAR ranges.

        SELECT * FROM rszrange INTO TABLE ranges WHERE eltuid = eltxref-teltuid AND objvers = 'A'.

        LOOP AT ranges INTO range.

          IF range-iobjnm = '0INFOPROV' AND range-lowflag = 1. " Value

            DATA infoprovider_id TYPE i.

            CLEAR infoprovider_id.
            DATA: infoprovider_is_data TYPE abap_bool.
            extr3_bw_infoprovider->add_infoprovider( EXPORTING infoprovider    = range-low
                                                     IMPORTING infoprovider_id = infoprovider_id
                                                               is_data         = infoprovider_is_data ).
            " In most cases is an infoprovider only a view to data which is modelled as code
            IF infoprovider_is_data = ''.
              DATA call_id TYPE i.
              call_id = element_manager->somix_call->add( ).
              element_manager->somix_call->set_caller_called_relation(
                EXPORTING
                  element_id = call_id
                  caller_id  = bw_filter_id
                  called_id  = infoprovider_id
              ).

            ELSE.
              DATA access_id TYPE i.
              access_id = element_manager->somix_access->add( ).
              element_manager->somix_access->set_accessor_accessed_relation(
                EXPORTING
                  element_id   = access_id
                  accessor_id  = bw_filter_id
                  accessed_id  = infoprovider_id
                  is_write     = 'X'
                  is_read      = 'X'
                  is_dependent = 'X'
              ).
            ENDIF.

          ENDIF.

          IF range-lowflag = 3. " Variable CIN

            DATA variable_id TYPE i.

            variable_id = add_variable( variable_eltuid = range-low ).

            call_id = element_manager->somix_call->add( ).
            element_manager->somix_call->set_caller_called_relation(
              EXPORTING
                element_id = call_id
                caller_id  = bw_filter_id
                called_id  = variable_id
            ).

          ENDIF.


        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_variable.

    DATA: variable TYPE rszglobv.

    SELECT SINGLE * FROM rszglobv INTO variable WHERE varuniid = variable_eltuid AND objvers = 'A'.

    IF sy-subrc = 0.

      DATA unique_name TYPE string.
      unique_name            = |sap.{ variable-vnam }|.

      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = |BW_VARIABLE|
          code                   = |{ variable-vnam }|
          technical_type         = |BW_VARIABLE|
          link_to_editor         = |bwmt://{ sy-sysid }/sap/bw/modeling/variable/{ variable-vnam }/a|
        IMPORTING
          id                     = variable_id
        CHANGING
          unique_name            = unique_name ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
