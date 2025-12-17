CLASS z2mse_extr3_bw_infoprovider DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_bw_infoprovider.
    METHODS add_infoprovider
      IMPORTING infoprovider    TYPE clike
      EXPORTING infoprovider_id TYPE i
                is_data         TYPE abap_bool.
    METHODS add_hcpr_components
      IMPORTING infoprovider    TYPE clike
                infoprovider_id TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_bw_infoprovider.
ENDCLASS.



CLASS z2mse_extr3_bw_infoprovider IMPLEMENTATION.
  METHOD get_instance.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
    ENDIF.
    r_instance = instance.

  ENDMETHOD.

  METHOD add_infoprovider.

    CLEAR infoprovider_id.
    CLEAR is_data.

    DATA infoprovider_directory TYPE tadir.
    DATA infoprovider_directories TYPE STANDARD TABLE OF tadir.
    DATA: infoprovider_object_type TYPE trobjtype.
    data link_to_editor TYPE string.

    SELECT * FROM tadir INTO TABLE infoprovider_directories WHERE obj_name = infoprovider.

    DATA: lines TYPE i.

    lines = lines( infoprovider_directories ).

    IF lines = 0.
      CLEAR infoprovider_object_type.
    ELSEIF lines = 1.
      LOOP AT infoprovider_directories INTO infoprovider_directory.
        infoprovider_object_type = infoprovider_directory-object.
      ENDLOOP.
    ELSE.
      LOOP AT infoprovider_directories INTO infoprovider_directory.
        CASE infoprovider_directory-object.
          WHEN 'ADSO'.
            infoprovider_object_type = 'ADSO'.
            link_to_editor = |bwmt://{ sy-sysid }/sap/bw/modeling/adso/{ infoprovider }/m|.
          WHEN 'HCPR'.
            infoprovider_object_type = 'HCPR'.
            link_to_editor = |bwmt://{ sy-sysid }/sap/bw/modeling/hcpr/{ infoprovider }/m|.
        ENDCASE.
      ENDLOOP.
      IF infoprovider_object_type IS INITIAL.
        infoprovider_object_type = 'XXXX'.
      ENDIF.
    ENDIF.

    DATA unique_name TYPE string.
    unique_name            = |sap.{ infoprovider }|.
    DATA inprovider_type TYPE string.
    inprovider_type = |BW_INFOPROVIDER_{ infoprovider_object_type }|.

    IF is_data = ''. " This is currently the default

      element_manager->somix_code->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          code_name_group        = inprovider_type
          code                   = |{ infoprovider }|
          technical_type         = inprovider_type
          link_to_editor         = link_to_editor
        IMPORTING
          id                     = infoprovider_id
        CHANGING
          unique_name            = unique_name ).

      IF infoprovider_object_type = 'HCPR'.
        add_hcpr_components( EXPORTING infoprovider    = infoprovider
                                       infoprovider_id = infoprovider_id ).
      ENDIF.

    ELSE.

      element_manager->somix_data->add(
        EXPORTING
          grouping_name_group    = ''
          grouping               = ''
          data_name_group        = inprovider_type
          data                   = |{ infoprovider }|
          technical_type         = inprovider_type
          link_to_editor         = link_to_editor
        IMPORTING
          id                     = infoprovider_id
        CHANGING
          unique_name            = unique_name ).

    ENDIF.

  ENDMETHOD.

  METHOD add_hcpr_components.

    TYPES:
      BEGIN OF ty_hcp_mapping,
        provider TYPE rsdodsobject,
        source   TYPE rsdiobjnm,
        target   TYPE rsohcprcolnm,
      END OF ty_hcp_mapping .
    TYPES:
      ty_hcp_mappings TYPE STANDARD TABLE OF ty_hcp_mapping WITH DEFAULT KEY .

    DATA: mapping_as_xml TYPE xstring,
          parsed_xml     TYPE STANDARD TABLE OF smum_xmltb,
          return         TYPE STANDARD TABLE OF bapiret2,
          cvalue         TYPE char255,
          offset         TYPE i,
          mapping        TYPE ty_hcp_mapping,
          mappings       TYPE ty_hcp_mappings.

    SELECT SINGLE xml_ui FROM rsohcpr INTO mapping_as_xml WHERE hcprnm = infoprovider AND objvers = 'A'.
    ASSERT sy-subrc EQ 0.

    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = mapping_as_xml
      TABLES
        xml_table = parsed_xml
        return    = return.

    FIELD-SYMBOLS: <xml> TYPE smum_xmltb.

    LOOP AT parsed_xml ASSIGNING <xml>.
      IF <xml>-cname = 'entity'.
        cvalue = <xml>-cvalue.
        SEARCH cvalue FOR 'composite'.
        IF sy-subrc EQ 0.
          offset = sy-fdpos.
          offset = offset - 1.
          IF offset EQ 0.
            ASSERT 1 = 1.
          ENDIF.
          mapping-provider = <xml>-cvalue(offset). "CompositeProvider
        ENDIF.

      ELSEIF
       <xml>-cname = 'targetName'.
        " A simple hack to get only the providers
*        mapping-target = <xml>-cvalue.
      ELSEIF
        <xml>-cname = 'sourceName'.
        " A simple hack to get only the providers
*        mapping-source = <xml>-cvalue.
        APPEND mapping TO mappings.
      ENDIF.
    ENDLOOP.

    " A simple hack to get only the providers
    SORT mappings.
    DELETE ADJACENT DUPLICATES FROM mappings.

    LOOP AT mappings INTO mapping.

      DATA: part_provider_id      TYPE i,
            part_provider_is_data TYPE abap_bool.

      add_infoprovider(
        EXPORTING
          infoprovider    = mapping-provider
        IMPORTING
          infoprovider_id = part_provider_id
          is_data         = part_provider_is_data ).

      IF part_provider_is_data = ''.
        DATA call_id TYPE i.
        call_id = element_manager->somix_call->add( ).
        element_manager->somix_call->set_caller_called_relation(
          EXPORTING
            element_id = call_id
            caller_id  = infoprovider_id
            called_id  = part_provider_id
        ).

      ELSE.
        DATA access_id TYPE i.
        access_id = element_manager->somix_access->add( ).
        element_manager->somix_access->set_accessor_accessed_relation(
          EXPORTING
            element_id   = access_id
            accessor_id  = infoprovider_id
            accessed_id  = part_provider_id
            is_write     = 'X'
            is_read      = 'X'
            is_dependent = 'X'
        ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
