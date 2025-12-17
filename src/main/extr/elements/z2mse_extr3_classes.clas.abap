CLASS z2mse_extr3_classes DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA extr3_planing TYPE REF TO z2mse_extr3_planing.

    CONSTANTS: is_class_type  TYPE seoclstype VALUE 0,
               interface_type TYPE seoclstype VALUE 1,
               attribute_type TYPE seocmptype VALUE 0,
               method_type    TYPE seocmptype VALUE 1,
               event_type     TYPE seocmptype VALUE 2.



    TYPES: BEGIN OF ty_class_component,
             clsname TYPE string,
             cmpname TYPE string,
             cmptype TYPE seocmptype,
             mtdtype TYPE seomtdtype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE STANDARD TABLE OF ty_class_component WITH KEY clsname cmpname.
    DATA: class_components TYPE ty_class_components.
    CLASS-METHODS clear.
    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_classes.
    METHODS add
      IMPORTING
        class                   TYPE string
        is_specific             TYPE abap_bool
      EXPORTING
        VALUE(is_added)         TYPE abap_bool
        VALUE(new_element_id)   TYPE z2mse_extr3_element_manager=>element_id_type
        VALUE(class_components) TYPE ty_class_components.
    METHODS add_component
      IMPORTING
        clsname               TYPE string
        cmpname               TYPE string
        is_specific           TYPE abap_bool
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS class_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE string
        VALUE(clstype)    TYPE seoclstype
        VALUE(exists)     TYPE abap_bool.
    METHODS comp_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE string
        VALUE(cmpname)    TYPE string
        VALUE(cmptype)    TYPE seocmptype
        VALUE(exists)     TYPE abap_bool.
    METHODS is_redefinition_of_method
      IMPORTING
        invoced_element_id1  TYPE i
        invocing_element_id2 TYPE i
      RETURNING
        VALUE(r_result)      TYPE abap_bool.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
    METHODS collect_infos REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_classes.

    TYPES: BEGIN OF element_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             class_name TYPE string,
             clstype    TYPE seoclstype,
             adt_link   TYPE string,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_class_name TYPE HASHED TABLE OF element_type WITH UNIQUE KEY class_name.

    TYPES: BEGIN OF element_comp_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             clsname    TYPE string,
             cmpname    TYPE string,
             cmptype    TYPE seocmptype,
             mtdtype    TYPE seomtdtype,
             adt_link   TYPE string,
           END OF element_comp_type.
    DATA elements_comp_element_id TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY element_id.
    DATA elements_comp_clsname_cmpname TYPE SORTED TABLE OF element_comp_type WITH UNIQUE KEY clsname cmpname.

    TYPES: BEGIN OF element_metarel_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             refclsname TYPE string,
             reltype    TYPE seoreltype,
           END OF element_metarel_type.
    DATA elements_metarel_element_id TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY element_id.
    DATA elements_metarel_refclsname TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY refclsname.

    TYPES: BEGIN OF redefined_type,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
             mtdname    TYPE seocpdname,
           END OF redefined_type.

    TYPES: BEGIN OF redefined_method_type,
             clsname            TYPE seoclsname,
             defined_in_clsname TYPE seoclsname,
             method             TYPE seocpdname,
           END OF redefined_method_type.
    DATA redefined_methods TYPE HASHED TABLE OF redefined_method_type WITH UNIQUE KEY method clsname defined_in_clsname.
    METHODS _add_component
      IMPORTING
        clsname               TYPE string
        cmpname               TYPE string
        is_specific           TYPE abap_bool
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(is_added_now)   TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.

    METHODS _add_metarel
      IMPORTING
        clsname     TYPE string
        is_specific TYPE abap_bool.

    "! Call me only after checking that the component to be added is not already added.
    METHODS _add_single_component_to_class
      IMPORTING
        i_found_class_name      TYPE string
        i_found_cmpname         TYPE string
        i_found_cmptype         TYPE seocmptype
        i_found_mtdtype         TYPE seomtdtype
        is_specific             TYPE abap_bool
      RETURNING
        VALUE(r_new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.

    METHODS _get_redefined
      IMPORTING
        class           TYPE string
      RETURNING
        VALUE(r_result) TYPE z2mse_extr3_classes=>ty_class_components.

ENDCLASS.



CLASS z2mse_extr3_classes IMPLEMENTATION.


  METHOD add.

    DATA: element      TYPE element_type,
          element_comp TYPE element_comp_type.

    READ TABLE elements_class_name INTO element WITH KEY class_name = class.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
      LOOP AT elements_comp_clsname_cmpname INTO element_comp WHERE clsname = class.
        DATA class_component TYPE z2mse_extr3_classes=>ty_class_component.
        class_component-clsname = element_comp-clsname.
        class_component-cmpname = element_comp-cmpname.
        class_component-cmptype = element_comp-cmptype.
        class_component-mtdtype = element_comp-mtdtype.
        INSERT class_component INTO TABLE class_components.
      ENDLOOP.
    ELSE.

      " Does table exists?
      DATA: found_class_name TYPE seoclsname,
            found_class_type TYPE seoclstype.

      TEST-SEAM seoclass.
        " No blank between ( and found... to be 7.02 compatible
        SELECT SINGLE clsname clstype FROM seoclass INTO (found_class_name , found_class_type ) WHERE clsname = class.
      END-TEST-SEAM.

      IF found_class_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = is_specific ).
        element-element_id = new_element_id.
        element-class_name = class.
        element-clstype = found_class_type.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_class_name.

      ENDIF.

      TEST-SEAM seocompo_2.

        SELECT clsname cmpname cmptype mtdtype
          FROM seocompo
          INTO CORRESPONDING FIELDS OF TABLE class_components
          WHERE cmptype <> 3 " A type
            AND clsname = class.

      END-TEST-SEAM.

      " Redefined methods may be in table SEOREDEF

      DATA: redefined_methods TYPE STANDARD TABLE OF seoredef WITH DEFAULT KEY,
            rm                TYPE seoredef.

      SELECT * FROM seoredef INTO TABLE redefined_methods WHERE clsname = class.
      DATA: cp TYPE ty_class_component.
      LOOP AT redefined_methods INTO rm.
        CLEAR cp.
        cp-clsname = class.
        cp-cmpname = rm-mtdname.
        cp-cmptype = 1. " Method
        cp-mtdtype = 0. " Method
        APPEND cp TO class_components.
      ENDLOOP.



      DATA: redefined_class_components TYPE ty_class_components,
            redefined_class_component  TYPE ty_class_component.

      redefined_class_components = _get_redefined( class ).

      LOOP AT redefined_class_components INTO redefined_class_component.
        INSERT redefined_class_component INTO TABLE class_components.
      ENDLOOP.

      SORT class_components.
      DELETE ADJACENT DUPLICATES FROM class_components.

      LOOP AT class_components INTO class_component.

        _add_component( EXPORTING clsname        = class_component-clsname
                                  cmpname        = class_component-cmpname
                                  is_specific    = is_specific ).

      ENDLOOP.

      _add_metarel( clsname     = class
                    is_specific = is_specific ) ##TODO. " Is it correct to set this specific?



    ENDIF.
  ENDMETHOD.


  METHOD add_component.

    DATA: is_added_now TYPE abap_bool.

    add( EXPORTING class          = clsname
                    is_specific   = is_specific
         IMPORTING is_added       = is_added ).

    IF is_added EQ abap_true.

      _add_component( EXPORTING clsname        = clsname
                                cmpname        = cmpname
                                is_specific    = is_specific
                      IMPORTING is_added       = is_added
                                new_element_id = new_element_id
                                is_added_now   = is_added_now ).

      IF is_specific EQ abap_true
      AND new_element_id IS NOT INITIAL.

        element_manager->model_builder->new_element_id( EXPORTING i_element_id  = new_element_id
                                                                  i_is_specific = abap_true ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD class_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.

    IF sy-subrc EQ 0.

      class_name = element-class_name.
      clstype = element-clstype.
      exists = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD clear.
    CLEAR instance.
  ENDMETHOD.


  METHOD collect_infos.

    DATA: part1            TYPE string,
          part2_classes    TYPE string,
          part2_interfaces TYPE string,
          name             TYPE string,
          part3            TYPE string.

    " Get ADT Link to class or interface

    CONCATENATE 'adt://' sysid '/sap/bc/adt/oo/' INTO part1.

    part2_classes = 'classes/'.

    part2_interfaces = 'interfaces/'.

    part3 = '/source/main'.

    FIELD-SYMBOLS: <element> TYPE element_type.

    LOOP AT elements_element_id ASSIGNING <element>.

      name = <element>-class_name.

      TRANSLATE name TO LOWER CASE.

      IF <element>-clstype EQ is_class_type.

        CONCATENATE part1 part2_classes name part3 INTO <element>-adt_link.

      ELSEIF <element>-clstype EQ interface_type.

        CONCATENATE part1 part2_interfaces name part3 INTO <element>-adt_link.

      ELSE.
        ASSERT 1 = 2.
      ENDIF.


      " Get ADT Link to components

      DATA: cifkey           TYPE seoclskey,
            cifref           TYPE REF TO if_oo_clif_incl_naming,
            clsref           TYPE REF TO if_oo_class_incl_naming,
            intref           TYPE REF TO if_oo_interface_incl_naming,
            source           TYPE seop_source_string,
            source_protected TYPE seop_source_string,
            source_private   TYPE seop_source_string,
            source_line      TYPE LINE OF seop_source_string.

      cifkey-clsname = <element>-class_name.

      CLEAR source.
      CLEAR source_protected.
      CLEAR source_private.

      CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
        EXPORTING
          cifkey = cifkey
        RECEIVING
          cifref = cifref
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        " :-(
      ELSE.
        CASE cifref->clstype.
          WHEN seoc_clstype_class.
            clsref ?= cifref.
            READ REPORT clsref->public_section
              INTO source.
            READ REPORT clsref->protected_section
              INTO source_protected.
            READ REPORT clsref->private_section
              INTO source_private.
          WHEN seoc_clstype_interface.
            intref ?= cifref.
            READ REPORT intref->public_section
              INTO source.
          WHEN OTHERS.
            " What is to be done?
        ENDCASE.
      ENDIF.

      APPEND LINES OF source_protected TO source.
      APPEND LINES OF source_private TO source.

      DATA: line_no TYPE i,
            line    TYPE string.

      line_no = 0.

*      LOOP AT source INTO source_line.
*        add 1 to line_no.
*        line = source_line.
*        condense line.
*        split line at ' ' into
*      ENDLOOP.
      DATA: tokens         TYPE STANDARD TABLE OF stokes,
            token          LIKE LINE OF tokens,
            next_component TYPE seocmptype,
            next_line      TYPE i.

      DATA statements TYPE STANDARD TABLE OF sstmnt.

      SCAN ABAP-SOURCE source TOKENS INTO tokens STATEMENTS INTO statements.

      LOOP AT tokens INTO token.
        ADD 1 TO line_no.
        IF line_no EQ next_line.

          DATA: element_comp TYPE element_comp_type.

          READ TABLE elements_comp_clsname_cmpname INTO element_comp
            WITH TABLE KEY
              clsname = <element>-class_name
              cmpname = token-str.

          IF sy-subrc <> 0.
            " What to do?
          ELSE.

            FIELD-SYMBOLS <element_comp> TYPE element_comp_type.

            READ TABLE elements_comp_element_id ASSIGNING <element_comp>
              WITH TABLE KEY element_id = element_comp-element_id.

            IF sy-subrc <> 0.
              " What to do?
            ELSE.

              DATA: row TYPE string.
              row = token-row.

              DATA: adt_link TYPE string.

              CONCATENATE <element>-adt_link '#start=' row ',1' INTO adt_link.

              CONDENSE adt_link NO-GAPS.

              IF <element_comp>-adt_link IS INITIAL. " Are there further hits?

                <element_comp>-adt_link = adt_link.

              ENDIF.

            ENDIF.

          ENDIF.

        ELSE.
          CLEAR next_line.
          CLEAR next_component.
          CASE token-str.
            WHEN 'CLASS-DATA' OR 'DATA'.
              next_component = attribute_type.
              next_line = line_no + 1.
            WHEN 'CLASS-METHODS' OR 'METHODS'.
              next_component = method_type.
              next_line = line_no + 1.
            WHEN 'CLASS-EVENTS' OR 'EVENTS'.
              next_component = event_type.
              next_line = line_no + 1.
          ENDCASE.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD comp_name.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.

    IF sy-subrc EQ 0.

      class_name = element_comp-clsname.
      cmpname = element_comp-cmpname.
      cmptype = element_comp-cmptype.
      exists = abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
      instance->type = class_type.
      instance->extr3_planing = z2mse_extr3_planing=>get_instance( element_manager = element_manager ).
    ENDIF.
    r_instance = instance.
  ENDMETHOD.


  METHOD is_redefinition_of_method.

    DATA: invoced  TYPE element_comp_type,
          invocing TYPE element_comp_type.

    READ TABLE elements_comp_element_id INTO invoced WITH KEY element_id = invoced_element_id1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE elements_comp_element_id INTO invocing WITH KEY element_id = invocing_element_id2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF invoced-cmpname <> invocing-cmpname.
      RETURN.
    ENDIF.

    DATA: r TYPE redefined_method_type.

    READ TABLE redefined_methods INTO r WITH TABLE KEY method = invocing-cmpname
                                                       clsname = invocing-clsname
                                                       defined_in_clsname = invoced-clsname.

    IF sy-subrc EQ 0.
      r_result = 'X'.
    ENDIF.


  ENDMETHOD.


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    IF sy-subrc EQ 0.

      DATA: last_id        TYPE i,
            class_id       TYPE i,
            file_anchor_id TYPE i.

      IF element-clstype EQ is_class_type.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
        IF element_manager->use_somix EQ 'X'.
          DATA: unique_name TYPE string.
          unique_name = |sap.{ element-class_name }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_class
                                                          grouping            = element-class_name
                                                          technical_type      = z2mse_extract3=>modifier_abapglobalclass
                                                          link_to_editor      = element-adt_link
                                                IMPORTING id                  = class_id
                                                CHANGING  unique_name         = unique_name ).

        ELSE. " SOMIX

          element_manager->famix_class->add( EXPORTING name_group = ng_abap_class
                                                       name       = element-class_name
                                                       modifiers  = z2mse_extract3=>modifier_abapglobalclass
                                             IMPORTING id         = last_id ).

          IF element-adt_link IS NOT INITIAL.

            element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                               file_name  = element-adt_link
                                                     IMPORTING id         = file_anchor_id ).

            IF file_anchor_id IS NOT INITIAL.
              element_manager->famix_class->set_source_anchor_by_id(
                EXPORTING
                  element_id         = last_id
                  source_anchor_id   = file_anchor_id
              ).

            ENDIF.

          ENDIF.

        ENDIF. " SOMIX

      ELSEIF element-clstype EQ interface_type.
        IF element_manager->use_somix EQ 'X'.

          unique_name = |sap.{ element-class_name }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group = ng_abap_class
                                                          grouping            = element-class_name
                                                          technical_type      = z2mse_extract3=>modifier_abapglobalinterface
                                                          link_to_editor      = element-adt_link
                                                IMPORTING id                  = class_id
                                                CHANGING  unique_name         = unique_name ).

        ELSE. " SOMIX
          " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
          " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
          element_manager->famix_class->add( EXPORTING name_group = ng_abap_class
                                                       name       = element-class_name
                                                       modifiers  = z2mse_extract3=>modifier_abapglobalinterface
                                             IMPORTING id         = last_id ).
          " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
          element_manager->famix_class->is_interface( element_id = last_id ).

          IF element-adt_link IS NOT INITIAL.

            element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                               file_name  = element-adt_link
                                                     IMPORTING id         = file_anchor_id ).

            IF file_anchor_id IS NOT INITIAL.
              element_manager->famix_class->set_source_anchor_by_id(
                EXPORTING
                  element_id         = last_id
                  source_anchor_id   = file_anchor_id
              ).

            ENDIF.

          ENDIF.

        ENDIF. " SOMIX

      ELSE.
        ASSERT 1 = 2.
      ENDIF.

      DATA association TYPE z2mse_extr3_element_manager=>association_type.
      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = z2mse_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO z2mse_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).

        IF element_manager->use_somix EQ 'X'.
          DATA: package_id TYPE i.
          DATA: devclass TYPE devclass.
          devclass = package->devclass( i_element_id = association-element_id2 ).
          unique_name = |sap.{ devclass }|.
          element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_package
                                                          grouping               = devclass
                                                          technical_type         = z2mse_extract3=>techtype_abappackage
                                                          link_to_editor         = ''
                                                IMPORTING id                     = package_id
                                                CHANGING  unique_name            = unique_name ).

          element_manager->somix_parentchild->add(  EXPORTING parent_id  = package_id
                                                              child_id   = class_id
                                                              is_main    = ''  ).

        ELSE. " SOMIX

          element_manager->famix_class->set_parent_package( element_id     = last_id
                                                            parent_package = package->devclass( i_element_id = association-element_id2 )
                                                            parent_package_name_group = ng_abap_package ).

        ENDIF. " SOMIX

      ENDLOOP.

    ELSE.

      DATA element_comp TYPE element_comp_type.

      READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.
      ASSERT sy-subrc EQ 0.
      CASE element_comp-cmptype.
        WHEN attribute_type.


*    DATA last_id TYPE i.!

          IF element_manager->use_somix EQ 'X'.

            unique_name = |sap.{ element_comp-clsname }.{ element_comp-cmpname }|.
            element_manager->somix_data->add( EXPORTING grouping_name_group = ng_abap_class
                                                        grouping    = element_comp-clsname
                                                        data_name_group = ng_abap_attribute ##TODO " Improve coding generally. Without grouping name the data name is not uniquw
                                                        data = element_comp-cmpname
                                                        technical_type = z2mse_extract3=>techtype_abapclassattribute
                                                        link_to_editor  = element_comp-adt_link
                                              IMPORTING id = last_id
                                              CHANGING  unique_name         = unique_name ).

            unique_name = |sap.{ element_comp-clsname }|.
            element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_class
                                                            grouping               = element_comp-clsname
                                                            technical_type         = '' " Leave unchanged
                                                            link_to_editor         = ''
                                                  IMPORTING id                     = class_id
                                                  CHANGING  unique_name            = unique_name ).
            element_manager->somix_parentchild->add( EXPORTING parent_id = class_id
                                                               child_id  = last_id
                                                               is_main   = 'X' ).

          ELSE. " SOMIX

            element_manager->famix_attribute->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).

            element_manager->famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                          parent_element = 'FAMIX.Class'
                                                          parent_name_group = ng_abap_class
                                                          parent_name    = element_comp-clsname ).

            IF element_comp-adt_link IS NOT INITIAL.

              element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                                 file_name  = element_comp-adt_link
                                                         IMPORTING id         = file_anchor_id ).

              IF file_anchor_id IS NOT INITIAL.
                element_manager->famix_attribute->set_source_anchor_by_id(
                  EXPORTING
                    element_id         = last_id
                    source_anchor_id   = file_anchor_id
                ).

              ENDIF.

            ENDIF.

            element_manager->famix_attribute->store_id( EXPORTING name_group = ng_abap_class
                                                                  class      = element_comp-clsname
                                                                  attribute  = element_comp-cmpname ).

          ENDIF. " SOMIX

*            sap_attribute->add( EXPORTING class     = class-clsname
*                                          attribute = component-cmpname ).
        WHEN method_type OR event_type.
          " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
          " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method

          IF element_manager->use_somix EQ 'X'.

            unique_name = |sap.{ element_comp-clsname }.{ element_comp-cmpname }|.
            element_manager->somix_code->add( EXPORTING grouping_name_group = ng_abap_class
                                                        grouping            = element_comp-clsname
                                                        code_name_group     = z2mse_extr3=>ng_abap_method
                                                        code                = element_comp-cmpname
                                                        technical_type      = z2mse_extract3=>techtype_abapmethod
                                                        link_to_editor      = element_comp-adt_link
                                              IMPORTING id                  = last_id
                                              CHANGING  unique_name         = unique_name ).

            unique_name = |sap.{ element_comp-clsname }|.
            element_manager->somix_grouping->add( EXPORTING grouping_name_group    = ng_abap_class
                                                            grouping               = element_comp-clsname
                                                            technical_type         = '' " Leave unchanged
                                                            link_to_editor         = '' " Leave unchanged
                                                  IMPORTING id                     = class_id
                                                  CHANGING  unique_name            = unique_name ).
            element_manager->somix_parentchild->add( EXPORTING parent_id = class_id
                                                               child_id  = last_id
                                                               is_main   = 'X' ).

            " Add usage by Planing function #140

            FIND 'IF_RSPLFA' IN element_comp-cmpname.
            IF sy-subrc = 0.
* BEGIN DELETE #140
*              DATA planing_function TYPE rsplf_srv.
*              DATA planing_functions TYPE STANDARD TABLE OF rsplf_srv WITH DEFAULT KEY.
*              CLEAR planing_functions.
*              " Find direct usages of method in planing function
*              DATA planing_function_parameters TYPE STANDARD TABLE OF rsplf_srv_p WITH DEFAULT KEY.
*              SELECT * FROM rsplf_srv_p INTO TABLE planing_function_parameters WHERE objvers = 'A' AND parnm = 'CLASS' AND value = element_comp-clsname.
*              IF sy-subrc = 0.
*                DATA pfp TYPE rsplf_srv_p.
*                LOOP AT planing_function_parameters INTO pfp.
*                  SELECT SINGLE * FROM rsplf_srv INTO planing_function WHERE objvers = 'A' AND srvnm = pfp-srvnm.
*                  IF sy-subrc = 0.
*                    APPEND planing_function TO planing_functions.
*                  ENDIF.
*                ENDLOOP.
*              ENDIF.
*              " Find usages of method in service types
*              DATA service_types TYPE STANDARD TABLE OF rsplf_srvtype WITH DEFAULT KEY.
*              DATA st TYPE rsplf_srvtype.
*              SELECT * FROM rsplf_srvtype INTO TABLE service_types WHERE objvers = 'A' AND classnm = element_comp-clsname.
*              LOOP AT service_types INTO st.
*
*                SELECT * FROM rsplf_srv INTO planing_function WHERE objvers = 'A' AND srvtypenm = st-srvtypenm.
*                  APPEND planing_function TO planing_functions.
*                ENDSELECT.
*              ENDLOOP.
*
*              SORT planing_functions.
*              DELETE ADJACENT DUPLICATES FROM planing_functions.
*
*              LOOP AT planing_functions INTO planing_function.
*                unique_name            = |sap.{ planing_function-infoprov }|.
*                DATA aggr_id TYPE i.
*                element_manager->somix_grouping->add(
*                  EXPORTING
*                    grouping_name_group    = 'AGGR_LEVEL'
*                    grouping               = planing_function-infoprov
*                    technical_type         = 'AGGR_LEVEL'
*                    link_to_editor         = ''
*                  IMPORTING
**                  exists_already_with_id =
*                    id                     = aggr_id
*                  CHANGING
*                    unique_name            = unique_name
*                ).
*                unique_name            = |sap.{ planing_function-infoprov }.{ planing_function-srvnm }|.
*                DATA planfunc_id TYPE i.
*                element_manager->somix_code->add(
*                  EXPORTING
*                    grouping_name_group    = 'AGGR_LEVEL'
*                    grouping               = planing_function-infoprov
*                    code_name_group        = 'PLANING_FUNCTION'
*                    code                   = planing_function-srvnm
*                    technical_type         = 'PLANING_FUNCTION'
*                    link_to_editor         = ''
*                  IMPORTING
**                  exists_already_with_id =
*                    id                     = planfunc_id
*                  CHANGING
*                    unique_name            = unique_name
*                ).
*                element_manager->somix_parentchild->add(
*                  EXPORTING
*                    parent_id = aggr_id
*                    child_id  = planfunc_id
*                    is_main   = 'X'
**                RECEIVING
**                  id        =
*                ).
*                DATA: call_id TYPE i.
*                call_id = element_manager->somix_call->add( ).
*                element_manager->somix_call->set_caller_called_relation(
*                  EXPORTING
*                    element_id = call_id
*                    caller_id  = planfunc_id
*                    called_id  = last_id
*                ).
*
*
*              ENDLOOP.
* BEGIN REPLACE #140

              extr3_planing->class_used_in_planing(
                     clsname = element_comp-clsname
                     method_id = last_id
               ).

* END REPLACE #140

            ENDIF.

            " End Insert Planing Function

          ELSE. " SOMIX

            element_manager->famix_method->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).

            " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
            " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
            element_manager->famix_method->set_signature( element_id = last_id
                                           signature = element_comp-cmpname ).

            element_manager->famix_method->set_parent_type( EXPORTING element_id = last_id
                                                       parent_element = 'FAMIX.Class'
                                                       parent_name_group = ng_abap_class
                                                       parent_name    = element_comp-clsname ).

            IF element_comp-adt_link IS NOT INITIAL.

              element_manager->famix_file_anchor->add( EXPORTING element_id = last_id " Required for Moose 6.1
                                                                 file_name  = element_comp-adt_link
                                                         IMPORTING id         = file_anchor_id ).

              IF file_anchor_id IS NOT INITIAL.
                element_manager->famix_method->set_source_anchor_by_id(
                  EXPORTING
                    element_id         = last_id
                    source_anchor_id   = file_anchor_id
                ).

              ENDIF.

            ENDIF.

            element_manager->famix_method->store_id( EXPORTING class_name_group = ng_abap_class
                                                               class  = element_comp-clsname
                                                               method_name_group = ng_abap_method
                                                               method = element_comp-cmpname ).

          ENDIF. " SOMIX


*            sap_method->add( EXPORTING class  = class-clsname
*                                       method = component-cmpname ).
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD name.
    DATA: class_name TYPE string,
          clstype    TYPE seoclstype,
          exists     TYPE abap_bool.

    class_name( EXPORTING element_id = element_id
            IMPORTING class_name =  class_name
                      clstype    = clstype
                      exists     = exists ).

    IF exists EQ abap_true.

      CASE clstype.
        WHEN is_class_type.
          element_type = |ABAPClass|.
        WHEN interface_type.
          element_type = |ABAPInterface|.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      parent_name = ||.
      name = class_name.

    ELSE.

      DATA: cmpname TYPE string,
            cmptype TYPE seocmptype.

      comp_name( EXPORTING element_id = element_id
                   IMPORTING class_name = class_name
                             cmpname    = cmpname
                             cmptype    = cmptype
                             exists     = exists ).

      ASSERT exists EQ abap_true.

      DATA element TYPE element_type.

      READ TABLE elements_class_name INTO element WITH KEY class_name = class_name.
      ASSERT sy-subrc EQ 0.
      clstype = element-clstype.

      CASE clstype.
        WHEN is_class_type.
          CASE cmptype.
            WHEN attribute_type.
              element_type = |ABAPClassAttribute|.
            WHEN method_type.
              element_type = |ABAPClassMethod|.
            WHEN event_type.
              element_type = |ABAPClassEvent|.
            WHEN OTHERS.
              ASSERT 1 = 2.
          ENDCASE.
        WHEN interface_type.
          CASE cmptype.
            WHEN attribute_type.
              element_type = |ABAPInterfaceAttribute|.
            WHEN method_type.
              element_type = |ABAPInterfaceMethod|.
            WHEN event_type.
              element_type = |ABAPInterfaceEvent|.
            WHEN OTHERS.
              ASSERT 1 = 2.
          ENDCASE.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      parent_name = class_name.
      name = cmpname.

    ENDIF.

  ENDMETHOD.


  METHOD _add_component.

    DATA element_comp TYPE element_comp_type.

    READ TABLE elements_comp_clsname_cmpname INTO element_comp WITH KEY clsname  = clsname cmpname = cmpname.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element_comp-element_id.
    ELSE.

      " Does component exists?
      DATA: found_class_name TYPE string,
            found_cmpname    TYPE string,
            found_cmptype    TYPE seocmptype,
            found_mtdtype    TYPE seomtdtype.

      TEST-SEAM seocompo.
        SELECT SINGLE clsname cmpname cmptype mtdtype FROM seocompo
          INTO (found_class_name, found_cmpname, found_cmptype, found_mtdtype ) WHERE clsname = clsname
                                                                                   AND cmpname = cmpname.
      END-TEST-SEAM.

      IF found_class_name IS INITIAL.
        " Is it a redefined method?

        DATA: redefined_methods TYPE STANDARD TABLE OF seoredef WITH DEFAULT KEY,
              rm                TYPE seoredef.

        SELECT * FROM seoredef INTO TABLE redefined_methods WHERE clsname = clsname
                                                              AND mtdname = cmpname.

        LOOP AT redefined_methods INTO rm.
          found_class_name = clsname.
          found_cmpname = rm-mtdname.
          found_cmptype = 1. " Method
          found_mtdtype = 0. " Method
          " A method name has to be unique, so assume only a single entry here
          EXIT.
        ENDLOOP.

      ENDIF.

      IF found_class_name IS NOT INITIAL.

        is_added = abap_true.

        IF found_cmptype EQ 3. " Is type
          RAISE EXCEPTION TYPE zcx_2mse_extr3_classes_wr_type.
        ENDIF.

      ELSE.

        DATA: redefined_class_components TYPE ty_class_components,
              redefined_class_component  TYPE ty_class_component.

        redefined_class_components = _get_redefined( clsname ).

        READ TABLE redefined_class_components INTO redefined_class_component WITH KEY clsname = clsname cmpname = cmpname.

        IF sy-subrc EQ 0.

          found_class_name = redefined_class_component-clsname.
          found_cmpname = redefined_class_component-cmpname.
          found_cmptype = redefined_class_component-cmptype.
          found_mtdtype = redefined_class_component-mtdtype.

          is_added = abap_true.

        ENDIF.

      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = _add_single_component_to_class( i_found_class_name = found_class_name
                                                         i_found_cmpname    = found_cmpname
                                                         i_found_cmptype    = found_cmptype
                                                         i_found_mtdtype    = found_mtdtype
                                                         is_specific        = is_specific ).
        is_added_now = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD _add_metarel.

    DATA: relation        TYPE seometarel,
          relations       TYPE STANDARD TABLE OF seometarel,
          element_metarel TYPE element_metarel_type,
          is_added        TYPE abap_bool,
          new_element_id  TYPE i.

    DATA access TYPE REF TO z2mse_extr3_access.
    access = z2mse_extr3_access=>get_instance( i_element_manager = element_manager ).

    DATA invocation TYPE REF TO z2mse_extr3_invocation.
    invocation = z2mse_extr3_invocation=>get_instance( i_element_manager = element_manager ).

    TEST-SEAM seometarel.

      SELECT * FROM seometarel INTO TABLE relations WHERE clsname = clsname
                                                      AND version = 1
                                                      AND state   = 1.

    END-TEST-SEAM.

    LOOP AT relations INTO relation WHERE reltype = 1 OR reltype = 2.

      element_metarel-element_id = new_element_id.
      element_metarel-refclsname = relation-refclsname.
      element_metarel-reltype = relation-reltype.

      INSERT element_metarel INTO TABLE elements_metarel_element_id.
      INSERT element_metarel INTO TABLE elements_metarel_refclsname.

      IF relation-reltype EQ 1. " Interface

        DATA: interface_class_components TYPE ty_class_components,
              interface_class_component  TYPE ty_class_component,
              reclsname_string           TYPE string.

        reclsname_string = relation-refclsname.

        me->add( EXPORTING class            = reclsname_string
                           is_specific      = is_specific
                 IMPORTING is_added         = is_added
                           new_element_id   = new_element_id
                           class_components = interface_class_components ).

        LOOP AT interface_class_components INTO interface_class_component.

          new_element_id = _add_single_component_to_class( i_found_class_name = clsname
                                                           i_found_cmpname    = |{ interface_class_component-clsname }~{ interface_class_component-cmpname }|
                                                           i_found_cmptype    = interface_class_component-cmptype
                                                           i_found_mtdtype    = interface_class_component-mtdtype
                                                           is_specific        = is_specific  ).

          DATA interface_element_id TYPE z2mse_extr3_element_manager=>element_id_type .

          me->add_component( EXPORTING clsname        = interface_class_component-clsname
                                       cmpname        = interface_class_component-cmpname
                                       is_specific    = is_specific
                              IMPORTING "*              is_added       =
                                        new_element_id = interface_element_id ).

          IF interface_class_component-cmptype EQ attribute_type.

            " Connections between attributes are not expressible in FAMIX, or?

*            access->add( EXPORTING accessed_element_id1  = new_element_id
*                                   accessing_element_id2 = interface_element_id ).

          ELSE.

            IF element_manager->interface_use_structure EQ abap_true.

              invocation->add( EXPORTING invoced_element_id1  = interface_element_id
                                         invocing_element_id2 = new_element_id ).

            ELSE.

              invocation->add( EXPORTING invoced_element_id1  = new_element_id
                                         invocing_element_id2 = interface_element_id ).

            ENDIF.

          ENDIF.


        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _add_single_component_to_class.

    " Add single component to class

    DATA element_comp2 TYPE element_comp_type.

    ASSERT i_found_cmpname IS NOT INITIAL.

    r_new_element_id = element_manager->add_element( element = me
                                                     is_specific = is_specific ).
    element_comp2-element_id = r_new_element_id.
    element_comp2-clsname = i_found_class_name.
    element_comp2-cmpname = i_found_cmpname.
    element_comp2-cmptype = i_found_cmptype.
    element_comp2-mtdtype = i_found_mtdtype.

    INSERT element_comp2 INTO TABLE elements_comp_element_id .
    INSERT element_comp2 INTO TABLE elements_comp_clsname_cmpname .

  ENDMETHOD.


  METHOD _get_redefined.

    DATA: redefined_components TYPE STANDARD TABLE OF redefined_type WITH DEFAULT KEY,
          redefined_component  TYPE redefined_type,
          found                TYPE abap_bool,
          superclass           TYPE seoclsname,
          component            TYPE z2mse_extr3_classes=>ty_class_component.

    TEST-SEAM seoredef.

      SELECT * FROM  seoredef INTO CORRESPONDING FIELDS OF TABLE redefined_components
        WHERE clsname = class
          AND version = 1.

    END-TEST-SEAM.

    LOOP AT redefined_components INTO redefined_component.
      superclass = redefined_component-refclsname.
      found = ''.
      WHILE found EQ ''.

        CLEAR component.

        TEST-SEAM seocompo_3.

          SELECT SINGLE clsname cmpname cmptype mtdtype
            FROM seocompo
            INTO component
            WHERE cmptype <> 3 " A type
              AND clsname = superclass
              AND cmpname = redefined_component-mtdname.

        END-TEST-SEAM.

        IF sy-subrc EQ 0.
          found = 'X'.


          DATA redefined_method TYPE redefined_method_type.
          redefined_method-method = component-cmpname.
          redefined_method-clsname = class.
          redefined_method-defined_in_clsname = component-clsname.

          INSERT redefined_method INTO TABLE redefined_methods. "Allow duplicate inserting here

          component-clsname = class .
          INSERT component INTO TABLE r_result.
        ELSE.

          " Find next superclass

          TEST-SEAM seometarel_2.

            SELECT SINGLE refclsname FROM seometarel INTO superclass
              WHERE clsname = superclass
                AND version = 1.

          END-TEST-SEAM.

          IF sy-subrc <> 0.
            found = 'X'.
            " Nothing found
          ENDIF.

        ENDIF.

      ENDWHILE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
