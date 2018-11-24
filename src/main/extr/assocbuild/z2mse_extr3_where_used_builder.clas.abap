CLASS z2mse_extr3_where_used_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_dynamic_read
      IMPORTING
        !i_dynamic_read TYPE string OPTIONAL .

    METHODS search_down
        REDEFINITION .
    METHODS search_up
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF wbcrossgt_type,
        otype    TYPE char2,
        name     TYPE eu_lname,
        include  TYPE programm,
        direct   TYPE sgrade,
        indirect TYPE sgrade,
      END OF wbcrossgt_type ,
      wbcrossgts_type TYPE SORTED TABLE OF wbcrossgt_type WITH UNIQUE KEY otype name include,

      BEGIN OF cross_type,
        type    TYPE char1,
        name    TYPE seu_name,
        include TYPE syrepid,
      END OF cross_type,
      cross_types TYPE STANDARD TABLE OF cross_type WITH DEFAULT KEY.

    DATA: g_dynamic_usage TYPE SORTED TABLE OF wbcrossgt WITH UNIQUE KEY otype name include.
ENDCLASS.



CLASS z2mse_extr3_where_used_builder IMPLEMENTATION.


  METHOD search_down.

    DATA: element TYPE REF TO z2mse_extr3_elements.

    element = element_manager->get_element( element_id ).

    DATA invocation TYPE REF TO z2mse_extr3_invocation.
    invocation = z2mse_extr3_invocation=>get_instance( i_element_manager = element_manager ).

    CASE element->type.
      WHEN element->class_type.
        "TBD
      WHEN element->table_type.
        "Is this needed?
      WHEN element->program_type.

        " Duplicate coding 1/2 see method search_up

        DATA programs2 TYPE REF TO z2mse_extr3_programs.
        programs2 = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).

        DATA: program             TYPE progname,
              program_type        TYPE string,
              program_attribute_2 TYPE string.



        programs2->program_name(
          EXPORTING
            i_element_id                 = element_id
          IMPORTING
            program                      = program
            program_type                 = program_type
            program_attribute_2          = program_attribute_2
*            external_program_name_class  =
*            external_program_name_method =
*            subc                         =
        ).

        DATA: include_name TYPE syrepid.
        CASE program_type.
          WHEN programs2->type_program.

            include_name = program.

          WHEN programs2->type_function.

            include_name = program_attribute_2.

          WHEN OTHERS.
            " TBD
        ENDCASE.

    ENDCASE.

    IF include_name IS NOT INITIAL.

      DATA: wbcrossgts TYPE wbcrossgts_type,
            wbcrossgt  TYPE wbcrossgt_type,
            cross      TYPE cross_type,
            crosss     TYPE cross_types.

      SELECT otype name include direct indirect
        FROM wbcrossgt
        INTO TABLE wbcrossgts
        WHERE include = include_name.

      SELECT type name include FROM cross INTO TABLE crosss
        WHERE include = include_name.

      " Read dynamic usages

      DATA: w TYPE wbcrossgt.
      LOOP AT g_dynamic_usage INTO w
        WHERE include = include_name.
        MOVE-CORRESPONDING w TO wbcrossgt.
        INSERT wbcrossgt INTO TABLE wbcrossgts.
      ENDLOOP.

      LOOP AT crosss INTO cross.

        DATA: is_added        TYPE abap_bool,
              uses_element_id TYPE z2mse_extr3_element_manager=>element_id_type.

        DATA programs TYPE REF TO z2mse_extr3_programs.
        programs = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).

        IF cross-type EQ 'R'.
          " SAP_2_FAMIX_67 Provide downsearch for programs - list found programs
          DATA: program_found  TYPE progname.
          program_found = cross-name.
          programs->add( EXPORTING program        = program_found
                         IMPORTING is_added       = is_added
                                   new_element_id = uses_element_id ).

          invocation->add( EXPORTING invoced_element_id1  = uses_element_id
                                     invocing_element_id2 = element_id ).

          CLEAR is_added.

        ELSEIF cross-type EQ 'F'.
          " TBD functions
        ELSE.
          " TBD ?
        ENDIF.

      ENDLOOP.

      LOOP AT wbcrossgts INTO wbcrossgt.

        CLEAR is_added.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD search_up.

    DATA: element   TYPE REF TO z2mse_extr3_elements,
          is_access TYPE abap_bool.

    element = element_manager->get_element( element_id ).

    DATA classes TYPE REF TO z2mse_extr3_classes.
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).

    DATA access TYPE REF TO z2mse_extr3_access.
    access = z2mse_extr3_access=>get_instance( i_element_manager = element_manager ).

    DATA invocation TYPE REF TO z2mse_extr3_invocation.
    invocation = z2mse_extr3_invocation=>get_instance( i_element_manager = element_manager ).

    CASE element->type.
      WHEN element->class_type.
        DATA class_name TYPE string.
        DATA cmpname TYPE string.
        DATA cmptype TYPE seocmptype.
        DATA clstype  TYPE seoclstype.
        DATA exists TYPE abap_bool.

        IF element_manager->exclude_found_sap_intf EQ abap_true.

          classes->class_name( EXPORTING element_id = element_id
                               IMPORTING class_name = class_name
                                         clstype    = clstype
                                         exists     = exists ).
          IF exists EQ abap_true.

            IF clstype EQ classes->interface_type.
              IF class_name CP 'Y*'
              OR class_name CP 'Z*'
              OR class_name CP '/*' .
                " OK
              ELSE.

                " Do not collect, it should be OK just to leave the method here
                RETURN.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

        classes->comp_name( EXPORTING element_id = element_id
                            IMPORTING class_name = class_name
                                      cmpname    = cmpname
                                      cmptype = cmptype ).

        IF element_manager->exclude_found_sap_intf EQ abap_true AND class_name CP 'IF*'.

          " Do not collect, it should be OK just to leave the method here
          RETURN.

        ENDIF.

        DATA: otype           TYPE char2,
              where_used_name TYPE eu_lname.

        CASE cmptype.
          WHEN classes->method_type.
            otype = 'ME'.
          WHEN classes->attribute_type.
            otype = 'DA'.

            is_access = abap_true.

          WHEN classes->event_type.
            otype = 'EV'.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        FIND '~' IN cmpname.

        IF sy-subrc <> 0.

          where_used_name = class_name && |\\| && otype && |:| && cmpname.

        ELSE.

          DATA: interface_name TYPE string,
                method_name    TYPE string.

          SPLIT cmpname AT '~' INTO interface_name method_name.

          where_used_name = class_name && |\\IN:| && interface_name && |\\| && otype && |:| && method_name.

        ENDIF.

      WHEN element->table_type.

        DATA tables TYPE REF TO z2mse_extr3_tables.
        tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).

        DATA table TYPE tabname.

        table = tables->table_name( i_element_id = element_id ).

        otype = 'TY'.

        where_used_name = table.

        is_access = abap_true.

      WHEN element->program_type.

        " Duplicate coding 2/2 see method search_down

        DATA programs2 TYPE REF TO z2mse_extr3_programs.
        programs2 = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).

        DATA: program             TYPE progname,
              program_type        TYPE string,
              program_attribute_2 TYPE string.



        programs2->program_name(
          EXPORTING
            i_element_id                 = element_id
          IMPORTING
            program                      = program
            program_type                 = program_type
            program_attribute_2          = program_attribute_2
*            external_program_name_class  =
*            external_program_name_method =
*            subc                         =
        ).

        DATA: cross_type TYPE char1,
              cross_name TYPE seu_name.
        CASE program_type.
          WHEN programs2->type_program.
            cross_type = 'R'.
            cross_name = program.
          WHEN programs2->type_function.
            cross_type = 'F'.
            cross_name = program_attribute_2.
          WHEN OTHERS.
            " TBD
        ENDCASE.


    ENDCASE.

    IF where_used_name IS NOT INITIAL OR cross_name IS NOT INITIAL.

      DATA: wbcrossgts TYPE wbcrossgts_type,
            wbcrossgt  TYPE wbcrossgt_type,
            cross      TYPE cross_type,
            crosss     TYPE cross_types.

      IF otype IS NOT INITIAL.

        SELECT otype name include direct indirect
          FROM wbcrossgt
          INTO TABLE wbcrossgts
          WHERE otype = otype
            AND name = where_used_name.

      ENDIF.

      IF cross_type IS NOT INITIAL.

        SELECT type name include FROM cross INTO TABLE crosss
          WHERE type = cross_type
            AND name = cross_name.

        LOOP AT crosss INTO cross.

          " Most where used information is in table wbcrossgt. Now some information is read from table cross. Use nonetheless wbcrossgt
          " for further processing. There is an integration test, that assures the correctness of the whole coding.

          CLEAR wbcrossgt.

          wbcrossgt-name = cross-name.
          wbcrossgt-include = cross-include.

          INSERT wbcrossgt INTO TABLE wbcrossgts.

        ENDLOOP.

      ENDIF.

      " Read dynamic usages

      DATA: w TYPE wbcrossgt.
      LOOP AT g_dynamic_usage INTO w
        WHERE otype = otype
          AND name = where_used_name.
        MOVE-CORRESPONDING w TO wbcrossgt.
        INSERT wbcrossgt INTO TABLE wbcrossgts.
      ENDLOOP.

      LOOP AT wbcrossgts INTO wbcrossgt.

        DATA: is_added           TYPE abap_bool,
              used_by_element_id TYPE z2mse_extr3_element_manager=>element_id_type.

        CLEAR is_added.

        " Analyze where used table
        DATA: pgmid    TYPE pgmid,
              object   TYPE trobjtype,
              obj_name TYPE trobj_name.

        DATA found_class_name TYPE string.
        DATA found_cmpname TYPE string.

        cl_oo_include_naming=>get_trkey_by_include(
          EXPORTING
            progname        = wbcrossgt-include
          IMPORTING
            pgmid           = pgmid
            object          = object
            obj_name        = obj_name
          EXCEPTIONS
            no_objecttype   = 1
            invalid_include = 2
            internal_error  = 3
            OTHERS          = 4
        ).
        IF sy-subrc EQ 0.

          CASE object.
            WHEN 'METH'.
              found_class_name = obj_name+0(30).
              " TBD include code here?
              found_cmpname = obj_name+30(61).

              DATA: temp TYPE string.
              temp = class_name && |~| && cmpname.

              IF found_cmpname <> temp. " Implementation of interface methods are in the where used list. These are added explicitely in the class coding. So filter here.

                classes->add_component(
                  EXPORTING
                    clsname        = found_class_name
                    cmpname        = found_cmpname
                    is_specific    = abap_false
                  IMPORTING
                    is_added       = is_added
                    new_element_id = used_by_element_id ).
                IF is_added EQ abap_true.

                  element_manager->model_builder->new_element_id( EXPORTING i_element_id  = used_by_element_id
                                                                            i_is_specific = abap_true ).

                ELSE.
                  "TBD what is to be done here?

                ENDIF.

              ENDIF.

            WHEN OTHERS.
              " TBD is code required here?
          ENDCASE.
*          <include_2_component>-is_class_component = abap_true.

        ELSE.


          "Check for usage in Web Dynpro ABAP
          DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.

          TEST-SEAM wdy_wb_sourcemap.

            SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = wbcrossgt-include AND srtf2 = 0.

          END-TEST-SEAM.

          IF sy-subrc EQ 0.
            DATA web_dynpro_component TYPE REF TO z2mse_extr3_web_dynpro_comp.
            web_dynpro_component = z2mse_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

            web_dynpro_component->add_component( EXPORTING wdy_component_name  = ls_wd_sourcemap-component_name
                                                           wdy_controller_name = ls_wd_sourcemap-controller_name
                                                 IMPORTING is_added            = is_added
                                                           new_element_id      = used_by_element_id ).

          ELSE.

            DATA programs TYPE REF TO z2mse_extr3_programs.
            programs = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).

            programs->add( EXPORTING program        = wbcrossgt-include
                           IMPORTING is_added       = is_added
                                     new_element_id = used_by_element_id ).

          ENDIF.

        ENDIF.

        IF is_added EQ abap_true.

          IF is_access EQ abap_true.

            access->add( EXPORTING accessed_element_id1  = element_id
                                   accessing_element_id2 = used_by_element_id ).

          ELSE.

            DATA: is_redefinition_of_method TYPE abap_bool.

            is_redefinition_of_method = classes->is_redefinition_of_method( invoced_element_id1  = element_id
                                                                            invocing_element_id2 = used_by_element_id ).

            IF is_redefinition_of_method EQ ''.

              invocation->add( EXPORTING invoced_element_id1  = element_id
                                         invocing_element_id2 = used_by_element_id ).

            ELSE.

              invocation->add( EXPORTING invoced_element_id1  = used_by_element_id
                                         invocing_element_id2 = element_id ).

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.




  METHOD set_dynamic_read.

    DATA: s     TYPE string,
          class TYPE string,
          t     TYPE TABLE OF string,
          w2    TYPE wbcrossgt.

    CLEAR g_dynamic_usage.

    s =  i_dynamic_read.
    TRANSLATE s TO UPPER CASE.
    CONDENSE s.
    SPLIT s AT space INTO TABLE t.

    LOOP AT t INTO class.

      DATA dyn_usage TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY.

      CALL METHOD (class)=>where_used
        IMPORTING
          data = dyn_usage.

      LOOP AT dyn_usage INTO w2.

        INSERT w2 INTO TABLE g_dynamic_usage.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
