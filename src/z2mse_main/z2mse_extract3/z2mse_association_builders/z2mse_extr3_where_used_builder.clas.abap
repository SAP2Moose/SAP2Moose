CLASS z2mse_extr3_where_used_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS search_up REDEFINITION.
    METHODS search_down REDEFINITION.
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
      wbcrossgts_type TYPE SORTED TABLE OF wbcrossgt_type WITH UNIQUE KEY otype name include.
ENDCLASS.



CLASS z2mse_extr3_where_used_builder IMPLEMENTATION.

  METHOD search_up.

    DATA: element TYPE REF TO z2mse_extr3_elements.

    element = element_manager->get_element( element_id ).

    DATA classes TYPE REF TO z2mse_extr3_classes.
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).

    CASE element->type.
      WHEN element->class_type.
        DATA class_name TYPE seoclsname.
        DATA cmpname TYPE seocmpname.
        DATA cmptype TYPE seocmptype.

        classes->comp_name( EXPORTING element_id = element_id
                            IMPORTING class_name = class_name
                                      cmpname    = cmpname
                                      cmptype = cmptype ).

        DATA: otype           TYPE char2,
              where_used_name TYPE eu_lname.

        CASE cmptype.
          WHEN classes->method_type.
            otype = 'ME'.
          WHEN classes->attribute_type.
            otype = 'DA'.
          WHEN classes->event_type.
            otype = 'EV'.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

        where_used_name = class_name && |\\| && otype && |:| && cmpname.




      WHEN element->table_type.

        DATA tables TYPE REF TO z2mse_extr3_tables.
        tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).

        DATA table TYPE tabname.

        table = tables->table_name( i_element_id = element_id ).

        otype = 'TY'.

        where_used_name = table.

    ENDCASE.

    IF where_used_name IS NOT INITIAL.

      DATA: wbcrossgts TYPE wbcrossgts_type,
            wbcrossgt  TYPE wbcrossgt_type.

      SELECT otype name include direct indirect
        FROM wbcrossgt
        INTO TABLE wbcrossgts
        WHERE otype = otype
          AND name = where_used_name.

      LOOP AT wbcrossgts INTO wbcrossgt.

        DATA: is_added           TYPE abap_bool,
              used_by_element_id TYPE z2mse_extr3_element_manager=>element_id_type.

        CLEAR is_added.

        " Analyze where used table
        DATA: pgmid    TYPE pgmid,
              object   TYPE trobjtype,
              obj_name TYPE trobj_name.

        DATA found_class_name TYPE seoclsname.
        DATA found_cmpname TYPE seocmpname.

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
              found_cmpname = obj_name+30(30).

              classes->add_component(
                EXPORTING
                  clsname        = found_class_name
                  cmpname        = found_cmpname
                IMPORTING
                  is_added       = is_added
                  new_element_id = used_by_element_id ).

              IF is_added EQ abap_false.
                "TBD what is to be done here?

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

*          IF sy-subrc EQ 0.
*            <include_2_component>-is_webdynpro = abap_true.
*            <include_2_component>-component_name = ls_wd_sourcemap-component_name.
*            <include_2_component>-controller_name = ls_wd_sourcemap-controller_name.
*
*          ELSE.
*            <include_2_component>-is_program_or_function = abap_true.
*          ENDIF.

        ENDIF.

        IF is_added EQ abap_true.



        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD search_down.

    DATA: element TYPE REF TO z2mse_extr3_elements.

    element = element_manager->get_element( element_id ).

  ENDMETHOD.

ENDCLASS.
