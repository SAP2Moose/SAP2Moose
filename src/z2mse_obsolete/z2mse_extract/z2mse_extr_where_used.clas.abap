CLASS z2mse_extr_where_used DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_wbcrossgt_test,
        otype    TYPE char2,
        name     TYPE eu_lname,
        include  TYPE programm,
        direct   TYPE sgrade,
        indirect TYPE sgrade,
      END OF ty_wbcrossgt_test ,
      ty_t_wbcrossgt_test TYPE SORTED TABLE OF ty_wbcrossgt_test WITH UNIQUE KEY otype name include.

    TYPES:
      BEGIN OF ty_include_to_component,
        include                TYPE programm,
        is_class_component     TYPE abap_bool,
        clsname                TYPE seoclsname,
        clstype                TYPE seoclstype,
        cmpname                TYPE seocmpname,
        cmptype                TYPE seocmptype,
        is_webdynpro           TYPE abap_bool,
        component_name         TYPE wdy_component_name,
        controller_name        TYPE wdy_controller_name,
        is_program_or_function TYPE abap_bool,
      END OF ty_include_to_component .
    TYPES:
      ty_includes_to_components TYPE HASHED TABLE OF ty_include_to_component WITH UNIQUE KEY include .
    METHODS constructor
      IMPORTING
        wbcrossgt_test         TYPE ty_t_wbcrossgt_test
        includes_to_components TYPE ty_includes_to_components.
    "! Returns all components that are found in the last where-used analysis. Returns this components only once
    METHODS get_components_where_used
      EXPORTING
        VALUE(components)            TYPE z2mse_extr_classes=>ty_class_components_hashed
        VALUE(web_dynpro_components) TYPE z2mse_extr_web_dynpro=>ty_web_dynpro_components_hash
        VALUE(includes)              TYPE z2mse_extr_programs=>ty_includes_hashed.
  PROTECTED SECTION.
    "! Filled during tests
    DATA g_wbcrossgt_test TYPE ty_t_wbcrossgt_test.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_includes_to_components_test TYPE ty_includes_to_components.
    "! All class components that where found during where used
    DATA g_class_components_where_used TYPE z2mse_extr_classes=>ty_class_components_hashed.
    "! All Web Dynpro ABAP components that where found during where used
    DATA g_web_dynpro_cmpnts_where_used TYPE z2mse_extr_web_dynpro=>ty_web_dynpro_components_hash.
    "! All Includes (Programs or Functions) that where found during where used
    DATA g_includes_where_used TYPE z2mse_extr_programs=>ty_includes_hashed.

    TYPES: BEGIN OF ty_where_used_name,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
           END OF ty_where_used_name.
    TYPES: ty_where_used_names TYPE HASHED TABLE OF ty_where_used_name WITH UNIQUE KEY otype where_used_name.

    METHODS _select_where_used_table
      IMPORTING
        i_names_to_components    TYPE z2mse_extr_where_used=>ty_where_used_names
      RETURNING
        VALUE(r_found_wbcrossgt) TYPE ty_t_wbcrossgt_test.
    METHODS _determine_mapping_include_to
      IMPORTING
        i_found_wbcrossgt              TYPE ty_t_wbcrossgt_test
      RETURNING
        VALUE(r_includes_2_components) TYPE ty_includes_to_components.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr_where_used IMPLEMENTATION.


  METHOD constructor.

    g_wbcrossgt_test = wbcrossgt_test.

    g_includes_to_components_test = includes_to_components.

    IF g_wbcrossgt_test IS NOT INITIAL
    OR g_includes_to_components_test IS NOT INITIAL.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_components_where_used.

    components = g_class_components_where_used.
    CLEAR g_class_components_where_used.

    web_dynpro_components = g_web_dynpro_cmpnts_where_used.
    CLEAR g_web_dynpro_cmpnts_where_used.

    includes = g_includes_where_used.
    CLEAR g_includes_where_used.

  ENDMETHOD.


  METHOD _determine_mapping_include_to.

    DATA found_wbcrossgt_line TYPE z2mse_extr_where_used_classes=>ty_wbcrossgt_test.

    " Determine mapping include to component

    DATA: includes_2_components TYPE ty_includes_to_components,
          include_2_component   TYPE ty_include_to_component.

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.
      include_2_component-include = found_wbcrossgt_line-include.
      INSERT include_2_component INTO TABLE r_includes_2_components.
    ENDLOOP.

    FIELD-SYMBOLS: <include_2_component> TYPE ty_include_to_component.

    LOOP AT r_includes_2_components ASSIGNING <include_2_component>.

      " Analyze where used table
      DATA: pgmid    TYPE pgmid,
            object   TYPE trobjtype,
            obj_name TYPE trobj_name.

      IF g_is_test EQ abap_false.

        cl_oo_include_naming=>get_trkey_by_include(
          EXPORTING
            progname        = <include_2_component>-include
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

          <include_2_component>-clsname = obj_name+0(30).
          " TBD include code here?
          <include_2_component>-cmpname = obj_name+30(30).
          CASE object.
            WHEN 'METH'.
              <include_2_component>-cmptype = z2mse_extr_classes=>method_type.
            WHEN OTHERS.
              " TBD is code required here?
          ENDCASE.
          <include_2_component>-is_class_component = abap_true.

        ELSE.


          "Check for usage in Web Dynpro ABAP
          DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.

          TEST-SEAM wdy_wb_sourcemap.

            SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = <include_2_component>-include AND srtf2 = 0.

          END-TEST-SEAM.

          IF sy-subrc EQ 0.
            <include_2_component>-is_webdynpro = abap_true.
            <include_2_component>-component_name = ls_wd_sourcemap-component_name.
            <include_2_component>-controller_name = ls_wd_sourcemap-controller_name.

          ELSE.
            <include_2_component>-is_program_or_function = abap_true.
          ENDIF.

        ENDIF.

      ELSE.

        DATA include_to_component_test TYPE ty_include_to_component.

        READ TABLE g_includes_to_components_test INTO include_to_component_test WITH TABLE KEY include = <include_2_component>-include.
        IF sy-subrc EQ 0.
          <include_2_component>-clsname = include_to_component_test-clsname.
          <include_2_component>-cmpname = include_to_component_test-cmpname.
          <include_2_component>-cmptype = include_to_component_test-cmptype.
          <include_2_component>-is_class_component = abap_true.
        ENDIF.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD _select_where_used_table.

    DATA name TYPE ty_where_used_name.

    " Select where used table
    IF g_is_test EQ abap_false.
      IF i_names_to_components IS NOT INITIAL.
        SELECT otype name include direct indirect
          FROM wbcrossgt
          INTO TABLE r_found_wbcrossgt
          FOR ALL ENTRIES IN i_names_to_components
          WHERE otype = i_names_to_components-otype
            AND name = i_names_to_components-where_used_name.
      ENDIF.
    ELSE.

      DATA found_wbcrossgt_line TYPE ty_wbcrossgt_test.

      LOOP AT i_names_to_components INTO name.
        LOOP AT g_wbcrossgt_test INTO found_wbcrossgt_line WHERE otype = name-otype
                                                             AND name = name-where_used_name.

          INSERT found_wbcrossgt_line INTO TABLE r_found_wbcrossgt.

        ENDLOOP.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
