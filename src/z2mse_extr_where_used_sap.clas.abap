CLASS z2mse_extr_where_used_sap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_wbcrossgt_test,
        otype    TYPE char2,
        name     TYPE eu_lname,
        include  TYPE programm,
        direct   TYPE sgrade,
        indirect TYPE sgrade,
      END OF ty_wbcrossgt_test.
    TYPES ty_t_wbcrossgt_test TYPE SORTED TABLE OF ty_wbcrossgt_test WITH UNIQUE KEY otype name include.

    TYPES: BEGIN OF ty_include_to_component,
             include            TYPE programm,
             is_class_component TYPE abap_bool,
             clsname            TYPE seoclsname,
             clstype            TYPE seoclstype,
             cmpname            TYPE seocmpname,
             cmptype            TYPE seocmptype,
           END OF ty_include_to_component.
    TYPES: ty_includes_to_components TYPE HASHED TABLE OF ty_include_to_component WITH UNIQUE KEY include.

    TYPES: BEGIN OF ty_comp_used_by_comp,
             clsname         TYPE seoclsname,
             cmpname         TYPE seocmpname,
             cmptype         TYPE seocmptype,
             used_by_clsname TYPE seoclsname,
             used_by_cmpname TYPE seocmpname,
             used_by_cmptype TYPE seocmptype,
           END OF ty_comp_used_by_comp.
    TYPES: ty_comps_used_by_comps TYPE STANDARD TABLE OF ty_comp_used_by_comp WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        wbcrossgt_test         TYPE ty_t_wbcrossgt_test OPTIONAL
        includes_to_components TYPE ty_includes_to_components OPTIONAL.
    "! Is this name correct?
    METHODS used_by_class_component
      IMPORTING
        class_components TYPE z2mse_extr_classes=>ty_class_components.
    METHODS add_usage_to_model
      IMPORTING
        famix_method     TYPE REF TO z2mse_famix_method
        famix_attribute  TYPE REF TO z2mse_famix_attribute
        famix_invocation TYPE REF TO z2mse_famix_invocation
        famix_access     TYPE REF TO z2mse_famix_access.
    METHODS get_components_where_used
      RETURNING VALUE(components) TYPE z2mse_extr_classes=>ty_class_components_hashed.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_name_to_component,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
             clsname         TYPE seoclsname,
             cmpname         TYPE seocmpname,
             cmptype         TYPE seocmptype,
           END OF ty_name_to_component.
    TYPES: ty_names_to_components TYPE HASHED TABLE OF ty_name_to_component WITH UNIQUE KEY otype where_used_name.
    "! Filled during tests
    DATA g_wbcrossgt_test TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test.
    "! Filled during tests
    DATA g_includes_to_components_test TYPE ty_includes_to_components.
    DATA g_is_test TYPE abap_bool.
    "! all class components from initial search
    DATA g_class_components_initial TYPE z2mse_extr_classes=>ty_class_components_hashed.
    "! All class components that where found during where used
    DATA g_class_components_where_used TYPE z2mse_extr_classes=>ty_class_components_hashed.
    DATA g_comps_used_by_comps TYPE ty_comps_used_by_comps.
    METHODS _fill_comps_used_by_comps
      IMPORTING
        i_names_to_components   TYPE z2mse_extr_where_used_sap=>ty_names_to_components
        i_found_wbcrossgt       TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test
        i_includes_2_components TYPE z2mse_extr_where_used_sap=>ty_includes_to_components.
    METHODS _determine_mapping_include_to
      IMPORTING
        i_found_wbcrossgt              TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test
      RETURNING
        VALUE(r_includes_2_components) TYPE z2mse_extr_where_used_sap=>ty_includes_to_components.
    METHODS _select_where_used_table
      IMPORTING
        i_names_to_components    TYPE z2mse_extr_where_used_sap=>ty_names_to_components
      RETURNING
        VALUE(r_found_wbcrossgt) TYPE z2mse_extr_where_used_sap=>ty_t_wbcrossgt_test.
    METHODS _get_mapping_to_name
      IMPORTING
        i_class_components           TYPE z2mse_extr_classes=>ty_class_components
      RETURNING
        VALUE(r_names_to_components) TYPE z2mse_extr_where_used_sap=>ty_names_to_components.
ENDCLASS.



CLASS Z2MSE_EXTR_WHERE_USED_SAP IMPLEMENTATION.


  METHOD add_usage_to_model.

    DATA comp_used_by_comp TYPE ty_comp_used_by_comp.

    LOOP AT g_comps_used_by_comps INTO comp_used_by_comp WHERE used_by_cmptype <> z2mse_extr_classes=>attribute_type.

      ASSERT comp_used_by_comp-used_by_cmptype EQ z2mse_extr_classes=>method_type OR
             comp_used_by_comp-used_by_cmptype EQ z2mse_extr_classes=>event_type.

      DATA using_method_id TYPE i.
      using_method_id = famix_method->get_id( class  = comp_used_by_comp-used_by_clsname
                                              method = comp_used_by_comp-used_by_cmpname ).

      IF using_method_id IS INITIAL.
        "! TBD report or handle this better.
        " This happened because interface methods are not in table SEOCOMPO
        " The information is in table SEOMETAREL

        "! TBD THis is duplicate coding!
        "!

        " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
        " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
        famix_method->add( EXPORTING name = comp_used_by_comp-used_by_cmpname IMPORTING id = using_method_id ).

        " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
        " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
        famix_method->set_signature( element_id = using_method_id
                                       signature = comp_used_by_comp-used_by_cmpname ).

        famix_method->set_parent_type( EXPORTING element_id = using_method_id
                                                   parent_element = 'FAMIX.Class'
                                                   parent_name_group = 'ABAP_CLASS'
                                                   parent_name    = comp_used_by_comp-used_by_clsname ).


        famix_method->store_id( EXPORTING class  = comp_used_by_comp-used_by_clsname
                                            method = comp_used_by_comp-used_by_cmpname ).


*        using_method_id = sap_method->add( class  = comp_used_by_comp-used_by_clsname
*                                           method = comp_used_by_comp-used_by_cmpname ).
      ENDIF.

      DATA used_id TYPE i.
      CASE comp_used_by_comp-cmptype.
        WHEN z2mse_extr_classes=>attribute_type.

          used_id = famix_attribute->get_id( class     = comp_used_by_comp-clsname
                                           attribute = comp_used_by_comp-cmpname ).


          " SAP_2_FAMIX_26      Map usage of ABAP class attributes by methods of classes to FAMIX.Invocation
          " SAP_2_FAMIX_27      Map usage of ABAP interface attributes by methods of classes to FAMIX.Invocation

          IF famix_access->is_new_access( accessor_id = using_method_id
                                            variable_id = used_id )
             EQ abap_true.
            DATA last_id2 TYPE i.
            last_id2 = famix_access->add( ).
            famix_access->set_accessor_variable_relation( EXPORTING element_id = last_id2
                                                                      accessor_id = using_method_id
                                                                      variable_id = used_id ).
          ENDIF.

*          sap_access->add_access( used_attribute = used_id
*                                  using_method   = using_method_id ).


        WHEN z2mse_extr_classes=>method_type OR z2mse_extr_classes=>event_type.

          used_id = famix_method->get_id( class  = comp_used_by_comp-clsname
                                        method = comp_used_by_comp-cmpname ).

          " This is an interface. Reverse the usage direction
          " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method

          DATA: temp TYPE seocmpname.
          temp = comp_used_by_comp-clsname && |~| && comp_used_by_comp-cmpname.

          IF comp_used_by_comp-used_by_cmpname EQ temp.
            DATA: inv_used_id  TYPE i,
                  inv_using_id TYPE i.
            " Reverse direction
            inv_used_id = using_method_id.
            inv_using_id = used_id.
*            sap_invocation->add_invocation( used_method  = using_method_id
*                                            using_method = used_id ).

          ELSE.
            inv_used_id = used_id.
            inv_using_id = using_method_id.

          ENDIF.
          " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
          " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
          IF famix_invocation->is_new_invocation_to_candidate( sender_id     = inv_using_id
                                                                 candidates_id = inv_used_id )
             EQ abap_true.

            DATA invocation_id TYPE i.
            invocation_id = famix_invocation->add( ).
            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                       sender_id     = inv_using_id
                                                                       candidates_id = inv_used_id
                                                                       signature     = 'DUMMY' ).
          ENDIF.


*            sap_invocation->add_invocation( used_method  = inv_used_id
*                                            using_method = inv_using_id ).


        WHEN OTHERS.
          ASSERT 1 = 2.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
    IF wbcrossgt_test IS SUPPLIED OR
       includes_to_components IS SUPPLIED.
      g_wbcrossgt_test = wbcrossgt_test.
      g_includes_to_components_test = includes_to_components.
      g_is_test = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_components_where_used.
    components = g_class_components_where_used.
  ENDMETHOD.


  METHOD used_by_class_component.

    DATA name_to_component TYPE ty_name_to_component.
    DATA names_to_components TYPE ty_names_to_components.

    DATA where_used_name TYPE eu_lname.
    DATA class_component TYPE z2mse_extr_classes=>ty_class_component.

    g_class_components_initial = class_components.

    names_to_components = _get_mapping_to_name( class_components ).

    DATA found_wbcrossgt TYPE ty_t_wbcrossgt_test.

    found_wbcrossgt = _select_where_used_table( names_to_components ).

    DATA includes_2_components TYPE z2mse_extr_where_used_sap=>ty_includes_to_components.


    includes_2_components = _determine_mapping_include_to( found_wbcrossgt ).

    _fill_comps_used_by_comps(
          i_names_to_components   = names_to_components
          i_found_wbcrossgt       = found_wbcrossgt
          i_includes_2_components = includes_2_components ).

    SORT g_comps_used_by_comps.

  ENDMETHOD.


  METHOD _determine_mapping_include_to.

    DATA found_wbcrossgt_line TYPE z2mse_extr_where_used_sap=>ty_wbcrossgt_test.

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
        IF sy-subrc <> 0.
          "! TBD handle
        ELSE.
          <include_2_component>-clsname = obj_name+0(30).
          "! <include_2_component>-clstype = ?
          <include_2_component>-cmpname = obj_name+30(30).
          CASE object.
            WHEN 'METH'.
              <include_2_component>-cmptype = z2mse_extr_classes=>method_type.
            WHEN OTHERS.
              "! TBD handle
          ENDCASE.
          <include_2_component>-is_class_component = abap_true.
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


  METHOD _fill_comps_used_by_comps.

    DATA ntc TYPE z2mse_extr_where_used_sap=>ty_name_to_component.
    DATA found_wbcrossgt_line TYPE z2mse_extr_where_used_sap=>ty_wbcrossgt_test.
    DATA include_2_component TYPE z2mse_extr_where_used_sap=>ty_include_to_component.

    " fill comps used by comps

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.

      DATA cubc TYPE ty_comp_used_by_comp.

      CLEAR cubc.
      READ TABLE i_names_to_components INTO ntc WITH TABLE KEY otype           = found_wbcrossgt_line-otype
                                                                           where_used_name = found_wbcrossgt_line-name.
      cubc-clsname = ntc-clsname.
      cubc-cmpname = ntc-cmpname.
      cubc-cmptype = ntc-cmptype.

      READ TABLE i_includes_2_components INTO include_2_component WITH TABLE KEY include = found_wbcrossgt_line-include.
      IF include_2_component-is_class_component EQ abap_true.
        cubc-used_by_clsname = include_2_component-clsname.
        cubc-used_by_cmpname = include_2_component-cmpname.
        cubc-used_by_cmptype = include_2_component-cmptype.

        INSERT cubc INTO TABLE g_comps_used_by_comps.

        READ TABLE g_class_components_initial TRANSPORTING NO FIELDS WITH TABLE KEY clsname = cubc-used_by_clsname
                                                                                    cmpname = cubc-used_by_cmpname.
        IF sy-subrc <> 0.
          DATA cwu LIKE LINE OF g_class_components_initial.
          cwu-clsname = cubc-used_by_clsname.
          cwu-cmpname = cubc-used_by_cmpname.
          cwu-cmptype = cubc-used_by_cmptype.
          INSERT cwu INTO TABLE g_class_components_where_used.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_mapping_to_name.

    DATA n2c TYPE z2mse_extr_where_used_sap=>ty_name_to_component.
    DATA comp TYPE z2mse_extr_classes=>ty_class_component.

    " get mapping to name
    LOOP AT i_class_components INTO comp.
      CLEAR n2c.
      CASE comp-cmptype.
        WHEN z2mse_extr_classes=>method_type.
          n2c-otype = 'ME'.
        WHEN z2mse_extr_classes=>attribute_type.
          n2c-otype = 'DA'.
        WHEN z2mse_extr_classes=>event_type.
          n2c-otype = 'EV'.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
      n2c-where_used_name = comp-clsname && |\\| && n2c-otype && |:| && comp-cmpname.
      n2c-clsname = comp-clsname.
      n2c-cmpname = comp-cmpname.
      n2c-cmptype = comp-cmptype.
      INSERT n2c INTO TABLE r_names_to_components.
    ENDLOOP.

  ENDMETHOD.


  METHOD _select_where_used_table.

    DATA name_to_component TYPE z2mse_extr_where_used_sap=>ty_name_to_component.

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

      LOOP AT i_names_to_components INTO name_to_component.
        LOOP AT g_wbcrossgt_test INTO found_wbcrossgt_line WHERE otype = name_to_component-otype
                                                             AND name = name_to_component-where_used_name.

          INSERT found_wbcrossgt_line INTO TABLE r_found_wbcrossgt.

        ENDLOOP.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
