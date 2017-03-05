"! Extract elements that use certain tables
CLASS z2mse_extr_where_used_tables DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr_where_used
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !wbcrossgt_test         TYPE ty_t_wbcrossgt_test OPTIONAL
        !includes_to_components TYPE ty_includes_to_components OPTIONAL.
    "! Receives a list of tables. Does a where-used analysis. Stores elements that are using the received elements internally
    METHODS used_by_table
      IMPORTING
        tables TYPE z2mse_extr_tables=>ty_tables_public.


    TYPES:
      BEGIN OF ty_table_used_by_element,
        table              TYPE tabname,
        is_class_component TYPE abap_bool,
        used_by_clsname    TYPE seoclsname,
        used_by_cmpname    TYPE seocmpname,
        used_by_cmptype    TYPE seocmptype,
        is_webdynpro       TYPE abap_bool,
        component_name     TYPE wdy_component_name,
        controller_name    TYPE wdy_controller_name,
      END OF ty_table_used_by_element .
    TYPES:
      ty_tables_used_by_elements TYPE STANDARD TABLE OF ty_table_used_by_element WITH DEFAULT KEY.

    "! Add all selected components to the model. Should be called only once
    METHODS add_usage_to_model
      IMPORTING
        !famix_method     TYPE REF TO z2mse_famix_method
        !famix_attribute  TYPE REF TO z2mse_famix_attribute
        !famix_invocation TYPE REF TO z2mse_famix_invocation
        !famix_access     TYPE REF TO z2mse_famix_access.

  PROTECTED SECTION.
  PRIVATE SECTION.
    "! all tables from initial search
    DATA g_tables_initial TYPE z2mse_extr_tables=>ty_tables_public.

    TYPES: BEGIN OF ty_name_to_table,
             otype           TYPE char2,
             where_used_name TYPE eu_lname,
             table           TYPE tabname,
           END OF ty_name_to_table.
    TYPES: ty_names_to_tables TYPE HASHED TABLE OF ty_name_to_table WITH UNIQUE KEY otype where_used_name.
    METHODS _get_mapping_to_name
      IMPORTING
        tables                   TYPE z2mse_extr_tables=>ty_tables_public
      RETURNING
        VALUE(r_names_to_tables) TYPE z2mse_extr_where_used_tables=>ty_names_to_tables.

    DATA g_tables_used_by_elements TYPE ty_tables_used_by_elements.
    METHODS _fill_tables_used_by_elements
      IMPORTING
        i_names_to_components   TYPE ty_names_to_tables
        i_found_wbcrossgt       TYPE ty_t_wbcrossgt_test
        i_includes_2_components TYPE ty_includes_to_components.
ENDCLASS.



CLASS z2mse_extr_where_used_tables IMPLEMENTATION.


  METHOD constructor.

    super->constructor( wbcrossgt_test         = wbcrossgt_test
                        includes_to_components = includes_to_components ).

  ENDMETHOD.


  METHOD used_by_table.

    g_tables_initial = tables.

    DATA: names_to_tables TYPE ty_names_to_tables,
          name_to_table   TYPE ty_name_to_table.

    names_to_tables = _get_mapping_to_name( tables = tables ).

    DATA found_wbcrossgt TYPE ty_t_wbcrossgt_test.
    DATA: names TYPE ty_where_used_names,
          name  TYPE ty_where_used_name.

    LOOP AT names_to_tables INTO name_to_table.
      CLEAR name.
      name-otype = name_to_table-otype.
      name-where_used_name = name_to_table-where_used_name.
      INSERT name INTO TABLE names.
    ENDLOOP.

    found_wbcrossgt = _select_where_used_table( names ).

    DATA includes_2_components TYPE ty_includes_to_components.

    includes_2_components = _determine_mapping_include_to( found_wbcrossgt ).

    _fill_tables_used_by_elements( EXPORTING i_names_to_components   = names_to_tables
                                          i_found_wbcrossgt       = found_wbcrossgt
                                          i_includes_2_components = includes_2_components ).

    SORT g_tables_used_by_elements.

  ENDMETHOD.

  METHOD _get_mapping_to_name.

    DATA n2t TYPE z2mse_extr_where_used_tables=>ty_name_to_table.
    DATA table TYPE z2mse_extr_tables=>ty_table_public.

    LOOP AT tables INTO table.
      CLEAR n2t.
    ENDLOOP.

    n2t-otype = 'TY'.
    "! TYPE is also used for classes, ... What happen if a table has the same name as a class?
    n2t-where_used_name = table-tabname.
    n2t-table = table-tabname.
    INSERT n2t INTO TABLE r_names_to_tables.

  ENDMETHOD.

  METHOD _fill_tables_used_by_elements.

    DATA ntc TYPE ty_name_to_table.
    DATA found_wbcrossgt_line TYPE ty_wbcrossgt_test.
    DATA include_2_component TYPE ty_include_to_component.

    " fill comps used by comps

    LOOP AT i_found_wbcrossgt INTO found_wbcrossgt_line.

      DATA tbube TYPE ty_table_used_by_element.

      CLEAR tbube.
      READ TABLE i_names_to_components INTO ntc WITH TABLE KEY otype           = found_wbcrossgt_line-otype
                                                               where_used_name = found_wbcrossgt_line-name.
      tbube-table = ntc-table.

      READ TABLE i_includes_2_components INTO include_2_component WITH TABLE KEY include = found_wbcrossgt_line-include.
      IF include_2_component-is_class_component EQ abap_true.
        tbube-is_class_component = abap_true.
        tbube-used_by_clsname = include_2_component-clsname.
        tbube-used_by_cmpname = include_2_component-cmpname.
        tbube-used_by_cmptype = include_2_component-cmptype.

        INSERT tbube INTO TABLE g_tables_used_by_elements.

        DATA cwu LIKE LINE OF g_class_components_where_used.
        cwu-clsname = tbube-used_by_clsname.
        cwu-cmpname = tbube-used_by_cmpname.
        cwu-cmptype = tbube-used_by_cmptype.
        INSERT cwu INTO TABLE g_class_components_where_used.

      ELSEIF include_2_component-is_webdynpro EQ abap_true.

        tbube-is_webdynpro = abap_true.
        tbube-component_name = include_2_component-component_name.
        tbube-controller_name = include_2_component-controller_name.

        INSERT tbube INTO TABLE g_tables_used_by_elements.

        DATA wdcwu LIKE LINE OF g_web_dynpro_cmpnts_where_used.
        wdcwu-component_name = include_2_component-component_name.
        wdcwu-controller_name = include_2_component-controller_name.
        INSERT wdcwu INTO TABLE g_web_dynpro_cmpnts_where_used.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD add_usage_to_model.

    DATA comp_used_by_comp TYPE ty_table_used_by_element.

    LOOP AT g_tables_used_by_elements INTO comp_used_by_comp.

      IF comp_used_by_comp-is_class_component EQ abap_true.

        IF comp_used_by_comp-used_by_cmptype EQ z2mse_extr_classes=>attribute_type.
          CONTINUE.
        ENDIF.

        ASSERT comp_used_by_comp-used_by_cmptype EQ z2mse_extr_classes=>method_type OR
               comp_used_by_comp-used_by_cmptype EQ z2mse_extr_classes=>event_type.

        DATA using_method_id TYPE i.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-used_by_clsname
                                                method = comp_used_by_comp-used_by_cmpname ).
      ELSEIF comp_used_by_comp-is_webdynpro EQ abap_true.

        using_method_id = famix_method->get_id( class  = comp_used_by_comp-component_name
                                                method = comp_used_by_comp-controller_name ).
      ELSE.
        ASSERT 1 = 2.
      ENDIF.


*      IF using_method_id IS INITIAL.
*        "! TBD report or handle this better.
*        " This happened because interface methods are not in table SEOCOMPO
*        " The information is in table SEOMETAREL
*
*        "! TBD THis is duplicate coding!
*        "!
*
*        " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
*        " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
*        famix_method->add( EXPORTING name = comp_used_by_comp-used_by_cmpname IMPORTING id = using_method_id ).
*
*        " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
*        " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
*        famix_method->set_signature( element_id = using_method_id
*                                       signature = comp_used_by_comp-used_by_cmpname ).
*
*        famix_method->set_parent_type( EXPORTING element_id = using_method_id
*                                                   parent_element = 'FAMIX.Class'
*                                                   parent_name_group = 'ABAP_CLASS'
*                                                   parent_name    = comp_used_by_comp-used_by_clsname ).
*
*
*        famix_method->store_id( EXPORTING class  = comp_used_by_comp-used_by_clsname
*                                            method = comp_used_by_comp-used_by_cmpname ).
*
*
**        using_method_id = sap_method->add( class  = comp_used_by_comp-used_by_clsname
**                                           method = comp_used_by_comp-used_by_cmpname ).
*      ENDIF.

      DATA used_id TYPE i.
*      CASE comp_used_by_comp-cmptype.
*        WHEN z2mse_extr_classes=>attribute_type.

      used_id = famix_attribute->get_id( class     = comp_used_by_comp-table
                                         attribute = comp_used_by_comp-table ).


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


*        WHEN z2mse_extr_classes=>method_type OR z2mse_extr_classes=>event_type.
*
*          used_id = famix_method->get_id( class_name_group = ''
*                                          class            = comp_used_by_comp-clsname
*                                          method           = comp_used_by_comp-cmpname ).
*
*          " This is an interface. Reverse the usage direction
*          " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method
*
*          DATA: temp TYPE seocmpname.
*          temp = comp_used_by_comp-clsname && |~| && comp_used_by_comp-cmpname.
*
*          IF comp_used_by_comp-used_by_cmpname EQ temp.
*
*            "! TBD This will not be used in future, because the implementation of an interface will be handled in Z2MSE_EXTR_CLASSES
*
*            CONTINUE.
*
*            DATA: inv_used_id  TYPE i,
*                  inv_using_id TYPE i.
*            " Reverse direction
*            inv_used_id = using_method_id.
*            inv_using_id = used_id.
**            sap_invocation->add_invocation( used_method  = using_method_id
**                                            using_method = used_id ).
*
*          ELSE.
*            inv_used_id = used_id.
*            inv_using_id = using_method_id.
*
*          ENDIF.
*          " SAP_2_FAMIX_24      Map usage of ABAP class methods by methods of classes to FAMIX.Invocation
*          " SAP_2_FAMIX_25      Map usage of ABAP interface methods by methods of classes to FAMIX.Invocation
*          IF famix_invocation->is_new_invocation_to_candidate( sender_id     = inv_using_id
*                                                                 candidates_id = inv_used_id )
*             EQ abap_true.
*
*            DATA invocation_id TYPE i.
*            invocation_id = famix_invocation->add( ).
*            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
*                                                                       sender_id     = inv_using_id
*                                                                       candidates_id = inv_used_id
*                                                                       signature     = 'DUMMY' ).
*          ENDIF.
*
*
**            sap_invocation->add_invocation( used_method  = inv_used_id
**                                            using_method = inv_using_id ).
*

*        WHEN OTHERS.
*          ASSERT 1 = 2.
*
*      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
