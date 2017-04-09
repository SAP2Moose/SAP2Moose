CLASS z2mse_extr3_classes DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: is_class_type  TYPE seoclstype VALUE 0,
               interface_type TYPE seoclstype VALUE 1,
               attribute_type TYPE seocmptype VALUE 0,
               method_type    TYPE seocmptype VALUE 1,
               event_type     TYPE seocmptype VALUE 2.



    TYPES: BEGIN OF ty_class_component,
             clsname TYPE seoclsname,
             cmpname TYPE seocmpname,
             cmptype TYPE seocmptype,
             mtdtype TYPE seomtdtype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE STANDARD TABLE OF ty_class_component WITH KEY clsname cmpname.
    DATA: class_components TYPE ty_class_components.

    CLASS-METHODS get_instance
      IMPORTING
        element_manager   TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(r_instance) TYPE REF TO z2mse_extr3_classes.
    METHODS add
      IMPORTING
        class                 TYPE seoclsname
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type
        class_components      TYPE ty_class_components.
    METHODS add_component
      IMPORTING
        clsname               TYPE seoclsname
        cmpname               TYPE seocmpname
        is_specific           TYPE abap_bool
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS class_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE seoclsname
        VALUE(clstype)    TYPE seoclstype
        VALUE(exists)     TYPE abap_bool.
    METHODS comp_name
      IMPORTING
        element_id        TYPE i
      EXPORTING
        VALUE(class_name) TYPE seoclsname
        VALUE(cmpname)    TYPE seocmpname
        VALUE(cmptype)    TYPE seocmptype
        VALUE(exists)     TYPE abap_bool.
    METHODS make_model REDEFINITION.
    METHODS name REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_classes.

    TYPES: BEGIN OF element_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             class_name TYPE seoclsname,
             clstype    TYPE seoclstype,
             "is_interface TYPE abap_bool,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_class_name TYPE HASHED TABLE OF element_type WITH UNIQUE KEY class_name.

    TYPES: BEGIN OF element_comp_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             clsname    TYPE seoclsname,
             cmpname    TYPE seocmpname,
             cmptype    TYPE seocmptype,
             mtdtype    TYPE seomtdtype,
           END OF element_comp_type.
    DATA elements_comp_element_id TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY element_id.
    DATA elements_comp_clsname_cmpname TYPE HASHED TABLE OF element_comp_type WITH UNIQUE KEY clsname cmpname.

    TYPES: BEGIN OF element_metarel_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             refclsname TYPE seoclsname,
             reltype    TYPE seoreltype,
           END OF element_metarel_type.
    DATA elements_metarel_element_id TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY element_id.
    DATA elements_metarel_refclsname TYPE HASHED TABLE OF element_metarel_type WITH UNIQUE KEY refclsname.

    METHODS _add_component
      IMPORTING
        clsname               TYPE seoclsname
        cmpname               TYPE seocmpname
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(is_added_now)   TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.

    METHODS _add_metarel
      IMPORTING
        clsname TYPE seoclsname.

    "! Call me only after checking that the component to be added is not already added.
    METHODS _add_single_component_to_class
      IMPORTING
        i_found_class_name      TYPE seoclsname
        i_found_cmpname         TYPE seocmpname
        i_found_cmptype         TYPE seocmptype
        i_found_mtdtype         TYPE seomtdtype
      RETURNING
        VALUE(r_new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.

ENDCLASS.



CLASS Z2MSE_EXTR3_CLASSES IMPLEMENTATION.


  METHOD add.

    DATA element TYPE element_type.

    READ TABLE elements_class_name INTO element WITH KEY class_name = class.
    IF sy-subrc EQ 0.
      is_added = abap_true.
      new_element_id = element-element_id.
    ELSE.

      " Does table exists?
      DATA: found_class_name TYPE seoclsname,
            found_class_type TYPE seoclstype.

      TEST-SEAM seoclass.
        SELECT SINGLE clsname clstype FROM seoclass INTO ( found_class_name , found_class_type ) WHERE clsname = class.
      END-TEST-SEAM.

      IF found_class_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = element_manager->add_element( element = me
                                                       is_specific = abap_false ).
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

      DATA class_component  TYPE ty_class_component.

      LOOP AT class_components INTO class_component.

        _add_component( EXPORTING clsname        = class_component-clsname
                                  cmpname        = class_component-cmpname ).

      ENDLOOP.

      _add_metarel( clsname = class ).

    ENDIF.

  ENDMETHOD.


  METHOD add_component.

    data: is_added_now TYPE abap_bool.

    add( EXPORTING class          = clsname
         IMPORTING is_added       = is_added ).

    IF is_added EQ abap_true.

      _add_component( EXPORTING clsname        = clsname
                                cmpname        = cmpname
                      IMPORTING is_added       = is_added
                                new_element_id = new_element_id
                                is_added_now   = is_added_now ).

      IF is_specific EQ abap_true and
         is_added_now eq abap_true.

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
    ENDIF.
    instance->type = class_type.
    r_instance = instance.
  ENDMETHOD.


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    IF sy-subrc EQ 0.

      DATA last_id TYPE i.

      IF element-clstype EQ is_class_type.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
        element_manager->famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                                     name       = element-class_name
                                                     modifiers  = z2mse_extract3=>modifier_abapglobalclass
                                           IMPORTING id         = last_id ).
      ELSEIF element-clstype EQ interface_type.
        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
        " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
        element_manager->famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                                     name       = element-class_name
                                                     modifiers  = z2mse_extract3=>modifier_abapglobalinterface
                                           IMPORTING id         = last_id ).
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        element_manager->famix_class->is_interface( element_id = last_id ).
      ELSE.
        ASSERT 1 = 2.
      ENDIF.
      DATA association TYPE z2mse_extr3_element_manager=>association_type.
      LOOP AT associations INTO association WHERE element_id1 = element_id
                                              AND association->type = z2mse_extr3_association=>parent_package_ass.
        DATA package TYPE REF TO z2mse_extr3_packages.
        package ?= element_manager->get_element( i_element_id = association-element_id2 ).
        element_manager->famix_class->set_parent_package( element_id     = last_id
                                                          parent_package = package->devclass( i_element_id = association-element_id2 ) ).

      ENDLOOP.

    ELSE.

      DATA element_comp TYPE element_comp_type.

      READ TABLE elements_comp_element_id INTO element_comp WITH KEY element_id = element_id.
      ASSERT sy-subrc EQ 0.
      CASE element_comp-cmptype.
        WHEN attribute_type.


*    DATA last_id TYPE i.!
          element_manager->famix_attribute->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).
          element_manager->famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                        parent_element = 'FAMIX.Class'
                                                        parent_name_group = 'ABAP_CLASS'
                                                        parent_name    = element_comp-clsname ).

          element_manager->famix_attribute->store_id( EXPORTING class     = element_comp-clsname
                                               attribute = element_comp-cmpname ).

*            sap_attribute->add( EXPORTING class     = class-clsname
*                                          attribute = component-cmpname ).
        WHEN method_type OR event_type.
          " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
          " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
          element_manager->famix_method->add( EXPORTING name = element_comp-cmpname IMPORTING id = last_id ).

          " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
          " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
          element_manager->famix_method->set_signature( element_id = last_id
                                         signature = element_comp-cmpname ).

          element_manager->famix_method->set_parent_type( EXPORTING element_id = last_id
                                                     parent_element = 'FAMIX.Class'
                                                     parent_name_group = 'ABAP_CLASS'
                                                     parent_name    = element_comp-clsname ).


          element_manager->famix_method->store_id( EXPORTING class  = element_comp-clsname
                                              method = element_comp-cmpname ).


*            sap_method->add( EXPORTING class  = class-clsname
*                                       method = component-cmpname ).
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD name.
    DATA: class_name TYPE seoclsname,
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

      DATA: cmpname TYPE seocmpname,
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
      DATA: found_class_name TYPE seoclsname,
            found_cmpname    TYPE seocmpname,
            found_cmptype    TYPE seocmptype,
            found_mtdtype    TYPE seomtdtype.

      TEST-SEAM seocompo.
        SELECT SINGLE clsname cmpname cmptype mtdtype FROM seocompo
          INTO ( found_class_name, found_cmpname, found_cmptype, found_mtdtype ) WHERE clsname = clsname
                                                                                   AND cmpname = cmpname.
      END-TEST-SEAM.

      IF found_class_name IS NOT INITIAL.
        is_added = abap_true.
      ENDIF.

      IF is_added EQ abap_true.

        new_element_id = _add_single_component_to_class( i_found_class_name = found_class_name
                                                         i_found_cmpname    = found_cmpname
                                                         i_found_cmptype    = found_cmptype
                                                         i_found_mtdtype    = found_mtdtype ).
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
              interface_class_component  TYPE ty_class_component.

        me->add( EXPORTING class            = relation-refclsname
                 IMPORTING is_added         = is_added
                           new_element_id   = new_element_id
                           class_components = interface_class_components ).

        LOOP AT interface_class_components INTO interface_class_component.

          new_element_id = _add_single_component_to_class( i_found_class_name = clsname
                                                           i_found_cmpname    = |{ interface_class_component-clsname }~{ interface_class_component-cmpname }|
                                                           i_found_cmptype    = interface_class_component-cmptype
                                                           i_found_mtdtype    = interface_class_component-mtdtype ).

          DATA interface_element_id TYPE z2mse_extr3_element_manager=>element_id_type .

          me->add_component( EXPORTING clsname        = interface_class_component-clsname
                                       cmpname        = interface_class_component-cmpname
                                       is_specific    = abap_false
                              IMPORTING "*              is_added       =
                                        new_element_id = interface_element_id ).

          IF interface_class_component-cmptype EQ attribute_type.

            " Connections between attributes are not expressible in FAMIX, or?

*            access->add( EXPORTING accessed_element_id1  = new_element_id
*                                   accessing_element_id2 = interface_element_id ).

          ELSE.

            invocation->add( EXPORTING invoced_element_id1  = new_element_id
                                       invocing_element_id2 = interface_element_id ).

          ENDIF.


        ENDLOOP.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _add_single_component_to_class.

    " Add single component to class

    DATA element_comp2 TYPE element_comp_type.

    r_new_element_id = element_manager->add_element( element = me
                                                     is_specific = abap_false ).
    element_comp2-element_id = r_new_element_id.
    element_comp2-clsname = i_found_class_name.
    element_comp2-cmpname = i_found_cmpname.
    element_comp2-cmptype = i_found_cmptype.
    element_comp2-mtdtype = i_found_mtdtype.

    INSERT element_comp2 INTO TABLE elements_comp_element_id .
    INSERT element_comp2 INTO TABLE elements_comp_clsname_cmpname .

  ENDMETHOD.
ENDCLASS.
