"! Extract informations on ABAP classes
"! Will be tested by transferring test data to the constructor.
"! Has Unit Tests.
CLASS z2mse_extr_classes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    "! use as replacement for ty_component_long
    "! Required because in case of interfaces the name of the interface is together with a ~ part of the name
    TYPES ty_component_long TYPE c LENGTH 61.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test.
    TYPES ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.
    TYPES: BEGIN OF ty_seoclass_test,
             clsname TYPE seoclsname,
           END OF ty_seoclass_test.
    TYPES ty_t_seoclass_test TYPE HASHED TABLE OF ty_seoclass_test WITH UNIQUE KEY clsname.

    TYPES: BEGIN OF ty_class,
             clsname  TYPE seoclsname,
             clstype  TYPE seoclstype,
             devclass TYPE tadir-devclass,
             exists   TYPE abap_bool,
           END OF ty_class.
    TYPES: ty_classes TYPE HASHED TABLE OF ty_class WITH UNIQUE KEY clsname.

    TYPES: BEGIN OF ty_seocompo_test,
             clsname TYPE seoclsname,
             cmpname TYPE seocmpname,
             cmptype TYPE seocmptype,
           END OF ty_seocompo_test.
    TYPES ty_t_seocompo_test TYPE HASHED TABLE OF ty_seocompo_test WITH UNIQUE KEY clsname cmpname.

    TYPES: BEGIN OF ty_seometarel_test,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
             version    TYPE seoversion,
             state      TYPE seostate,
             reltype    TYPE seoreltype,
           END OF ty_seometarel_test.
    TYPES ty_t_seometarel_test TYPE HASHED TABLE OF ty_seometarel_test WITH UNIQUE KEY clsname refclsname version.

    TYPES: BEGIN OF ty_class_component,
             clsname TYPE seoclsname,
             cmpname TYPE ty_component_long,
             cmptype TYPE seocmptype,
           END OF ty_class_component.
    TYPES ty_class_components TYPE SORTED TABLE OF ty_class_component WITH UNIQUE KEY clsname cmpname cmptype.
    TYPES ty_class_components_hashed TYPE HASHED TABLE OF ty_class_component WITH UNIQUE KEY clsname cmpname cmptype.

    CONSTANTS: class_type     TYPE seoclstype VALUE 0,
               interface_type TYPE seoclstype VALUE 1,
               attribute_type TYPE seocmptype VALUE 0,
               method_type    TYPE seocmptype VALUE 1,
               event_type     TYPE seocmptype VALUE 2,
               tadir_clas     TYPE tadir-object VALUE 'CLAS' ##NO_TEXT,
               tadir_intf     TYPE tadir-object VALUE 'INTF' ##NO_TEXT.
    METHODS constructor
      IMPORTING
        !tadir_test     TYPE ty_t_tadir_test OPTIONAL
        seoclass_test   TYPE ty_t_seoclass_test OPTIONAL
        seocompo_test   TYPE ty_t_seocompo_test OPTIONAL
        seometarel_test TYPE ty_t_seometarel_test OPTIONAL.
    "! Call once to select all classes that are in a list of packages
    METHODS select_classes_by_packages
      IMPORTING
        packages TYPE z2mse_extr_packages=>ty_packages.
    "! Add classes by a list of components
    METHODS select_classes_by_components
      IMPORTING
        components TYPE ty_class_components_hashed.

    "! Add all selected components to the model. Should be called only once
    METHODS add_to_model
      IMPORTING
        famix_package   TYPE REF TO z2mse_famix_package
        famix_class     TYPE REF TO z2mse_famix_class
        famix_method    TYPE REF TO z2mse_famix_method
        famix_attribute TYPE REF TO z2mse_famix_attribute
        famix_invocation TYPE REF TO z2mse_famix_invocation
        famix_access     TYPE REF TO z2mse_famix_access.
    "! Returns components. Returns these Components only once
    METHODS get_comp_to_do_where_used
      RETURNING VALUE(components) TYPE z2mse_extr_classes=>ty_class_components.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_tadir_obj_name,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir_obj_name.

    TYPES: ty_tadir_obj_names TYPE HASHED TABLE OF ty_tadir_obj_name WITH UNIQUE KEY obj_name.

    "! Filled during tests
    DATA g_tadir_test TYPE z2mse_extr_classes=>ty_t_tadir_test.
    "! Filled during tests
    DATA g_seoclass_test TYPE z2mse_extr_classes=>ty_t_seoclass_test.
    "! Filled during tests
    DATA g_seocompo_test TYPE z2mse_extr_classes=>ty_t_seocompo_test.
    "! Filled during tests
    DATA g_seometarel_test TYPE z2mse_extr_classes=>ty_t_seometarel_test.
    "! A list of all primarily selected and existing classes or interfaces
    DATA g_selected_classes TYPE z2mse_extr_classes=>ty_classes.
    "! A list of all components of primarily selected and existing classes or interfaces. Only if not yet transfered to where used analysis
    DATA g_selected_components_new TYPE z2mse_extr_classes=>ty_class_components_hashed.
    "! A list of all components of primarily selected and existing classes or interfaces
    DATA g_selected_components TYPE z2mse_extr_classes=>ty_class_components_hashed.
    DATA g_is_test TYPE abap_bool.
    "! Checks whether a class exists. There can be TADIR entries for not existing classes.
    METHODS _check_existence_and_add_intf
      CHANGING classes TYPE z2mse_extr_classes=>ty_classes.
    METHODS _read_class_details
      IMPORTING classes           TYPE z2mse_extr_classes=>ty_classes
      RETURNING VALUE(components) TYPE z2mse_extr_classes=>ty_class_components.
    METHODS _select_from_tadir
      IMPORTING
                i_packages              TYPE z2mse_extr_packages=>ty_packages
      RETURNING VALUE(selected_classes) TYPE z2mse_extr_classes=>ty_classes.
    METHODS _select_from_tadir_by_comp
      IMPORTING
                obj_names               TYPE ty_tadir_obj_names
      RETURNING VALUE(selected_classes) TYPE z2mse_extr_classes=>ty_classes.

    METHODS _add_classes_to_model
      IMPORTING
        famix_package    TYPE REF TO z2mse_famix_package
        famix_class      TYPE REF TO z2mse_famix_class
        famix_method     TYPE REF TO z2mse_famix_method
        famix_attribute  TYPE REF TO z2mse_famix_attribute
        famix_invocation TYPE REF TO z2mse_famix_invocation
        famix_access     TYPE REF TO z2mse_famix_access
        classes          TYPE z2mse_extr_classes=>ty_classes
        components       TYPE z2mse_extr_classes=>ty_class_components_hashed.
    METHODS add_and_sort_to_classes_table
      IMPORTING
        i_tadirvalues      TYPE z2mse_extr_classes=>ty_t_tadir_test
      CHANGING
        c_selected_classes TYPE z2mse_extr_classes=>ty_classes.
ENDCLASS.



CLASS Z2MSE_EXTR_CLASSES IMPLEMENTATION.


  METHOD add_and_sort_to_classes_table.

    DATA tadirline TYPE z2mse_extr_classes=>ty_tadir_test.

    "Add and sort to classes table
    DATA class TYPE ty_class.

    LOOP AT i_tadirvalues INTO tadirline.

      CLEAR class.

      class-clsname = tadirline-obj_name.
      CASE tadirline-object.
        WHEN tadir_clas.
          class-clstype = class_type.
        WHEN tadir_intf.
          class-clstype = interface_type.
        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      class-devclass = tadirline-devclass.

      INSERT class INTO TABLE c_selected_classes.

    ENDLOOP.

    SORT c_selected_classes BY clsname.

  ENDMETHOD.


  METHOD add_to_model.

    DATA line LIKE LINE OF g_selected_components_new.

    LOOP AT g_selected_components_new INTO line.
      INSERT line INTO TABLE g_selected_components.
    ENDLOOP.

    me->_add_classes_to_model( EXPORTING famix_package    = famix_package
                                          famix_class      = famix_class
                                          famix_method    = famix_method
                                          famix_attribute  = famix_attribute
                                          famix_invocation = famix_invocation
                                          famix_access = famix_access
                                          classes       = g_selected_classes
                                          components    = g_selected_components ).

*    me->_add_classes_to_model( EXPORTING famix_package    = famix_package
*                                          famix_class      = famix_class
*                                          famix_method    = famix_method
*                                          famix_attribute  = famix_attribute
*                                          classes       = g_add_classes
*                                          components    = g_add_components ).

  ENDMETHOD.


  METHOD constructor.
    IF tadir_test IS SUPPLIED
    OR seoclass_test IS SUPPLIED
    OR seocompo_test IS SUPPLIED
    OR seometarel_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_seoclass_test = seoclass_test.
      g_seocompo_test = seocompo_test.
      g_seometarel_test = seometarel_test.
      g_is_test = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_comp_to_do_where_used.

    DATA line LIKE LINE OF g_selected_components_new.

    LOOP AT g_selected_components_new INTO line.
      READ TABLE g_selected_components TRANSPORTING NO FIELDS WITH TABLE KEY clsname = line-clsname cmpname = line-cmpname cmptype = line-cmptype.
      IF sy-subrc <> 0.
        INSERT line INTO TABLE components.
      ENDIF.
    ENDLOOP.

    LOOP AT g_selected_components_new INTO line.
      INSERT line INTO TABLE g_selected_components.
    ENDLOOP.
    CLEAR g_selected_components_new.

  ENDMETHOD.


  METHOD select_classes_by_components.

    DATA component LIKE LINE OF components.

    DATA: obj_name  TYPE ty_tadir_obj_name,
          obj_names TYPE ty_tadir_obj_names.

    LOOP AT components INTO component.
      READ TABLE g_selected_classes TRANSPORTING NO FIELDS WITH TABLE KEY clsname = component-clsname.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.
      obj_name = component-clsname.
      INSERT obj_name INTO TABLE obj_names.
    ENDLOOP.
    DATA: l_add_classes TYPE ty_classes,
          l_added_class TYPE ty_class.
    l_add_classes = me->_select_from_tadir_by_comp( obj_names = obj_names ).

    _check_existence_and_add_intf( CHANGING classes = l_add_classes ).

    LOOP AT l_add_classes INTO l_added_class.
      INSERT l_added_class INTO TABLE g_selected_classes.
    ENDLOOP.
    g_selected_components_new = _read_class_details(  l_add_classes ).

  ENDMETHOD.


  METHOD select_classes_by_packages.

    g_selected_classes = _select_from_tadir( packages ).
    _check_existence_and_add_intf( CHANGING classes = g_selected_classes ).
    g_selected_components_new = _read_class_details(  g_selected_classes ).

  ENDMETHOD.


  METHOD _add_classes_to_model.

    DATA: class LIKE LINE OF g_selected_classes.
    DATA last_id TYPE i.
    LOOP AT classes INTO class.
      famix_package->add( name = class-devclass ).

      IF class-clstype EQ class_type.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
        famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                      name                   = class-clsname
                                      modifiers              = z2mse_extract_sap2=>modifier_abapglobalclass
                            IMPORTING id         = last_id ).
*        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
*                                  name       = class-clsname
*                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalclass
*                        IMPORTING id         = last_id ).
        famix_class->set_parent_package( element_id     = last_id
                                       parent_package = class-devclass ).
      ELSEIF class-clstype EQ interface_type.
        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
        " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
        famix_class->add( EXPORTING name_group             = 'ABAP_CLASS'
                                      name                   = class-clsname
                                      modifiers              = z2mse_extract_sap2=>modifier_abapglobalinterface
                            IMPORTING id         = last_id ).
*        sap_class->add( EXPORTING name_group = 'ABAP_CLASS'
*                                  name       = class-clsname
*                                  modifiers  = yrw1_mcextract_sap2=>modifier_abapglobalinterface
*                        IMPORTING id         = last_id ).
        famix_class->set_parent_package( element_id     = last_id
                                       parent_package = class-devclass ).
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        famix_class->is_interface( element_id = last_id ).
      ELSE.
        CONTINUE.
      ENDIF.

      DATA component TYPE ty_class_component.

      LOOP AT components INTO component WHERE clsname = class-clsname.

        CASE component-cmptype.
          WHEN attribute_type.


*    DATA last_id TYPE i.!
            famix_attribute->add( EXPORTING name = component-cmpname IMPORTING id = last_id ).
            famix_attribute->set_parent_type( EXPORTING element_id = last_id
                                                          parent_element = 'FAMIX.Class'
                                                          parent_name_group = 'ABAP_CLASS'
                                                          parent_name    = class-clsname ).

            famix_attribute->store_id( EXPORTING class     = class-clsname
                                                 attribute = component-cmpname ).

*            sap_attribute->add( EXPORTING class     = class-clsname
*                                          attribute = component-cmpname ).
          WHEN method_type OR event_type.
            " SAP_2_FAMIX_15        Map methods of classes to FAMIX.Method
            " SAP_2_FAMIX_16        Map methods of interfaces to FAMIX.Method
            famix_method->add( EXPORTING name = component-cmpname IMPORTING id = last_id ).

            " SAP_2_FAMIX_41      Fill the attribut signature of FAMIX.METHOD with the name of the method
            " SAP_2_FAMIX_42        Fill the attribut signature of FAMIX.METHOD with the name of the method
            famix_method->set_signature( element_id = last_id
                                           signature = component-cmpname ).

            famix_method->set_parent_type( EXPORTING element_id = last_id
                                                       parent_element = 'FAMIX.Class'
                                                       parent_name_group = 'ABAP_CLASS'
                                                       parent_name    = class-clsname ).


            famix_method->store_id( EXPORTING class  = class-clsname
                                                method = component-cmpname ).


*            sap_method->add( EXPORTING class  = class-clsname
*                                       method = component-cmpname ).
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.

      ENDLOOP.

    ENDLOOP.

    "! TBD Add implementation of interfaces here

    LOOP AT components INTO component.
      DATA: interface TYPE string,
            element   TYPE string.
      SPLIT component-cmpname AT '~' INTO interface element.

      IF element IS NOT INITIAL.

        CASE component-cmptype.
          WHEN attribute_type.

          WHEN method_type.

            DATA: class_comp_id     TYPE i,
                  interface_comp_id TYPE i.

            class_comp_id = famix_method->get_id(
               EXPORTING
*                class_name_group  =
                 class             = component-clsname
*                method_name_group =
                 method            = component-cmpname
*              RECEIVING
*                id                =
             ).

            interface_comp_id = famix_method->get_id(
*                        class_name_group  =
                        class             = interface
*                        method_name_group =
                        method            = element
                    ).

            DATA invocation_id TYPE i.
            invocation_id = famix_invocation->add( ).
            famix_invocation->set_invocation_by_reference( EXPORTING element_id = invocation_id
                                                                       sender_id     = interface_comp_id
                                                                       candidates_id = class_comp_id
                                                                       signature     = 'DUMMY' ).

          WHEN OTHERS.
            "! TBD is this error handling OK?
            ASSERT 1 = 2.
        ENDCASE.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD _check_existence_and_add_intf.

    TYPES: BEGIN OF ty_existing,
             clsname TYPE seoclsname,
           END OF ty_existing.
    TYPES ty_t_existing TYPE STANDARD TABLE OF ty_existing WITH DEFAULT KEY.
    DATA: line  TYPE ty_existing,
          table TYPE ty_t_existing.

    FIELD-SYMBOLS: <class> LIKE LINE OF classes.

*    " Build helper table with correct type for select
*    LOOP AT g_selected_tadir INTO selected_tadir.
*      line-clsname = selected_tadir-obj_name.
*      INSERT line INTO TABLE check_table.
*    ENDLOOP.

    " Select from database
    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.
        SELECT clsname FROM seoclass INTO TABLE table FOR ALL ENTRIES IN classes WHERE clsname = classes-clsname.
      ENDIF.

    ELSE.

      DATA seoclass_line TYPE ty_seoclass_test.

      LOOP AT g_seoclass_test INTO seoclass_line.
        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seoclass_line-clsname.
        IF sy-subrc EQ 0.
          line-clsname = seoclass_line-clsname.
          INSERT line INTO TABLE table.
        ENDIF.
      ENDLOOP.

    ENDIF.

    " Mark as existing
    LOOP AT table INTO line.

      READ TABLE classes ASSIGNING <class> WITH KEY clsname = line-clsname.
      IF sy-subrc EQ 0.
        <class>-exists = abap_true.
      ENDIF.

    ENDLOOP.

    DELETE classes WHERE exists = abap_false.

    " Add interfaces

    TYPES: BEGIN OF ty_interface,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
           END OF ty_interface.
    TYPES: ty_interfaces TYPE STANDARD TABLE OF ty_interface.
    DATA interface TYPE ty_interface.
    DATA interfaces TYPE ty_interfaces.

    " Select from database
    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.

        SELECT clsname refclsname FROM seometarel INTO TABLE interfaces
          FOR ALL ENTRIES IN classes
          WHERE clsname = classes-clsname
            AND version = 1 "Active
            AND state = 1 "Implemented
            AND reltype = 1. "Interface implementation

      ENDIF.

    ELSE.

      DATA seometarel         TYPE ty_seometarel_test.

      "! TBD this is duplicate code, remove by single access to database

      LOOP AT g_seometarel_test INTO seometarel
          WHERE version = 1
            AND state = 1
            AND reltype = 1.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seometarel-clsname.
        IF sy-subrc EQ 0.
          CLEAR interface.
          interface-clsname = seometarel-clsname.
          interface-refclsname = seometarel-refclsname.
          INSERT interface INTO TABLE interfaces.
        ENDIF.

      ENDLOOP.

    ENDIF.

    " Find interfaces that are not yet selected
    LOOP AT interfaces INTO interface.

      READ TABLE g_selected_classes TRANSPORTING NO FIELDS WITH TABLE KEY clsname = interface-refclsname.
      IF sy-subrc <> 0.
        "TBD Add to classes
        DATA class  TYPE ty_class.
        CLEAR class.
        class-clsname = interface-refclsname.
        class-clstype = interface_type.
        "! TBD fill package correct
        class-devclass = ''.
        class-exists = abap_true.
        INSERT class INTO TABLE classes.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD _read_class_details.

    TYPES: BEGIN OF ty_interface,
             clsname    TYPE seoclsname,
             refclsname TYPE seoclsname,
           END OF ty_interface.
    TYPES: ty_interfaces TYPE STANDARD TABLE OF ty_interface.
    DATA interface TYPE ty_interface.
    DATA interfaces TYPE ty_interfaces.

    "! The new components.
    "! In case of interfaces add methods and attributes of interface with notation
    "! interface name ~ attribute/methodname
    DATA new_comps_including_intf TYPE z2mse_extr_classes=>ty_class_components.
    DATA new_comp_including_intf TYPE z2mse_extr_classes=>ty_class_component.

    DATA new_comps_intf TYPE z2mse_extr_classes=>ty_class_components.
    DATA new_comp_intf TYPE z2mse_extr_classes=>ty_class_component.

    IF g_is_test EQ abap_false.

      IF classes IS NOT INITIAL.

        SELECT clsname cmpname cmptype
          FROM seocompo
          INTO CORRESPONDING FIELDS OF TABLE new_comps_including_intf
          FOR ALL ENTRIES IN classes
          WHERE cmptype <> 3 AND clsname = classes-clsname.

        SELECT clsname refclsname FROM seometarel INTO TABLE interfaces
          FOR ALL ENTRIES IN classes
          WHERE clsname = classes-clsname
            AND version = 1 "Active
            AND state = 1 "Implemented
            AND reltype = 1. "Interface implementation

        IF interfaces IS NOT INITIAL.

          SELECT clsname cmpname cmptype
            FROM seocompo
            INTO CORRESPONDING FIELDS OF TABLE new_comps_intf
            FOR ALL ENTRIES IN interfaces
            WHERE cmptype <> 3 AND clsname = interfaces-refclsname.

        ENDIF.

      ENDIF.
    ELSE.
      DATA: seocompo           TYPE ty_seocompo_test,
            selected_component TYPE ty_class_component,
            seometarel         TYPE ty_seometarel_test.
      LOOP AT g_seocompo_test INTO seocompo.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seocompo-clsname.
        IF sy-subrc EQ 0.
          CLEAR selected_component.
          selected_component-clsname = seocompo-clsname.
          selected_component-cmpname = seocompo-cmpname.
          selected_component-cmptype = seocompo-cmptype.
          INSERT selected_component INTO TABLE new_comps_including_intf.
        ENDIF.

      ENDLOOP.

      LOOP AT g_seometarel_test INTO seometarel
        WHERE version = 1
          AND state = 1
          AND reltype = 1.

        READ TABLE classes TRANSPORTING NO FIELDS WITH KEY clsname = seometarel-clsname.
        IF sy-subrc EQ 0.
          CLEAR interface.
          interface-clsname = seometarel-clsname.
          interface-refclsname = seometarel-refclsname.
          INSERT interface INTO TABLE interfaces.
        ENDIF.

      ENDLOOP.

      IF interfaces IS NOT INITIAL.

        LOOP AT g_seocompo_test INTO seocompo.

          READ TABLE interfaces TRANSPORTING NO FIELDS WITH KEY refclsname = seocompo-clsname.
          IF sy-subrc EQ 0.
            CLEAR selected_component.
            selected_component-clsname = seocompo-clsname.
            selected_component-cmpname = seocompo-cmpname.
            selected_component-cmptype = seocompo-cmptype.
            INSERT selected_component INTO TABLE new_comps_intf.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ENDIF.

    LOOP AT new_comps_including_intf INTO new_comp_including_intf.
      INSERT new_comp_including_intf INTO TABLE components.
    ENDLOOP.

    " Add interface attributes and methods
    DATA class  TYPE z2mse_extr_classes=>ty_class.
    DATA component  TYPE ty_class_component.
    LOOP AT interfaces INTO interface.
      LOOP AT new_comps_intf INTO new_comp_intf WHERE clsname = interface-refclsname.
        CLEAR component.
        component-clsname = interface-clsname.
        CONCATENATE interface-refclsname '~' new_comp_intf-cmpname INTO component-cmpname.
        component-cmptype = new_comp_intf-cmptype.
        INSERT component INTO TABLE components.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD _select_from_tadir.

    " Select from TADIR
    CLEAR selected_classes.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF i_packages IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN i_packages  WHERE
          pgmid = 'R3TR' AND
          devclass = i_packages-package AND
          ( object = tadir_clas OR
            object = tadir_intf ).

      ENDIF.
    ELSE.
      DATA package LIKE LINE OF i_packages.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT i_packages INTO package.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          ( object = tadir_clas OR object = tadir_intf ) AND
          devclass = package-package.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_classes_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_classes = selected_classes ).

  ENDMETHOD.


  METHOD _select_from_tadir_by_comp.

    " Select from TADIR
    CLEAR selected_classes.

    DATA: tadirline   TYPE ty_tadir_test,
          tadirvalues TYPE ty_t_tadir_test.

    IF g_is_test EQ abap_false.
      IF obj_names  IS NOT INITIAL.
        SELECT object obj_name devclass FROM tadir INTO CORRESPONDING FIELDS OF TABLE tadirvalues FOR ALL ENTRIES IN obj_names   WHERE
          pgmid = 'R3TR' AND
          obj_name = obj_names-obj_name AND
          ( object = tadir_clas OR
            object = tadir_intf ).

      ENDIF.
    ELSE.
      DATA obj_name LIKE LINE OF obj_names.
      DATA tadir_test LIKE LINE OF g_tadir_test.
      LOOP AT obj_names INTO obj_name.
        LOOP AT g_tadir_test INTO tadir_test WHERE
          ( object = tadir_clas OR object = tadir_intf ) AND
          obj_name = obj_name-obj_name.

          tadirline-object = tadir_test-object.
          tadirline-obj_name = tadir_test-obj_name.
          tadirline-devclass = tadir_test-devclass.
          INSERT tadirline INTO TABLE tadirvalues.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    add_and_sort_to_classes_table( EXPORTING i_tadirvalues = tadirvalues
                                    CHANGING c_selected_classes = selected_classes ).

  ENDMETHOD.
ENDCLASS.
