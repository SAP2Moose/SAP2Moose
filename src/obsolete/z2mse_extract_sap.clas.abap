CLASS z2mse_extract_sap DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:BEGIN OF db_table_type,
            db_table                       TYPE tabname,
            id_in_model                    TYPE i,
            id_of_dummy_attribute_in_model TYPE i,
          END OF db_table_type.
    TYPES: db_tables_type TYPE HASHED TABLE OF db_table_type WITH UNIQUE KEY db_table.
    TYPES:         ty_s_compsn                       TYPE RANGE OF tadir-obj_name.
    TYPES:         ty_s_pack                         TYPE RANGE OF tadir-devclass.
    METHODS constructor
      IMPORTING
        i_g_filter_using_package       TYPE abap_bool
        i_g_filter_using_name          TYPE abap_bool
        i_g_parameter_package_to_analz TYPE parentcl
        i_p_iprog                      TYPE abap_bool
        i_p_clas                       TYPE abap_bool
        i_p_wdyn                       TYPE abap_bool
        i_p_intf                       TYPE abap_bool
        i_p_prog                       TYPE abap_bool
        i_p_tables                     TYPE abap_bool
        i_s_compsn                     TYPE ty_s_compsn
        i_s_pack                       TYPE ty_s_pack
        i_g_param_usage_outpack_groupd TYPE abap_bool.
    METHODS extract
      EXPORTING
        mse_model           TYPE z2mse_model=>lines_type
        VALUE(nothing_done) TYPE abap_bool.
  PRIVATE SECTION.

    TYPES: BEGIN OF class_component_type,
             clsname TYPE seocompo-clsname,
             cmpname TYPE seocompo-cmpname,
             cmptype TYPE seocompo-cmptype,
           END OF class_component_type.

    TYPES component_type TYPE string.

    TYPES: BEGIN OF component_infos_type,
             component      TYPE component_type,
             component_name TYPE string,
             package        TYPE devclass,
           END OF component_infos_type.

    TYPES components_infos_type TYPE HASHED TABLE OF component_infos_type WITH UNIQUE KEY component component_name.

    TYPES: BEGIN OF map_tadir_component_type,
             object    TYPE trobjtype, " The SAP TADIR Name
             component TYPE component_type, " As called here
           END OF map_tadir_component_type.

    TYPES tadir_components_mapping_type TYPE HASHED TABLE OF map_tadir_component_type WITH UNIQUE KEY object
                                                                                WITH UNIQUE HASHED KEY comp COMPONENTS component.

    TYPES: BEGIN OF class_interface_type,
             obj_name TYPE seoclsname,
           END OF class_interface_type.

    TYPES: BEGIN OF program_type,
             program TYPE char30,
           END OF program_type.

    TYPES:BEGIN OF class_type,
            type  TYPE c LENGTH 1, " C Class, W WebDynpro
            class TYPE seoclsname,
          END OF class_type.



    TYPES: BEGIN OF inheritance_type,
             clsname    TYPE seometarel-clsname,
             refclsname TYPE seometarel-refclsname,
             reltype    TYPE seometarel-reltype,
           END OF inheritance_type.

    "! Maps the component lists from SAP (table TADIR) to the component list used in this program
    DATA g_tadir_components_mapping TYPE tadir_components_mapping_type.
    DATA g_filter_using_package TYPE abap_bool."TBD fill
    DATA g_filter_using_name TYPE abap_bool."TBD fill
    DATA g_parameter_package_to_analyze TYPE parentcl.
    DATA p_iprog TYPE abap_bool.
    DATA p_clas TYPE abap_bool.
    DATA p_wdyn TYPE abap_bool.
    DATA p_intf TYPE abap_bool.
    DATA p_prog TYPE abap_bool.
    DATA p_tables TYPE abap_bool.
    DATA s_compsn TYPE RANGE OF tadir-obj_name.
    DATA s_pack TYPE RANGE OF tadir-devclass.
    DATA g_param_usage_outpack_groupd TYPE abap_bool."TBD fill

    CONSTANTS comptype_attribute TYPE seocmptype VALUE '0'.
    CONSTANTS comptype_method TYPE seocmptype VALUE '1'.
    CONSTANTS globclass_component_key TYPE string VALUE 'GlobClass' ##NO_TEXT.
    CONSTANTS webdynpro_component_comp_key TYPE string VALUE 'WebDynproComponent' ##NO_TEXT.
    CONSTANTS globintf_component_key TYPE string VALUE 'GlobIntf' ##NO_TEXT.
    CONSTANTS abapprogram_component_key TYPE string VALUE 'ABAPProgram' ##NO_TEXT.
    CONSTANTS databasetable_component_key TYPE string VALUE 'DataBaseTable' ##NO_TEXT.
    CONSTANTS tadir_clas TYPE string VALUE 'CLAS' ##NO_TEXT.
    "! WebDynpro component
    CONSTANTS tadir_wdyn TYPE string VALUE 'WDYN' ##NO_TEXT.
    CONSTANTS tadir_intf TYPE string VALUE 'INTF' ##NO_TEXT.
    CONSTANTS tadir_prog TYPE string VALUE 'PROG' ##NO_TEXT.
    CONSTANTS tadir_tabl TYPE string VALUE 'TABL' ##NO_TEXT.
    CONSTANTS modifier_abapglobalclass TYPE string VALUE 'ABAPGlobalClass' ##NO_TEXT.
    CONSTANTS modifier_abapglobalinterface TYPE string VALUE 'ABAPGlobalInterface' ##NO_TEXT.
    CONSTANTS modifier_webdynpro_component TYPE string VALUE 'ABAPWebDynproComponent'.





    METHODS _set_default_language
      IMPORTING
        model TYPE REF TO z2mse_model.

    TYPES: BEGIN OF package_type,
             devclass TYPE devclass,
           END OF package_type.
    TYPES:
      processed_packages_type TYPE HASHED TABLE OF package_type WITH UNIQUE KEY devclass.
    METHODS _determine_packages_to_analyze
      IMPORTING
        sap_package               TYPE REF TO z2mse_sap_package
        package_first             TYPE tdevc
      RETURNING
        VALUE(processed_packages) TYPE processed_packages_type.
    TYPES:
      classes_type  TYPE STANDARD TABLE OF class_interface_type WITH DEFAULT KEY,
      programs_type TYPE STANDARD TABLE OF program_type WITH DEFAULT KEY.
    METHODS _analyze_components
      IMPORTING
        components_infos TYPE components_infos_type
      EXPORTING
        VALUE(classes)   TYPE classes_type
        VALUE(programs)  TYPE programs_type
        VALUE(db_tables) TYPE db_tables_type.
    METHODS _read_all_programs
      IMPORTING
        sap_package      TYPE REF TO z2mse_sap_package
        sap_program      TYPE REF TO z2mse_sap_program
        components_infos TYPE components_infos_type
        programs         TYPE programs_type
      CHANGING
        model            TYPE REF TO z2mse_model.
    TYPES:existing_classes_type TYPE HASHED TABLE OF class_type WITH UNIQUE KEY type class.
    METHODS _add_classes_to_model
      IMPORTING
        sap_package      TYPE REF TO z2mse_sap_package
        sap_class        TYPE REF TO Z2MSE_sap_class
        components_infos TYPE components_infos_type
        existing_classes TYPE existing_classes_type.
    METHODS _add_tables_to_model
      IMPORTING
        sap_package      TYPE REF TO z2mse_sap_package
        components_infos TYPE components_infos_type
      CHANGING
        db_tables        TYPE db_tables_type
        sap_db_table     TYPE REF TO Z2MSE_sap_db_table.
    METHODS _determine_inheritances_betwee
      IMPORTING
        sap_inheritance  TYPE REF TO Z2MSE_sap_inheritance
        existing_classes TYPE existing_classes_type.
    TYPES: class_components_type   TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.
    METHODS _determine_class_components
      IMPORTING
        existing_classes        TYPE existing_classes_type
      RETURNING
        VALUE(class_components) TYPE class_components_type.
    METHODS _add_to_class_components_to_mo
      IMPORTING
        class_components TYPE class_components_type
        sap_method       TYPE REF TO z2mse_sap_method
        sap_attribute    TYPE REF TO Z2MSE_sap_attribute.
    METHODS _determine_usage_of_methods
      IMPORTING
                sap_class                   TYPE REF TO Z2MSE_sap_class
                class_components            TYPE class_components_type
                sap_package                 TYPE REF TO z2mse_sap_package
                sap_method                  TYPE REF TO z2mse_sap_method
                sap_attribute               TYPE REF TO Z2MSE_sap_attribute
                sap_invocation              TYPE REF TO z2mse_sap_invocation
                sap_access                  TYPE REF TO Z2MSE_sap_access
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.

    METHODS _determine_usage_of_db_tables
      IMPORTING
                sap_class                   TYPE REF TO Z2MSE_sap_class
                class_components            TYPE class_components_type
                sap_package                 TYPE REF TO z2mse_sap_package
                sap_method                  TYPE REF TO z2mse_sap_method
                sap_attribute               TYPE REF TO Z2MSE_sap_attribute
                sap_invocation              TYPE REF TO z2mse_sap_invocation
                sap_access                  TYPE REF TO Z2MSE_sap_access
                db_tables                   TYPE db_tables_type
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.

    "! Determine usages for components
    "! If using components are not part of the model, they are either added or replaced by a dummy component
    "! Either provide class_component or db_table
    METHODS _determine_usages
      IMPORTING
                sap_class                   TYPE REF TO Z2MSE_sap_class
                class_component             TYPE class_component_type OPTIONAL
                db_table                    TYPE db_table_type OPTIONAL
                sap_package                 TYPE REF TO z2mse_sap_package
                sap_method                  TYPE REF TO z2mse_sap_method
                sap_invocation              TYPE REF TO z2mse_sap_invocation
                sap_access                  TYPE REF TO Z2MSE_sap_access
                used                        TYPE i
      RETURNING VALUE(new_components_infos) TYPE components_infos_type.
    TYPES:
      classes_4_type        TYPE STANDARD TABLE OF class_interface_type WITH DEFAULT KEY.
    METHODS _read_all_classes
      IMPORTING
        classes                 TYPE classes_4_type
      RETURNING
        VALUE(existing_classes) TYPE existing_classes_type.
    "! Evaluate user selection and return initial list of objects to analyze
    "! @parameter nothing_selected | nothing is selected
    METHODS _select_requested_components
      IMPORTING
        sap_package           TYPE REF TO z2mse_sap_package
        package_to_analyze    TYPE parentcl
        select_by_top_package TYPE abap_bool
      EXPORTING
        components_infos      TYPE components_infos_type
        nothing_selected      TYPE abap_bool.
    METHODS _handle_used_by_class
      IMPORTING
        i_sap_class                   TYPE REF TO Z2MSE_sap_class
        i_class_component             TYPE class_component_type
        i_sap_package                 TYPE REF TO z2mse_sap_package
        i_sap_method                  TYPE REF TO z2mse_sap_method
        i_sap_invocation              TYPE REF TO z2mse_sap_invocation
        i_sap_access                  TYPE REF TO Z2MSE_sap_access
        i_used                        TYPE i
        i_class_component_is_supplied TYPE abap_bool
        ib_table_is_supplied          TYPE abap_bool
        i_using_class                 TYPE string
        i_using_method                TYPE string
        i_modifier_of_using_class     TYPE string
      RETURNING
        VALUE(r_new_components_infos) TYPE components_infos_type.
    METHODS _convert_compnt_to_famix_model
      IMPORTING
        component       TYPE string
      RETURNING
        VALUE(modifier) TYPE string.

ENDCLASS.

CLASS z2mse_extract_sap IMPLEMENTATION.

  METHOD constructor.
    g_filter_using_package = i_g_filter_using_package.
    g_filter_using_name = i_g_filter_using_name.
    g_parameter_package_to_analyze = i_g_parameter_package_to_analz.
    p_iprog = i_p_iprog.
    p_clas = i_p_clas.
    p_wdyn = i_p_wdyn.
    p_intf = i_p_intf.
    p_prog = i_p_prog.
    p_tables = i_p_tables.
    s_compsn = i_s_compsn.
    s_pack = i_s_pack.
    g_param_usage_outpack_groupd = i_g_param_usage_outpack_groupd.
  ENDMETHOD.

  METHOD extract.

    TYPES:BEGIN OF package_type,
            devclass TYPE devclass,
          END OF package_type.

    DATA components_infos TYPE components_infos_type.
    DATA new_components_infos TYPE components_infos_type.
    DATA processed_components_infos TYPE components_infos_type.
    DATA classes TYPE STANDARD TABLE OF class_interface_type.
    DATA programs TYPE STANDARD TABLE OF program_type.
    DATA db_tables TYPE z2mse_extract_sap=>db_tables_type.
    DATA existing_classes TYPE HASHED TABLE OF class_type WITH UNIQUE KEY type class.

    DATA class_components TYPE HASHED TABLE OF class_component_type WITH UNIQUE KEY clsname cmpname.

    " Do not use singleton pattern, but define each instance only one time at the start. Multiple instanciation allowed unless
    " specified for the class

    DATA model            TYPE REF TO z2mse_model.
    CREATE OBJECT model.
    DATA sap_package     TYPE REF TO z2mse_sap_package.
    CREATE OBJECT sap_package EXPORTING model = model.
    DATA sap_program     TYPE REF TO z2mse_sap_program.
    CREATE OBJECT sap_program EXPORTING model = model.
    DATA sap_class TYPE REF TO Z2MSE_sap_class.
    CREATE OBJECT sap_class EXPORTING model = model.
    DATA sap_inheritance TYPE REF TO Z2MSE_sap_inheritance.
    CREATE OBJECT sap_inheritance EXPORTING model = model.
    DATA sap_method TYPE REF TO z2mse_sap_method.
    CREATE OBJECT sap_method EXPORTING model = model.
    DATA sap_attribute   TYPE REF TO Z2MSE_sap_attribute.
    CREATE OBJECT sap_attribute EXPORTING model = model.
    DATA sap_invocation  TYPE REF TO z2mse_sap_invocation.
    CREATE OBJECT sap_invocation EXPORTING model = model.
    DATA sap_access      TYPE REF TO Z2MSE_sap_access.
    CREATE OBJECT sap_access EXPORTING model = model.
    DATA check_famix_model TYPE REF TO Z2MSE_check_famix_model.
    CREATE OBJECT check_famix_model.

    DATA sap_db_table TYPE REF TO Z2MSE_sap_db_table.
    CREATE OBJECT sap_db_table EXPORTING model = model.

    " Set TADIR mapping
    DATA ls_mapping TYPE map_tadir_component_type. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_mapping.
    ls_mapping-object = tadir_clas.
    ls_mapping-component = globclass_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_wdyn.
    ls_mapping-component = webdynpro_component_comp_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_intf.
    ls_mapping-component = globintf_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_prog.
    ls_mapping-component = abapprogram_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    CLEAR ls_mapping.
    ls_mapping-object = tadir_tabl.
    ls_mapping-component = databasetable_component_key.
    INSERT ls_mapping INTO TABLE g_tadir_components_mapping.

    _set_default_language( model ).

    DATA nothing_selected TYPE abap_bool.

    IF g_filter_using_package EQ abap_true.
      DATA select_by_top_package TYPE boolean.
      select_by_top_package = abap_true.
    ELSEIF g_filter_using_name EQ abap_true.
      select_by_top_package = abap_false.
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
    _select_requested_components( EXPORTING  sap_package          = sap_package
                                             select_by_top_package = select_by_top_package
                                             package_to_analyze    = g_parameter_package_to_analyze
                                   IMPORTING components_infos      = components_infos
                                             nothing_selected      = nothing_selected ).



    " Select requested components by complex filters





    IF nothing_selected EQ abap_true.
      nothing_done = abap_true.
      RETURN.
    ENDIF.

    WHILE lines( components_infos ) <> 0.

      _analyze_components( EXPORTING components_infos = components_infos
                           IMPORTING classes          = classes
                                     programs         = programs
                                     db_tables        = db_tables ).

      "TBD finalize issue8 add db tables to extractor

      _read_all_programs( EXPORTING sap_package    = sap_package
                                    sap_program      = sap_program
                                    components_infos = components_infos
                                    programs         = programs
                           CHANGING model = model ).

      existing_classes = _read_all_classes( classes ).

      _add_classes_to_model( sap_package     = sap_package
                             sap_class       = sap_class
                             components_infos  = components_infos
                             existing_classes  = existing_classes ).

      _add_tables_to_model( EXPORTING sap_package      = sap_package
                                      components_infos = components_infos
                             CHANGING db_tables        = db_tables
                                      sap_db_table     = sap_db_table ).

      _determine_inheritances_betwee( sap_inheritance = sap_inheritance
                                      existing_classes  = existing_classes ).

      class_components = _determine_class_components( existing_classes ).

      _add_to_class_components_to_mo( class_components = class_components
                                      sap_method       = sap_method
                                      sap_attribute    = sap_attribute ).

      new_components_infos = _determine_usage_of_methods( sap_class        = sap_class
                                                          class_components = class_components
                                                          sap_package      = sap_package
                                                          sap_method       = sap_method
                                                          sap_attribute    = sap_attribute
                                                          sap_invocation   = sap_invocation
                                                          sap_access       = sap_access ).

      " SAP_2_FAMIX_55      Determine Usages of database tables by class methods
      INSERT LINES OF _determine_usage_of_db_tables( sap_class        = sap_class
                                                           class_components = class_components
                                                           sap_package      = sap_package
                                                           sap_method       = sap_method
                                                           sap_attribute    = sap_attribute
                                                           sap_invocation   = sap_invocation
                                                           sap_access       = sap_access
                                                           db_tables        = db_tables ) INTO TABLE new_components_infos.

      " Determine package for new components

      " TBD Find more performant solution

      FIELD-SYMBOLS <component_infos> LIKE LINE OF new_components_infos.
      LOOP AT new_components_infos ASSIGNING <component_infos>.
        "READ
        DATA object TYPE trobjtype.
        DATA ls_tadir LIKE LINE OF g_tadir_components_mapping.
        READ TABLE g_tadir_components_mapping
              INTO ls_tadir
              WITH KEY component  = <component_infos>-component.
        ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing
        object = ls_tadir-object.

        TEST-SEAM select_devclass_tadir_object.

          SELECT SINGLE devclass FROM tadir
            INTO <component_infos>-package
           WHERE pgmid = 'R3TR'
             AND object = object
             AND obj_name = <component_infos>-component_name.

        END-TEST-SEAM.

        IF sy-subrc <> 0. "OK
          " TBD
          " Report errors
          DELETE new_components_infos WHERE component = <component_infos>-component
                                        AND component_name = <component_infos>-component_name.
        ENDIF.

      ENDLOOP.

      INSERT LINES OF components_infos INTO TABLE processed_components_infos.

      CLEAR components_infos.

      " Add parent package for new global classes

      LOOP AT new_components_infos ASSIGNING <component_infos>.

        IF <component_infos>-component EQ globclass_component_key
         OR <component_infos>-component EQ globintf_component_key
         OR <component_infos>-component EQ webdynpro_component_comp_key.
          " GlobClass
          DATA: id_of_new_component TYPE i.

          sap_class->add(
            EXPORTING
              name                   = <component_infos>-component_name
              modifiers              = _convert_compnt_to_famix_model( <component_infos>-component )
            IMPORTING
              id                     = id_of_new_component ).

          sap_package->add( name = <component_infos>-package ).

          sap_class->set_parent_package( EXPORTING element_id     = id_of_new_component
                                                   parent_package = <component_infos>-package ).
        ELSE.

          TEST-SEAM write_error_no_determ_parent_p.

            FORMAT COLOR COL_NEGATIVE.

            WRITE: / 'For new element of type ', <component_infos>-component, 'no determination of parent package specified'.

            FORMAT COLOR COL_BACKGROUND.

          END-TEST-SEAM.

        ENDIF.

      ENDLOOP.

      " SAP_2_FAMIX_48      Allow to select all using objects
      " Fullfilled by adding new_components_infos to components_infos and repeating the analysis in the while loop

      FIELD-SYMBOLS <component_infos_2> LIKE LINE OF new_components_infos.
      LOOP AT new_components_infos ASSIGNING <component_infos_2>.

        READ TABLE processed_components_infos TRANSPORTING NO FIELDS WITH TABLE KEY component = <component_infos_2>-component component_name = <component_infos_2>-component_name.

        IF sy-subrc <> 0. "OK

          INSERT <component_infos_2> INTO TABLE components_infos.

        ENDIF.

      ENDLOOP.

    ENDWHILE.



    " Add parent packages for packages

    DATA packages TYPE sap_package->packages_type.
    packages = sap_package->get_all_packages( ).
    IF packages IS NOT INITIAL.
      TYPES: BEGIN OF devclass_type,
               devclass TYPE tdevc-devclass,
             END OF devclass_type.
      DATA packages_with_type_devclass TYPE STANDARD TABLE OF devclass_type WITH KEY devclass.


      DATA package LIKE LINE OF packages.
      LOOP AT packages INTO package.
        DATA ls_package_with_type_devclass LIKE LINE OF packages_with_type_devclass.
        CLEAR ls_package_with_type_devclass.
        ls_package_with_type_devclass-devclass = package.
        APPEND ls_package_with_type_devclass TO packages_with_type_devclass.

      ENDLOOP.
      TYPES: BEGIN OF packages_info_type,
               devclass TYPE tdevc-devclass,
               parentcl TYPE tdevc-parentcl,
             END OF  packages_info_type.
      DATA package_info TYPE packages_info_type.
      DATA packages_info TYPE STANDARD TABLE OF packages_info_type WITH DEFAULT KEY.

      TEST-SEAM select_devclass_parentcl_tdevc.

        SELECT  devclass parentcl FROM tdevc INTO TABLE packages_info FOR ALL ENTRIES IN packages_with_type_devclass
          WHERE devclass = packages_with_type_devclass-devclass.

      END-TEST-SEAM.

      LOOP AT packages_info INTO package_info.
        IF package_info-parentcl IS NOT INITIAL.
          sap_package->add( name = package_info-parentcl ). " So that parent class can always be assigned
          sap_package->add( name = package_info-devclass ).
          sap_package->set_parent_package( this_package = package_info-devclass
                                           parent_package = package_info-parentcl ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    TEST-SEAM check_famix_model.

      check_famix_model->check( model = model ).

    END-TEST-SEAM.

    model->make_mse( IMPORTING mse_model = mse_model ).

  ENDMETHOD.

  METHOD _determine_usages.

    DATA where_used_name TYPE eu_lname.
    DATA class_component_is_supplied TYPE abap_bool.
    DATA db_table_is_supplied TYPE abap_bool.
    IF class_component IS SUPPLIED.
      class_component_is_supplied = abap_true.
      CASE class_component-cmptype.
        WHEN comptype_method.

          " SAP_2_FAMIX_17      Determine usage of class methods by programs and classes
          " SAP_2_FAMIX_18      Determine usage of interface methods by programs and classes

          where_used_name = class_component-clsname && |\\ME:| && class_component-cmpname.
          DATA where_used_components TYPE STANDARD TABLE OF wbcrossgt.

          TEST-SEAM select_wbcrossgt_me.

            SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'ME' AND name = where_used_name.

          END-TEST-SEAM.

        WHEN comptype_attribute.

          " SAP_2_FAMIX_19      Determine usage of class attributes by programs and classes
          " SAP_2_FAMIX_20      Determine usage of interface attributes by programs and classes

          where_used_name = class_component-clsname && |\\DA:| && class_component-cmpname.

          TEST-SEAM select_wbcrossgt_da.

            SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'DA' AND name = where_used_name.

          END-TEST-SEAM.

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
    ELSEIF db_table IS SUPPLIED.
      db_table_is_supplied = abap_true.
      where_used_name = db_table-db_table.

      TEST-SEAM select_wbcrossgt_ty.

        SELECT * FROM wbcrossgt INTO TABLE where_used_components WHERE otype = 'TY' AND name = where_used_name.

      END-TEST-SEAM.

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

    FIELD-SYMBOLS <where_used_component> LIKE LINE OF where_used_components.
    LOOP AT where_used_components ASSIGNING <where_used_component>.

      DATA: using_class TYPE string.
      DATA: using_method TYPE string.
      DATA: modifier_of_using_class TYPE string.

      DATA ls_mtdkey TYPE seocpdkey.

      TEST-SEAM seo_method_get_name_by_include.

        CALL FUNCTION 'SEO_METHOD_GET_NAME_BY_INCLUDE'
          EXPORTING
            progname = <where_used_component>-include
          IMPORTING
            mtdkey   = ls_mtdkey.

      END-TEST-SEAM.

      IF ls_mtdkey IS NOT INITIAL.
*        FIND FIRST OCCURRENCE OF '~' IN ls_mtdkey-cpdname.
*        if sy-subrc eq 0.
        " This is an interface. Reverse the usage direction
        " SAP_2_FAMIX_64      Methods that implement an interface are used by the interface method


*        else.
        using_class = ls_mtdkey-clsname.

        " Used by method

        IF ls_mtdkey-cpdname IS INITIAL.
          using_method = 'DUMMY'.
        ELSE.
          using_method = ls_mtdkey-cpdname.
        ENDIF.

        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'

        modifier_of_using_class = modifier_abapglobalclass.

*        ENDIF.

        INSERT LINES OF _handle_used_by_class(
             i_sap_class                   = sap_class
             i_class_component             = class_component
             i_sap_package                 = sap_package
             i_sap_method                  = sap_method
             i_sap_invocation              = sap_invocation
             i_sap_access                  = sap_access
             i_used                        = used
             i_class_component_is_supplied = class_component_is_supplied
             ib_table_is_supplied          = db_table_is_supplied
             i_using_class                 = using_class
             i_using_method                = using_method
             i_modifier_of_using_class     = modifier_of_using_class ) INTO TABLE new_components_infos.

      ELSE.
        "Check for usage in Web Dynpro ABAP
        DATA ls_wd_sourcemap TYPE wdy_wb_sourcemap.

        TEST-SEAM wdy_wb_sourcemap.

          SELECT SINGLE * FROM wdy_wb_sourcemap INTO ls_wd_sourcemap WHERE relid = 'LI' AND inclname = <where_used_component>-include AND srtf2 = 0.

        END-TEST-SEAM.

        IF sy-subrc EQ 0.

          using_class = ls_wd_sourcemap-component_name.
          using_method = ls_wd_sourcemap-controller_name.
          modifier_of_using_class = modifier_webdynpro_component.

          INSERT LINES OF _handle_used_by_class(
                i_sap_class                   = sap_class
                i_class_component             = class_component
                i_sap_package                 = sap_package
                i_sap_method                  = sap_method
                i_sap_invocation              = sap_invocation
                i_sap_access                  = sap_access
                i_used                        = used
                i_class_component_is_supplied = class_component_is_supplied
                ib_table_is_supplied          = db_table_is_supplied
                i_using_class                 = using_class
                i_using_method                = using_method
                i_modifier_of_using_class     = modifier_of_using_class ) INTO TABLE new_components_infos.

        ELSE.

          " TBD Implement other usages
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_default_language.

    " Set default language

    DATA famix_custom_source_language TYPE REF TO Z2MSE_famix_custom_source_lng.
    CREATE OBJECT famix_custom_source_language EXPORTING model = model.

    famix_custom_source_language->add( name = 'ABAP' ).

    " Do not assign any entities to ABAP, because otherwise this will not be the default language anymore
    " So do not do this for ABAP, but maybe for another language
    " famix_package->set_declared_source_language( EXPORTING source_language_element = 'FAMIX.CustomSourceLanguage'
    "                                                        source_language_name    = 'ABAP' ).

  ENDMETHOD.


  METHOD _determine_packages_to_analyze.

    " Determine packages to analyze

    "! A temporal helper table used to find all packages (development classes) in the selection
    DATA temp_packages_to_search TYPE STANDARD TABLE OF package_type.

    sap_package->add( name = package_first-devclass ).

    DATA ls_processed_package LIKE LINE OF processed_packages. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_processed_package.
    ls_processed_package-devclass = package_first-devclass.
    INSERT ls_processed_package INTO TABLE processed_packages.

    DATA ls_temp_package_to_search LIKE LINE OF temp_packages_to_search. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_temp_package_to_search.
    ls_temp_package_to_search-devclass = g_parameter_package_to_analyze.
    INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.
    WHILE temp_packages_to_search IS NOT INITIAL.
      IF temp_packages_to_search IS NOT INITIAL.
        TYPES: BEGIN OF abap_731_package_type,
                 devclass TYPE tdevc-devclass,
                 parentcl TYPE tdevc-parentcl,
               END OF abap_731_package_type.
        DATA: packages TYPE STANDARD TABLE OF abap_731_package_type WITH DEFAULT KEY.

        TEST-SEAM select_devclass_parentcl_tdev2.

          SELECT devclass  parentcl FROM tdevc INTO TABLE packages
           FOR ALL ENTRIES IN temp_packages_to_search
            WHERE parentcl = temp_packages_to_search-devclass.

        END-TEST-SEAM.

      ENDIF.

      CLEAR temp_packages_to_search.

      DATA package LIKE LINE OF packages.
      LOOP AT packages INTO package.

        CLEAR ls_processed_package.
        ls_processed_package-devclass = package-devclass.
        INSERT ls_processed_package INTO TABLE processed_packages.
        IF sy-subrc EQ 0. "OK
          " New package
          " Search again
          CLEAR ls_temp_package_to_search.
          ls_temp_package_to_search-devclass = package-devclass.
          INSERT ls_temp_package_to_search INTO TABLE temp_packages_to_search.
          sap_package->add( name = package-devclass ).
          IF package-parentcl IS NOT INITIAL.
            " Assume that this is not a top package but that there exists a parent package
            sap_package->set_parent_package( this_package = package-devclass
                                             parent_package = package-parentcl ).
          ENDIF.
        ENDIF.

      ENDLOOP.

      SORT temp_packages_to_search.
      DELETE ADJACENT DUPLICATES FROM temp_packages_to_search.

    ENDWHILE.

  ENDMETHOD.


  METHOD _analyze_components.

    " Loop over all packages to find classes and programms

    " SAP_2_FAMIX_1     Extract classes from Dictionary
    " SAP_2_FAMIX_2     Extract interfaces as FAMIX.Class with attribute isinterface

    DATA class LIKE LINE OF classes.

    FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.

    LOOP AT components_infos ASSIGNING <component_infos>.
      MOVE-CORRESPONDING <component_infos> TO class.
      INSERT class INTO TABLE classes.

      IF <component_infos>-component EQ globclass_component_key
      OR <component_infos>-component EQ globintf_component_key
      OR <component_infos>-component EQ webdynpro_component_comp_key.

        class-obj_name = <component_infos>-component_name.
        INSERT class INTO TABLE classes.

      ELSEIF <component_infos>-component EQ abapprogram_component_key.
        DATA ls_program LIKE LINE OF programs. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
        CLEAR ls_program.
        ls_program-program = <component_infos>-component_name.
        INSERT ls_program INTO TABLE programs.
      ELSEIF <component_infos>-component EQ databasetable_component_key.
        DATA ls_db_table LIKE LINE OF db_tables.
        CLEAR ls_db_table.
        ls_db_table-db_table = <component_infos>-component_name.
        INSERT ls_db_table INTO TABLE db_tables.
      ELSE.
        ASSERT 1 = 2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _read_all_programs.

    " Read all programs

    " SAP_2_FAMIX_4     Extract programs

    FIELD-SYMBOLS <program> LIKE LINE OF programs.
    LOOP AT programs ASSIGNING <program>.

      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos>
            WITH TABLE KEY component = abapprogram_component_key
                           component_name = <program>-program.
      ASSERT sy-subrc EQ 0. "OK

      sap_package->add( name  = <component_infos>-package ).

      DATA module_reference TYPE i.
      module_reference = sap_program->add( name = <program>-program ).

      sap_program->set_parent_package( element_id = module_reference
                                       parent_package = <component_infos>-package ).

      IF p_iprog EQ abap_true.

        DATA program_analyzer TYPE REF TO z2mse_program_analyzer.
        CREATE OBJECT program_analyzer.

        program_analyzer->extract( EXPORTING module_reference = module_reference
                                             program          = <program>-program
                                    CHANGING model            = model ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD _add_classes_to_model.

    " Add to model
    DATA existing_class LIKE LINE OF existing_classes.
    LOOP AT existing_classes INTO existing_class.

      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = globclass_component_key component_name = existing_class-class.
      IF sy-subrc <> 0. "OK
        " It may be an interface
        READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = globintf_component_key component_name = existing_class-class.

        IF sy-subrc <> 0. "OK
          " It may be a Web Dynpro component
          READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = webdynpro_component_comp_key component_name = existing_class-class.
          ASSERT sy-subrc EQ 0. "OK
        ENDIF.
      ENDIF.

      sap_package->add( EXPORTING name = <component_infos>-package ).

      DATA last_id TYPE i.

      IF <component_infos>-component EQ globintf_component_key.
        " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_abapglobalinterface
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
        " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
        sap_class->is_interface( element_id = last_id ).
      ELSEIF <component_infos>-component EQ globclass_component_key.
        " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_abapglobalclass
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
      ELSEIF <component_infos>-component EQ webdynpro_component_comp_key.
        sap_class->add( EXPORTING name      = existing_class-class
                                  modifiers = modifier_webdynpro_component
                        IMPORTING id        = last_id ).
        sap_class->set_parent_package( element_id     = last_id
                                       parent_package = <component_infos>-package ).
      ELSE.
        ASSERT 1 = 2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _add_tables_to_model.

    FIELD-SYMBOLS: <db_table> LIKE LINE OF db_tables.
    LOOP AT db_tables ASSIGNING <db_table>.

      FIELD-SYMBOLS <component_infos> LIKE LINE OF components_infos.
      READ TABLE components_infos ASSIGNING <component_infos> WITH TABLE KEY component = databasetable_component_key component_name = <db_table>-db_table.
      ASSERT sy-subrc EQ 0.
      sap_package->add( EXPORTING name = <component_infos>-package ).

      sap_db_table->add( EXPORTING name = <db_table>-db_table IMPORTING id                 = <db_table>-id_in_model
                                                                        dummy_attribute_id = <db_table>-id_of_dummy_attribute_in_model ).

      sap_db_table->set_parent_package( element_id     = <db_table>-id_in_model
                                        parent_package = <component_infos>-package ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _determine_inheritances_betwee.

    " Determine inheritances between selected classes

    DATA: inheritances TYPE STANDARD TABLE OF  inheritance_type.

    IF existing_classes IS NOT INITIAL.

      TEST-SEAM select_inheritance.

        SELECT clsname refclsname reltype FROM seometarel INTO CORRESPONDING FIELDS OF TABLE inheritances
          FOR ALL ENTRIES IN existing_classes WHERE clsname = existing_classes-class
                                                 AND version = 1.

      END-TEST-SEAM.

    ENDIF.

    " Delete all inheritances where superclass is not in selected packages
    DATA inheritance LIKE LINE OF inheritances.
    LOOP AT inheritances INTO inheritance.
      READ TABLE existing_classes TRANSPORTING NO FIELDS WITH TABLE KEY type = 'C' class = inheritance-refclsname.
      IF sy-subrc <> 0. "OK
        DELETE inheritances.
      ENDIF.
    ENDLOOP.

    " Add inheritances to model
    DATA inheritance_2 LIKE LINE OF inheritances.
    LOOP AT inheritances INTO inheritance_2.
      CASE inheritance_2-reltype.
        WHEN 2.
          " Inheritance
          DATA inheritance_id TYPE i.
          inheritance_id = sap_inheritance->add( ).
          sap_inheritance->set_sub_and_super_class( EXPORTING element_id = inheritance_id
                                                              subclass_name         = inheritance_2-clsname
                                                              superclass_name       = inheritance_2-refclsname ).
        WHEN 1.
          " Interface implementation

          inheritance_id = sap_inheritance->add( ).
          sap_inheritance->set_interface_for_class( EXPORTING element_id = inheritance_id
                                                              interface_name         = inheritance_2-clsname
                                                              class_name       = inheritance_2-refclsname ).

        WHEN 0.
          " Interface composition     (i COMPRISING i_ref)
          " TBD
        WHEN 5.
          " Enhancement            ( c enhances c_ref)
          " TBD
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD _determine_class_components.

    " Determine class components

    " SAP_2_FAMIX_9         Extract methods of classes
    " SAP_2_FAMIX_10        Extract methods of interfaces
    " SAP_2_FAMIX_11        Extract attributes of classes
    " SAP_2_FAMIX_12        Extract attributes of interfaces

    IF existing_classes IS NOT INITIAL.

      TEST-SEAM select_seocompo.

        SELECT clsname cmpname cmptype FROM seocompo INTO TABLE class_components
          FOR ALL ENTRIES IN existing_classes
          WHERE
            clsname = existing_classes-class.

      END-TEST-SEAM.

    ENDIF.

  ENDMETHOD.


  METHOD _add_to_class_components_to_mo.

    " Add to class components to model

    DATA class_component LIKE LINE OF class_components.
    LOOP AT class_components INTO class_component.

      CASE class_component-cmptype.
        WHEN comptype_attribute. "Attribute

          DATA existing_id TYPE i.
          existing_id =  sap_attribute->get_id( class     = class_component-clsname
                                                attribute = class_component-cmpname ).
          IF existing_id EQ 0. " not found

            sap_attribute->add( EXPORTING class     = class_component-clsname
                                          attribute = class_component-cmpname ).
*            famix_attribute->set_parent_type(
*              EXPORTING
*                parent_element = 'FAMIX.Class'
*                parent_name    = CONV string( class_component-clsname ) ).
*            famix_attribute->store_id( EXPORTING class     = CONV string( class_component-clsname )
*                                                 attribute = CONV string( class_component-cmpname ) ).

          ENDIF.

        WHEN comptype_method. "Method

          existing_id = sap_method->get_id( class  = class_component-clsname
                                            method = class_component-cmpname ).

          IF existing_id EQ 0. " not found

            sap_method->add( class  = class_component-clsname
                             method = class_component-cmpname ).

          ENDIF.
        WHEN 2. "Event
        WHEN 3. "Type
        WHEN OTHERS.
          " TBD Warn

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD _determine_usage_of_methods.

    DATA class_component TYPE class_component_type.

    " Determine usage of methods

    LOOP AT class_components INTO class_component WHERE cmptype = comptype_attribute  " Methods
                                                     OR cmptype = comptype_method. "Attributes

      CASE class_component-cmptype.
        WHEN comptype_method.
          DATA used_id TYPE i.
          used_id = sap_method->get_id( class  = class_component-clsname
                                        method = class_component-cmpname ).

        WHEN comptype_attribute.
          used_id = sap_attribute->get_id( class     = class_component-clsname
                                            attribute = class_component-cmpname ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.

      INSERT LINES OF _determine_usages( sap_class        = sap_class
                                         class_component  = class_component
                                         sap_package      = sap_package
                                         sap_method       = sap_method
                                         sap_invocation   = sap_invocation
                                         sap_access       = sap_access
                                         used          = used_id ) INTO TABLE new_components_infos.

    ENDLOOP.

  ENDMETHOD.

  METHOD _determine_usage_of_db_tables.
    DATA db_table LIKE LINE OF db_tables.

    LOOP AT db_tables INTO db_table.

      INSERT LINES OF _determine_usages( sap_class        = sap_class
                                         db_table         = db_table
                                         sap_package      = sap_package
                                         sap_method       = sap_method
                                         sap_invocation   = sap_invocation
                                         sap_access       = sap_access
                                         used          = db_table-id_of_dummy_attribute_in_model ) INTO TABLE new_components_infos.

    ENDLOOP.

  ENDMETHOD.

  METHOD _read_all_classes.

    DATA classname TYPE seoclsname.
    DATA webdynpro_component TYPE wdy_component_name.
    DATA new_line TYPE z2mse_extract_sap=>class_type.

    " Read all classes

    " Determine existing classes
    IF classes IS NOT INITIAL.

      TEST-SEAM select_seoclass.

        SELECT clsname AS class FROM seoclass INTO classname FOR ALL ENTRIES IN classes
          WHERE
            clsname = classes-obj_name.

          CLEAR new_line.
          new_line-type = 'C'.
          new_line-class = classname.
          INSERT new_line INTO TABLE existing_classes.

        ENDSELECT.

      END-TEST-SEAM.

      TEST-SEAM select_wdy_component.

        SELECT component_name AS class FROM wdy_component INTO webdynpro_component FOR ALL ENTRIES IN classes
          WHERE
            component_name = classes-obj_name
            AND version = 'A'.

          CLEAR new_line.
          new_line-type = 'W'.
          new_line-class = webdynpro_component.
          INSERT new_line INTO TABLE existing_classes.

        ENDSELECT.

      END-TEST-SEAM.

    ENDIF.

  ENDMETHOD.

  METHOD _select_requested_components.

    DATA first_package TYPE tdevc.
    DATA processed_packages TYPE z2mse_extract_sap=>processed_packages_type.
    DATA object TYPE trobjtype.

    IF select_by_top_package EQ abap_true.

      " Select components in package and sub package
      " SAP_2_FAMIX_3     Select all components in a package and the sub packages of this package

      TEST-SEAM select_devclass_parentcl_tdev3.

        SELECT SINGLE devclass parentcl FROM tdevc INTO first_package WHERE devclass = package_to_analyze.

      END-TEST-SEAM.

      IF sy-subrc <> 0. "OK
        WRITE: 'Package does not exist: ', package_to_analyze.
        nothing_selected  = abap_true.
      ENDIF.

      processed_packages = _determine_packages_to_analyze( sap_package = sap_package
                                                           package_first = first_package ).

    ENDIF.

    IF   select_by_top_package EQ abap_false
      OR processed_packages IS NOT INITIAL.
      DO 5 TIMES.
        CASE sy-index.
          WHEN 1.
            IF p_clas EQ abap_true.
              object = tadir_clas.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 2.
            IF p_wdyn EQ abap_true.
              object = tadir_wdyn.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 3.
            IF p_intf EQ abap_true.
              object = tadir_intf.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 4.
            IF p_prog EQ abap_true.
              object = tadir_prog.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN 5.
            IF p_tables EQ abap_true.
              " SAP_2_FAMIX_53        Extract database tables
              object = tadir_tabl.
            ELSE.
              CONTINUE.
            ENDIF.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        IF select_by_top_package EQ abap_true.
          DATA: BEGIN OF tadir_component,
                  obj_name TYPE tadir-obj_name,
                  object   TYPE tadir-object,
                  devclass TYPE tadir-devclass,
                END OF tadir_component.

          TEST-SEAM select_tadir.

            SELECT obj_name object devclass FROM tadir INTO tadir_component FOR ALL ENTRIES IN processed_packages
              WHERE pgmid = 'R3TR'
                AND object = object
                AND devclass = processed_packages-devclass.

              DATA ls_component_info LIKE LINE OF components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
              DATA ls_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion

              READ TABLE g_tadir_components_mapping INTO ls_map WITH TABLE KEY object = tadir_component-object.
              ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing

              CLEAR ls_component_info.
              ls_component_info-component = ls_map-component.
              ls_component_info-component_name = tadir_component-obj_name.
              ls_component_info-package = tadir_component-devclass.
              INSERT ls_component_info INTO TABLE components_infos.

            ENDSELECT.

          END-TEST-SEAM.

        ELSE.

          TEST-SEAM select_tadir_2.

            SELECT obj_name object devclass FROM tadir INTO tadir_component
              WHERE pgmid = 'R3TR'
                AND object = object
                AND obj_name IN s_compsn
                AND devclass IN s_pack.

              DATA ls_map_2 LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
              READ TABLE g_tadir_components_mapping INTO ls_map_2 WITH TABLE KEY object = tadir_component-object.
              ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing

              CLEAR ls_component_info.
              ls_component_info-component = ls_map_2-component.
              ls_component_info-component_name = tadir_component-obj_name.
              ls_component_info-package = tadir_component-devclass.
              INSERT ls_component_info INTO TABLE components_infos.

            ENDSELECT.

          END-TEST-SEAM.

        ENDIF.
      ENDDO.
    ENDIF.

    " Select only real tables
    DATA: tabclass TYPE dd02l-tabclass.
    LOOP AT components_infos INTO ls_component_info WHERE component = databasetable_component_key.

      TEST-SEAM select_dd02l.

        SELECT SINGLE tabclass FROM dd02l INTO tabclass WHERE tabname = ls_component_info-component_name
                                                          AND as4local = 'A'
                                                          AND as4vers = ''.

      END-TEST-SEAM.

      IF sy-subrc <> 0.
        "TBD report error
      ELSE.
        IF tabclass EQ 'INTTAB' OR tabclass EQ 'VIEW'.
          DELETE components_infos WHERE component = ls_component_info-component AND component_name = ls_component_info-component_name.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lines( components_infos ) EQ 0.
      WRITE: 'Nothing selected '.
      nothing_selected  = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _handle_used_by_class.

    DATA tadir_object TYPE string.
    CASE i_modifier_of_using_class.
      WHEN modifier_abapglobalclass.
        tadir_object = tadir_clas.
      WHEN modifier_webdynpro_component.
        tadir_object = tadir_wdyn.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

    " Handle used by class
    DATA using_method_id TYPE i.
    using_method_id = i_sap_method->get_id( class  = i_using_class
                                          method = i_using_method ).
    IF using_method_id EQ 0.

      IF g_param_usage_outpack_groupd EQ abap_false.

        " Method does not exist, create the class
        " SAP_2_FAMIX_21      If a component is used by a class that is not selected, add this class to the model
        " SAP_2_FAMIX_22      Do not assign classes that included due to usage to a package


        DATA exists_already_with_id TYPE i.
        i_sap_class->add( EXPORTING name      = i_using_class
                                  modifiers = i_modifier_of_using_class
                        IMPORTING exists_already_with_id = exists_already_with_id ).

        IF exists_already_with_id IS INITIAL.

          " SAP_2_FAMIX_47      If no dummy class is specified in case a using class is not in the initial selection, analyze this classes also

          DATA ls_new_components_info LIKE LINE OF r_new_components_infos. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion

          DATA ls_tadir_comp_map LIKE LINE OF g_tadir_components_mapping. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
          READ TABLE g_tadir_components_mapping INTO ls_tadir_comp_map WITH TABLE KEY object = tadir_object.
          ASSERT sy-subrc EQ 0. " To be compatible with ABAP 7.40, there an exception is raised if table reads finds nothing
          CLEAR ls_new_components_info.
          ls_new_components_info-component_name = i_using_class.
          ls_new_components_info-component   = ls_tadir_comp_map-component .
          INSERT ls_new_components_info INTO TABLE r_new_components_infos.

        ENDIF.

      ELSE.
        " SAP_2_FAMIX_35        Add usage to a single dummy class "OTHER_SAP_CLASS" if required by a parameter
        DATA id TYPE i.
        i_sap_class->add( EXPORTING name                   = 'OTHER_SAP_CLASS'
                                  modifiers              = modifier_abapglobalclass
                        IMPORTING exists_already_with_id = exists_already_with_id
                                  id                     = id ).
        i_sap_package->add( name = 'OTHER_SAP_PACKAGE' ).
        i_sap_class->set_parent_package( element_id     = id
                                       parent_package = 'OTHER_SAP_PACKAGE' ).

      ENDIF.

      " Now there is a class, but no duplicate class

      IF g_param_usage_outpack_groupd EQ abap_false.
        using_method_id = i_sap_method->get_id( class  = i_using_class
                                              method = i_using_method ).
      ELSE.
        using_method_id = i_sap_method->get_id( class  = 'OTHER_SAP_CLASS'
                                              method = 'OTHER_SAP_METHOD' ).
      ENDIF.


      IF using_method_id EQ 0.
        IF g_param_usage_outpack_groupd EQ abap_false.
          " Now also the method is to be created
          " SAP_2_FAMIX_23       If a component is used by a class that is not selected, add the using methods to the model

          using_method_id = i_sap_method->add( class  = i_using_class
                                             method = i_using_method ).

        ELSE.

          " SAP_2_FAMIX_36        Add a usage to a single dummy method "OTHER_SAP_METHOD" if required by a parameter

          using_method_id = i_sap_method->add( class  = 'OTHER_SAP_CLASS'
                                             method = 'OTHER_SAP_METHOD'  ).

        ENDIF.
      ENDIF.

    ENDIF.

    IF i_class_component_is_supplied EQ abap_true.
      CASE i_class_component-cmptype.
        WHEN comptype_method.

          i_sap_invocation->add_invocation( used_method  = i_used
                                          using_method = using_method_id ).

        WHEN comptype_attribute.

          i_sap_access->add_access( used_attribute = i_used
                                  using_method   = using_method_id ).

        WHEN OTHERS.
          ASSERT 1 = 2.
      ENDCASE.
    ELSEIF ib_table_is_supplied EQ abap_true.

      " SAP_2_FAMIX_57      Model usages of database tables by a an access to the dummy attribute of the modelled FAMIX Class
      i_sap_access->add_access( used_attribute  = i_used
                              using_method = using_method_id ).

    ELSE.
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.


  METHOD _convert_compnt_to_famix_model.

    " convert component to FAMIX modifier

    CASE component.
      WHEN globclass_component_key.
        modifier = modifier_abapglobalclass.
      WHEN globintf_component_key.
        modifier = modifier_abapglobalinterface.
      WHEN webdynpro_component_comp_key.
        modifier = modifier_webdynpro_component.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
