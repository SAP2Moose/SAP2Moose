CLASS z2mse_extr3_tadir_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_element_manager TYPE REF TO z2mse_extr3_element_manager.
    METHODS search_down REDEFINITION.
    METHODS search_up REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: tables         TYPE REF TO z2mse_extr3_tables,
          classes        TYPE REF TO z2mse_extr3_classes,
          programs       TYPE REF TO z2mse_extr3_programs,
          web_dynpro_components TYPE REF TO z2mse_extr3_web_dynpro_comp,
          parent_package TYPE REF TO z2mse_extr3_parent_package.
ENDCLASS.



CLASS Z2MSE_EXTR3_TADIR_BUILDER IMPLEMENTATION.


  METHOD constructor.

    super->constructor( i_element_manager = i_element_manager ).

    parent_package = z2mse_extr3_parent_package=>get_instance( i_element_manager = element_manager ).
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
    programs = z2mse_extr3_programs=>get_instance( i_element_manager = element_manager ).
    tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
    web_dynpro_components = z2mse_extr3_web_dynpro_comp=>get_instance( element_manager = element_manager ).

  ENDMETHOD.


  METHOD search_down.

    DATA: element        TYPE REF TO z2mse_extr3_elements,
          package        TYPE REF TO z2mse_extr3_packages,
          is_found       TYPE abap_bool,
          new_element_id TYPE z2mse_extr3_element_manager=>element_id_type,
          class_name     TYPE seoclsname,
          tabname        TYPE tabname,
          program        TYPE PROGNAME,
          WDY_COMPONENT_NAME TYPE WDY_COMPONENT_NAME.

    element = element_manager->get_element( element_id ).

    IF element->type EQ element->package_type.

      package ?= element.

      DATA devclass TYPE devclass.

      devclass = package->devclass( element_id ).

      TYPES: BEGIN OF ty_tadir,
               pgmid    TYPE pgmid,
               object   TYPE trobjtype,
               obj_name TYPE sobj_name,
             END OF ty_tadir.

      DATA: tadir  TYPE ty_tadir,
            tadirs TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

      SELECT pgmid object obj_name FROM tadir INTO TABLE tadirs WHERE devclass = devclass.

      LOOP AT tadirs INTO tadir.

        is_found = abap_false.

        CASE tadir-pgmid.
          WHEN 'R3TR'.
            CASE tadir-object.
              WHEN 'CLAS' OR 'INTF'.

                class_name = tadir-obj_name.
                classes->add( EXPORTING class          = class_name
                              IMPORTING is_added       = is_found
                                        new_element_id = new_element_id ).

              WHEN 'DEVC'.
              WHEN 'FUGR'.
              WHEN 'PROG'.

                program = tadir-obj_name.
                programs->add( EXPORTING program        = program
                               IMPORTING is_added       = is_found
                                         new_element_id = new_element_id ).

              WHEN 'TABL'.

                tabname = tadir-obj_name.
                tables->add( EXPORTING table          = tabname
                             IMPORTING is_added       = is_found
                                       new_element_id = new_element_id ).

              WHEN 'WDYN'.

              WDY_COMPONENT_NAME = tadir-obj_name.
              web_dynpro_components->add( EXPORTING wdy_component_name = wdy_component_name
                                          IMPORTING is_added       = is_found
                                                    new_element_id = new_element_id ).

              WHEN OTHERS.
                " TBD handle
            ENDCASE.

          WHEN OTHERS.
            "TBD handle
        ENDCASE.

        IF is_found EQ abap_true.

          parent_package->add( EXPORTING element_id        = new_element_id
                                         parent_element_id = element_id ).

        ENDIF.


      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD search_up.

    DATA: element        TYPE REF TO z2mse_extr3_elements,
          package        TYPE REF TO z2mse_extr3_packages,
          is_found       TYPE abap_bool,
          new_element_id TYPE z2mse_extr3_element_manager=>element_id_type,
          class_name     TYPE seoclsname,
          clstype        TYPE seoclstype,
          tabname        TYPE tabname.

    package = z2mse_extr3_packages=>get_instance( i_element_manager = element_manager ).

    element = element_manager->get_element( element_id ).

    DATA: object   TYPE trobjtype,
          obj_name TYPE sobj_name.

    CLEAR object.
    CLEAR obj_name.

    CASE element->type.
      WHEN element->class_type.
        classes->class_name( EXPORTING element_id = element_id
                             IMPORTING class_name = class_name
                                       clstype    = clstype ).
        obj_name = class_name.
        IF clstype EQ classes->is_class_type.
          object = 'CLAS'.
        ELSEIF clstype EQ classes->interface_type.
          object = 'INTF'.
        ELSE.
          ASSERT 1 = 2.
        ENDIF.
      WHEN element->table_type.
        tabname = tables->table_name( i_element_id = element_id ).
        object = 'TABL'.
        obj_name = tabname.
    ENDCASE.

    TYPES: BEGIN OF ty_tadir,
             devclass TYPE devclass,
           END OF ty_tadir.

    DATA: tadir  TYPE ty_tadir,
          tadirs TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.
    IF object IS NOT INITIAL.
      SELECT devclass FROM tadir INTO TABLE tadirs WHERE pgmid = 'R3TR'
                                                     AND object = object
                                                     AND obj_name = obj_name.
      IF sy-subrc EQ 0.
        LOOP AT tadirs INTO tadir.

          package->add( EXPORTING package        = tadir-devclass
                        IMPORTING is_added       = is_found
                                  new_element_id = new_element_id ).

          IF is_found EQ abap_true.

            parent_package->add( EXPORTING element_id        = element_id
                                           parent_element_id = new_element_id ).

          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
