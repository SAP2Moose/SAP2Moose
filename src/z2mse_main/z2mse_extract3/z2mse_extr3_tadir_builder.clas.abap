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
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: tables         TYPE REF TO z2mse_extr3_tables,
          classes        TYPE REF TO z2mse_extr3_classes,
          parent_package TYPE REF TO z2mse_extr3_parent_package.
ENDCLASS.



CLASS z2mse_extr3_tadir_builder IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_element_manager = i_element_manager ).

    parent_package = z2mse_extr3_parent_package=>get_instance( i_element_manager = element_manager ).
    classes = z2mse_extr3_classes=>get_instance( element_manager = element_manager ).
    tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).

  ENDMETHOD.

  METHOD search_down.

    DATA: element        TYPE REF TO z2mse_extr3_elements,
          package        TYPE REF TO z2mse_extr3_packages,
          is_found       TYPE abap_bool,
          new_element_id TYPE z2mse_extr3_element_manager=>element_id_type,
          class_name     TYPE string,
          tabname        TYPE tabname.

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
              WHEN 'TABL'.

                tabname = tadir-obj_name.
                tables->add( EXPORTING table          = tabname
                             IMPORTING is_added       = is_found
                                       new_element_id = new_element_id ).

              WHEN 'WDYN'.
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

      "TBD proceed here

    ENDIF.

  ENDMETHOD.

ENDCLASS.
