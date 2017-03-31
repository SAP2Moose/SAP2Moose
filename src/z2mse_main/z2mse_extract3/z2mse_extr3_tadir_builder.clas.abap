CLASS z2mse_extr3_tadir_builder DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_association_build
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS search_down REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2mse_extr3_tadir_builder IMPLEMENTATION.


  METHOD search_down.

    DATA: element        TYPE REF TO z2mse_extr3_elements,
          package        TYPE REF TO z2mse_extr3_packages,
          tables         TYPE REF TO z2mse_extr3_tables,
          parent_package TYPE REF TO z2mse_extr3_parent_package,
          is_found       TYPE abap_bool.

    parent_package = z2mse_extr3_parent_package=>get_instance( i_element_manager = element_manager ).

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

        CASE tadir-pgmid.
          WHEN 'R3TR'.
            CASE tadir-object.
              WHEN 'CLAS'.
              WHEN 'DEVC'.
              WHEN 'FUGR'.
              WHEN 'INTF'.
              WHEN 'PROG'.
              WHEN 'TABL'.

                tables = z2mse_extr3_tables=>get_instance( i_element_manager = element_manager ).
                DATA: tabname TYPE tabname,
                      new_element_id TYPE z2mse_extr3_element_manager=>element_id_type.
                tabname = tadir-obj_name.
                tables->add( EXPORTING table = tabname
                             IMPORTING is_added = is_found
                                       new_element_id = new_element_id ).
                IF is_found EQ abap_true.

                  parent_package->add( EXPORTING element_id        = new_element_id
                                                 parent_element_id = element_id ).

                ENDIF.
              WHEN 'WDYN'.
              WHEN OTHERS.
                " TBD handle
            ENDCASE.

          WHEN OTHERS.
            "TBD handle
        ENDCASE.


      ENDLOOP.

      "TBD proceed here

    ENDIF.

  ENDMETHOD.
ENDCLASS.
