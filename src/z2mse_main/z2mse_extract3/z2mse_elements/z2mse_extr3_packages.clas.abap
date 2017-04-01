"! I describe an element of type package
CLASS z2mse_extr3_packages DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  CREATE PRIVATE
  GLOBAL FRIENDS z2mse_extr3_packages_mock.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      IMPORTING
                i_element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING VALUE(instance)   TYPE REF TO z2mse_extr3_packages.
    METHODS add
      IMPORTING package               TYPE devclass
      EXPORTING VALUE(is_added)       TYPE abap_bool
                VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS devclass
      IMPORTING
        i_element_id    TYPE i
      RETURNING
        VALUE(r_result) TYPE devclass.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
    METHODS _does_package_exists
      IMPORTING
        i_package     TYPE devclass
      RETURNING
        VALUE(exists) TYPE abap_bool.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id TYPE z2mse_extr3_element_manager=>element_id_type,
             devclass   TYPE devclass,
           END OF element_type.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_packages.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_devclass TYPE HASHED TABLE OF element_type WITH UNIQUE KEY devclass.
ENDCLASS.



CLASS z2mse_extr3_packages IMPLEMENTATION.


  METHOD add.

    READ TABLE elements_devclass TRANSPORTING NO FIELDS WITH KEY devclass = package.
    IF sy-subrc <> 0.

      DATA exists TYPE abap_bool.

      exists = _does_package_exists( package ).
      IF exists EQ abap_true.
        is_added = abap_true.

        new_element_id = element_manager->add_element( element = me ).

        DATA element TYPE element_type.
        element-element_id = new_element_id.
        element-devclass = package.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_devclass.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = i_element_manager.
    ENDIF.
    instance = instance.
    instance->type = package_type.
  ENDMETHOD.


  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    element_manager->famix_package->add( name = element-devclass ).

  ENDMETHOD.

  METHOD devclass.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-devclass.

  ENDMETHOD.


  METHOD _does_package_exists.

    " Does package exists?
    DATA found_obj_name TYPE sobj_name.
    TEST-SEAM tadir.
      SELECT SINGLE obj_name FROM tadir INTO found_obj_name WHERE pgmid = 'R3TR'
                                                              AND object = 'DEVC'
                                                              AND obj_name = i_package.
    END-TEST-SEAM.

    IF sy-subrc EQ 0.
      exists = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
