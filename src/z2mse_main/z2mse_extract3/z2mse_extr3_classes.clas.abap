CLASS z2mse_extr3_classes DEFINITION
  PUBLIC
  INHERITING FROM z2mse_extr3_elements
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      IMPORTING
        element_manager TYPE REF TO z2mse_extr3_element_manager
      RETURNING
        VALUE(instance) TYPE REF TO z2mse_extr3_classes.
    METHODS add
      IMPORTING
        class                 TYPE string
      EXPORTING
        VALUE(is_added)       TYPE abap_bool
        VALUE(new_element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS class_name
      IMPORTING
        element_id        TYPE i
      RETURNING
        VALUE(class_name) TYPE seoclsname.
    METHODS make_model REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO z2mse_extr3_classes.
    TYPES: BEGIN OF element_type,
             element_id   TYPE z2mse_extr3_element_manager=>element_id_type,
             class_name   TYPE seoclsname,
             is_interface TYPE abap_bool,
           END OF element_type.
    DATA elements_element_id TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements_class_name TYPE HASHED TABLE OF element_type WITH UNIQUE KEY class_name.
ENDCLASS.



CLASS z2mse_extr3_classes IMPLEMENTATION.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance
        EXPORTING
          i_element_manager = element_manager.
    ENDIF.
    instance = instance.
    instance->type = class_type.
  ENDMETHOD.


  METHOD add.

    READ TABLE elements_class_name TRANSPORTING NO FIELDS WITH KEY class_name = class.
    IF sy-subrc <> 0.

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

        new_element_id = element_manager->add_element( element = me ).

        DATA element TYPE element_type.
        element-element_id = new_element_id.
        element-class_name = class.
        IF found_class_type EQ 0.
          element-is_interface = abap_false.
        ELSEIF found_class_type EQ 1.
          element-is_interface = abap_true.
        ELSE.
          ASSERT 1 = 2. " Serious inconsistency
        ENDIF.
        INSERT element INTO TABLE elements_element_id.
        INSERT element INTO TABLE elements_class_name.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD class_name.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    class_name = element-class_name.

  ENDMETHOD.

  METHOD make_model.

    DATA element TYPE element_type.

    READ TABLE elements_element_id INTO element WITH TABLE KEY element_id = element_id.
    ASSERT sy-subrc EQ 0.

    DATA last_id TYPE i.

    IF element-is_interface EQ abap_false.
      " SAP_2_FAMIX_59      Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalClass'
      " SAP_2_FAMIX_6     Map ABAP classes to FAMIX.Class
      element_manager->famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                                   name       = element-class_name
                                                   modifiers  = z2mse_extract3=>modifier_abapglobalclass
                                         IMPORTING id         = last_id ).
    ELSE.
      " SAP_2_FAMIX_60        Mark the FAMIX Class with the attribute modifiers = 'ABAPGlobalInterface'
      " SAP_2_FAMIX_7     Map ABAP Interfaces to FAMIX.Class
      element_manager->famix_class->add( EXPORTING name_group = 'ABAP_CLASS'
                                                   name       = element-class_name
                                                   modifiers  = z2mse_extract3=>modifier_abapglobalinterface
                                         IMPORTING id         = last_id ).
      " SAP_2_FAMIX_8       Set the attribute isInterface in case of ABAP Interfaces
      element_manager->famix_class->is_interface( element_id = last_id ).
    ENDIF.
    DATA association TYPE z2mse_extr3_element_manager=>association_type.
    LOOP AT associations INTO association WHERE element_id1 = element_id
                                            AND association->type = z2mse_extr3_association=>parent_package_ass.
      DATA package TYPE REF TO z2mse_extr3_packages.
      package ?= element_manager->get_element( i_element_id = association-element_id2 ).
      element_manager->famix_class->set_parent_package( element_id     = last_id
                                                        parent_package = package->devclass( i_element_id = association-element_id2 ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
