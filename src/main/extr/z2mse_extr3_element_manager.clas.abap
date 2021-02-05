"! I know all elements and associations between elements that are currently known.
"! I provide general methods to add new elements and associations between elements.
CLASS z2mse_extr3_element_manager DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    DATA model            TYPE REF TO z2mse_model.
    DATA famix_package     TYPE REF TO z2mse_famix_package.
    DATA famix_class     TYPE REF TO z2mse_famix_class.
    DATA famix_method     TYPE REF TO z2mse_famix_method.
    DATA famix_attribute     TYPE REF TO z2mse_famix_attribute.
    DATA famix_invocation     TYPE REF TO z2mse_famix_invocation.
    DATA famix_access     TYPE REF TO z2mse_famix_access.
    DATA famix_file_anchor TYPE REF TO z2mse_famix_file_anchor.
    DATA exclude_found_sap_intf TYPE abap_bool READ-ONLY.
    DATA interface_use_structure TYPE abap_bool READ-ONLY.
    "! A unique identifier for each object extracted
    TYPES element_id_type TYPE i.

    TYPES: BEGIN OF association_type,
             element_id1 TYPE element_id_type,
             element_id2 TYPE element_id_type,
             ass_type    TYPE c LENGTH 30, "To prevent problem with local classes, better would be: z2mse_extr3_association=>ass_type,
             association TYPE REF TO z2mse_extr3_association,
           END OF association_type.
    TYPES associations_type TYPE STANDARD TABLE OF association_type WITH KEY element_id1 element_id2 ass_type association.
    METHODS constructor
      IMPORTING i_model_builder          TYPE REF TO z2mse_extr3_model_builder
                i_exclude_found_sap_intf TYPE abap_bool
                i_interface_use_structure     TYPE abap_bool.
    "! Call if an element might be added.
    "! Add the element if it is not already part of the model.
    METHODS add_element
      IMPORTING
                element           TYPE REF TO z2mse_extr3_elements
                is_specific       TYPE abap_bool
      RETURNING VALUE(element_id) TYPE z2mse_extr3_element_manager=>element_id_type.
    METHODS add_association
      IMPORTING
        element_1   TYPE element_id_type
        element_2   TYPE element_id_type
        association TYPE REF TO z2mse_extr3_association.
    "! Call so that the classes that contain the collected elements determine further informations that are required for the model.
    METHODS collect_infos
      IMPORTING
        sysid TYPE string OPTIONAL.
    "! Call to build the mse model
    METHODS make_model
      RETURNING
        VALUE(r_result) TYPE z2mse_model=>lines_type.
    METHODS get_element
      IMPORTING
        i_element_id    TYPE element_id_type
      RETURNING
        VALUE(r_result) TYPE REF TO z2mse_extr3_elements.
    METHODS get_associations
      IMPORTING
                i_element_id        TYPE element_id_type
      RETURNING VALUE(associations) TYPE associations_type.
    DATA model_builder TYPE REF TO z2mse_extr3_model_builder.


  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF element_type,
             element_id TYPE element_id_type,
             "! A reference to the instance that handles this object
             element    TYPE REF TO z2mse_extr3_elements,
           END OF element_type.
    TYPES elements_type TYPE HASHED TABLE OF element_type WITH UNIQUE KEY element_id.
    DATA elements TYPE elements_type.
    TYPES associations1_type TYPE SORTED TABLE OF association_type WITH UNIQUE KEY element_id1 element_id2 ass_type.
    TYPES associations2_type TYPE SORTED TABLE OF association_type WITH UNIQUE KEY element_id2 element_id1 ass_type.
    DATA associations1 TYPE associations1_type.
    DATA associations2 TYPE associations2_type.
    DATA next_element_id TYPE i.
ENDCLASS.



CLASS z2mse_extr3_element_manager IMPLEMENTATION.


  METHOD add_association.

    DATA line TYPE association_type.
    line-element_id1 = element_1.
    line-element_id2 = element_2.
    line-ass_type    = association->type.
    line-association = association.
    INSERT line INTO TABLE associations1.
    INSERT line INTO TABLE associations2.

  ENDMETHOD.


  METHOD add_element.

    DATA element_line TYPE element_type.
    element_line-element_id = next_element_id.
    element_id = next_element_id.
    element_line-element =  element.
    INSERT element_line INTO TABLE elements.

    model_builder->new_element_id( i_element_id  = element_id
                                   i_is_specific = is_specific ).

    ADD 1 TO next_element_id.

  ENDMETHOD.


  METHOD collect_infos.

    DATA: element      TYPE element_type.

    LOOP AT elements INTO element.

      IF element-element->infos_are_collected EQ abap_false.

        element-element->collect_infos( sysid ).

        element-element->infos_are_collected = abap_true.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    model_builder = i_model_builder.

    exclude_found_sap_intf = i_exclude_found_sap_intf.

    interface_use_structure = i_interface_use_structure.

    next_element_id = 1.

    CREATE OBJECT model.

    DATA f_custom_source_language TYPE REF TO z2mse_famix_custom_source_lng.
    CREATE OBJECT f_custom_source_language EXPORTING model = model.
    f_custom_source_language->add( name = 'SAP' name_group = z2mse_extr3=>ng_source_language ).

    CREATE OBJECT famix_package EXPORTING model = model.
    CREATE OBJECT famix_class EXPORTING model = model.
    CREATE OBJECT famix_method EXPORTING model = model.
    CREATE OBJECT famix_attribute EXPORTING model = model.
    CREATE OBJECT famix_invocation EXPORTING model = model.
    CREATE OBJECT famix_access EXPORTING model = model.
    CREATE OBJECT famix_file_anchor EXPORTING model = model.

  ENDMETHOD.


  METHOD get_associations.
    DATA association TYPE association_type.

    LOOP AT associations1 INTO association WHERE element_id1 = i_element_id.
      INSERT association INTO TABLE associations.
    ENDLOOP.

    LOOP AT associations2 INTO association WHERE element_id2 = i_element_id.
      INSERT association INTO TABLE associations.
    ENDLOOP.

    SORT associations.
    DELETE ADJACENT DUPLICATES FROM associations.

  ENDMETHOD.


  METHOD get_element.

    DATA element TYPE element_type.

    READ TABLE elements INTO element WITH TABLE KEY element_id = i_element_id.
    ASSERT sy-subrc EQ 0.

    r_result = element-element.

  ENDMETHOD.


  METHOD make_model.

    DATA: element      TYPE element_type,
          associations TYPE associations_type,
          association  TYPE association_type,
          step         TYPE i.

    DO 2 TIMES.

      step = sy-index.

      LOOP AT elements INTO element.

        " Add packages first to the Moose model. This simplifies building the model, as many elements have parent
        IF step EQ 1.
          IF element-element->type <> element-element->package_type.
            CONTINUE.
          ENDIF.
        ELSEIF element-element->type EQ element-element->package_type.
          CONTINUE.
        ENDIF.

        associations = get_associations( i_element_id = element-element_id ).

        element-element->make_model( element_id = element-element_id
                                     associations = associations ).

      ENDLOOP.

    ENDDO.

    DATA: access               TYPE REF TO z2mse_extr3_access,
          invocation           TYPE REF TO z2mse_extr3_invocation,
          association_instance TYPE REF TO z2mse_extr3_association.

    access = z2mse_extr3_access=>get_instance( i_element_manager = me ).
    invocation = z2mse_extr3_invocation=>get_instance( i_element_manager = me ).

    LOOP AT associations1 INTO association.

      CLEAR association_instance.

      IF association-association->type EQ association-association->access_ass.
        association_instance = access.
      ELSEIF association-association->type EQ association-association->invocation_ass.
        association_instance = invocation.
      ENDIF.

      IF association_instance IS BOUND.
        association_instance->make_model( association = association ).
      ENDIF.

    ENDLOOP.


    model->make_mse( IMPORTING mse_model = r_result ).

  ENDMETHOD.
ENDCLASS.
