CLASS z2mse_extr_web_dynpro DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_tadir_test,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_tadir_test .
    TYPES:
      ty_t_tadir_test TYPE HASHED TABLE OF ty_tadir_test WITH UNIQUE KEY object obj_name.

    METHODS constructor
      IMPORTING
        !tadir_test TYPE ty_t_tadir_test OPTIONAL.


    TYPES: BEGIN OF ty_web_dynpro_component,
             component_name  TYPE wdy_component_name,
             controller_name TYPE wdy_controller_name,
           END OF ty_web_dynpro_component.
    TYPES ty_web_dynpro_components TYPE SORTED TABLE OF ty_web_dynpro_component WITH UNIQUE KEY component_name controller_name.
    TYPES ty_web_dynpro_components_hash TYPE HASHED TABLE OF ty_web_dynpro_component WITH UNIQUE KEY component_name controller_name.

    "! Add Web Dynpro Components by a list of Web Dynpro Components
    METHODS select_classes_by_components
      IMPORTING
        components TYPE ty_web_dynpro_components_hash.

    "! Add all selected components to the model. Should be called only once
    METHODS add_to_model
      IMPORTING
        famix_class  TYPE REF TO z2mse_famix_class
        famix_method TYPE REF TO z2mse_famix_method.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA g_is_test TYPE abap_bool.
    "! Filled during tests
    DATA g_tadir_test TYPE ty_t_tadir_test.
    "! A list of all components of primarily selected and existing classes or interfaces. Only if not yet transfered to where used analysis
    DATA g_selctd_web_dynpro_compts TYPE ty_web_dynpro_components_hash.
ENDCLASS.



CLASS z2mse_extr_web_dynpro IMPLEMENTATION.


  METHOD constructor.
    IF tadir_test IS SUPPLIED.
      g_tadir_test = tadir_test.
      g_is_test = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD select_classes_by_components.
    DATA component TYPE ty_web_dynpro_component.
    LOOP AT components INTO component.
      INSERT component INTO TABLE g_selctd_web_dynpro_compts.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_to_model.

    DATA wdy_comp TYPE ty_web_dynpro_component.
    DATA class_id TYPE i.
    DATA method_id TYPE i.


    LOOP AT g_selctd_web_dynpro_compts INTO wdy_comp.
      famix_class->add( EXPORTING name_group             = 'WEB_DYNPRO'
                                name                   = wdy_comp-component_name
                                modifiers              = 'ABAPWebDynproComponent'
                        IMPORTING id         = class_id ).
      famix_method->add( EXPORTING name = wdy_comp-controller_name IMPORTING id = method_id ).

      famix_method->set_signature( element_id = method_id
                                     signature = wdy_comp-controller_name ).
      famix_method->set_parent_type(
        EXPORTING
          element_id         = method_id
          parent_element     = 'FAMIX.Class'
          parent_id          = class_id ).

      "! TBD Really required, this appears to be not exact, no namegroup, ...
      famix_method->store_id( EXPORTING class  = wdy_comp-component_name
                                        method = wdy_comp-controller_name ).


    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
