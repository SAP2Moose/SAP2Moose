******************************************** Begin Include Z_FAMIX_ABAP ******************************************************************************************
*The MIT License (MIT)
*
*Copyright (c) 2016 Rainer Winkler, CubeServ
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.

CLASS cl_famix_entity DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
  PROTECTED SECTION.
    DATA elementname TYPE string.
    DATA model TYPE REF TO cl_model.
    DATA mv_last_used_id TYPE i.
ENDCLASS.

CLASS cl_famix_entity IMPLEMENTATION.

  METHOD constructor.
    model = i_model.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! Declare source language
    "! @parameter i_source_language_element | the FAMIX element of the source language
    "! @parameter i_source_language_name | the name of the source language
    METHODS set_declared_source_language
      IMPORTING
        i_source_language_element TYPE string
        i_source_language_name    TYPE string.
ENDCLASS.

CLASS cl_famix_sourced_entity IMPLEMENTATION.

  METHOD set_declared_source_language.
    model->add_reference( EXPORTING i_attribute_name = 'declaredSourceLanguage'
                                    i_elementname       = i_source_language_element
                                    i_name_of_reference = i_source_language_name ).
  ENDMETHOD.

ENDCLASS.



CLASS cl_famix_named_entity DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.

    "! Call once to create a new named entity
    "! @parameter e_exists_already_with_id | contains the id if entry already existed
    "! @parameter r_id | the id in model either if just created or already existing
    METHODS add
      IMPORTING i_name_group                    TYPE string OPTIONAL
                i_name                          TYPE string
      EXPORTING VALUE(e_exists_already_with_id) TYPE i
      RETURNING VALUE(r_id)                     TYPE i.
    "! Call once to set the parent package
    "! @parameter i_parent_package | the name of an element of type FAMIX.Package
    METHODS set_parent_package IMPORTING i_parent_package TYPE string.

  PROTECTED SECTION.





ENDCLASS.

CLASS cl_famix_named_entity IMPLEMENTATION.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname = elementname
                                        i_is_named_entity = true
                                        i_can_be_referenced_by_name = true
                                        i_name_group = i_name_group
                                        i_name = i_name
                              IMPORTING e_exists_already_with_id = e_exists_already_with_id ).
    mv_last_used_id = r_id.
  ENDMETHOD.

  METHOD set_parent_package.
    model->add_reference( i_elementname = 'FAMIX.Package' i_name_of_reference = i_parent_package i_attribute_name = 'parentPackage' ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_parameter DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! Set the parent behavioural entity, either a method or a function
    "! @parameter i_parent_id | id of parent entity
    METHODS set_parent_behavioural_entity
      IMPORTING
        i_parent_id TYPE i.
ENDCLASS.

CLASS cl_famix_parameter IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Parameter'.
  ENDMETHOD.

  METHOD set_parent_behavioural_entity.
    model->add_reference_by_id( EXPORTING i_attribute_name = 'parentBehaviouralEntity'
                                          i_reference_id   = i_parent_id ).
  ENDMETHOD.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = false i_name = i_name
                              IMPORTING e_exists_already_with_id = e_exists_already_with_id ).
    mv_last_used_id = r_id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_attribute DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter i_class | the class of the method
    "! @parameter i_attribute | the attribute name
    METHODS store_id
      IMPORTING
        i_class     TYPE string
        i_attribute TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter i_class | the class of the method
    "! @parameter i_attribute | the attribute name
    "! @parameter r_id | the ID of the element
    METHODS get_id
      IMPORTING
                i_class     TYPE string
                i_attribute TYPE string
      RETURNING VALUE(r_id) TYPE i.
    METHODS add REDEFINITION.

    "! set the parent type, for instance the class the method is contained in
    "! @parameter i_parent_element | the FAMIX element of the parent Type
    "! @parameter i_parent_name | the name of the parent element
    METHODS set_parent_type
      IMPORTING
        i_parent_element TYPE string
        i_parent_name    TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_attribute_id,
             class     TYPE string,
             attribute TYPE string,
             id        TYPE i,
           END OF ty_attribute_id.
    DATA: mt_attribute_id TYPE HASHED TABLE OF ty_attribute_id WITH UNIQUE KEY class attribute.
ENDCLASS.

CLASS cl_famix_attribute IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Attribute'.
  ENDMETHOD.
  METHOD set_parent_type.
    model->add_reference( EXPORTING i_attribute_name    = 'parentType'
                                    i_elementname       = i_parent_element
                                    i_name_of_reference = i_parent_name ).
  ENDMETHOD.
  METHOD add.
    r_id = model->add_entity( i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = false i_name = i_name ).
    mv_last_used_id = r_id.
  ENDMETHOD.


  METHOD store_id.
    mt_attribute_id = VALUE #( BASE mt_attribute_id ( id = mv_last_used_id class = i_class attribute = i_attribute ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE mt_attribute_id ASSIGNING FIELD-SYMBOL(<ls_attribute_id>) WITH TABLE KEY class = i_class attribute = i_attribute.
    IF sy-subrc EQ ok.
      r_id = <ls_attribute_id>-id.
    ELSE.
      r_id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_container_entity DEFINITION INHERITING FROM cl_famix_named_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the container an element is in
    "! @parameter i_container_element | the FAMIX element of the Container
    "! @parameter i_parent_container | the name of the Container
    METHODS set_container IMPORTING i_container_element TYPE string i_parent_container TYPE string.
    "! Set the container an element is in using the reference
    "! @parameter i_container_element | the FAMIX element of the Container
    "! @parameter i_parent_container_id | the id of the Container
    METHODS set_container_by_id IMPORTING i_container_element TYPE string i_parent_container_id TYPE i.
  PROTECTED SECTION.


ENDCLASS.

CLASS cl_famix_container_entity IMPLEMENTATION.

  METHOD set_container.
    model->add_reference( EXPORTING i_elementname       = i_container_element
                                    i_name_of_reference = i_parent_container
                                    i_attribute_name    = 'container' ).
  ENDMETHOD.

  METHOD set_container_by_id.
    model->add_reference_by_id( EXPORTING i_attribute_name = 'container'
                                          i_reference_id   = i_parent_container_id ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_behavioural_entity DEFINITION INHERITING FROM cl_famix_container_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the signature of a method
    "! This might not be relevant for ABAP, but is contained here for completeness
    "! @parameter i_signature | The signature like myMethod( myParameters, ...)
    METHODS set_signature IMPORTING i_signature TYPE string.

ENDCLASS.

CLASS cl_famix_behavioural_entity IMPLEMENTATION.

  METHOD set_signature.
    model->add_string( EXPORTING i_attribute_name = 'signature'
                                 i_string         = i_signature ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_namespace DEFINITION INHERITING FROM cl_famix_container_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.

ENDCLASS.

CLASS cl_famix_namespace IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Namespace'.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_package DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_package IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Package'.
  ENDMETHOD.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = true i_name = i_name
                              IMPORTING e_exists_already_with_id = e_exists_already_with_id ).
    mv_last_used_id = r_id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_module DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_module IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Module'.
  ENDMETHOD.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = true i_name = i_name
                              IMPORTING e_exists_already_with_id = e_exists_already_with_id ).
    mv_last_used_id = r_id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_method DEFINITION INHERITING FROM cl_famix_behavioural_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! set the parent type, for instance the class the method is contained in
    "! Provide either i_parent_name or i_parent_id
    "! @parameter i_parent_element | the FAMIX element of the parent Type
    "! @parameter i_parent_name | optional the name of the parent element
    "! @parameter i_parent_id | optional the id of the parent element
    METHODS set_parent_type
      IMPORTING
        i_parent_element TYPE string
        i_parent_name    TYPE string OPTIONAL
        i_parent_id      TYPE i OPTIONAL.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter i_class | the class of the method
    "! @parameter i_method | the method name
    METHODS store_id
      IMPORTING
        i_class  TYPE string
        i_method TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter i_class | the class of the method
    "! @parameter i_method | the method name
    "! @parameter r_id | the ID of the element
    METHODS get_id
      IMPORTING
                i_class     TYPE string
                i_method    TYPE string
      RETURNING VALUE(r_id) TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_method_id,
             class  TYPE string,
             method TYPE string,
             id     TYPE i,
           END OF ty_method_id.
    DATA: mt_method_id TYPE HASHED TABLE OF ty_method_id WITH UNIQUE KEY class method.
ENDCLASS.

CLASS cl_famix_method IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Method'.
  ENDMETHOD.

  METHOD set_parent_type.
    IF i_parent_name IS SUPPLIED.
      model->add_reference( EXPORTING i_attribute_name    = 'parentType'
                                      i_elementname       = i_parent_element
                                      i_name_of_reference = i_parent_name ).
    ELSEIF i_parent_id IS SUPPLIED.
      model->add_reference_by_id( EXPORTING i_attribute_name = 'parentType'
                                            i_reference_id   = i_parent_id ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    r_id = model->add_entity( i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = false i_name = i_name ).
    mv_last_used_id = r_id.
  ENDMETHOD.

  METHOD store_id.
    mt_method_id = VALUE #( BASE mt_method_id ( id = mv_last_used_id class = i_class method = i_method ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE mt_method_id ASSIGNING FIELD-SYMBOL(<ls_method_id>) WITH TABLE KEY class = i_class method = i_method.
    IF sy-subrc EQ ok.
      r_id = <ls_method_id>-id.
    ELSE.
      r_id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_class DEFINITION INHERITING FROM cl_famix_container_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    "! Set if it is an interface
    METHODS is_interface.
ENDCLASS.

CLASS cl_famix_class IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Class'.
  ENDMETHOD.

  METHOD is_interface.
    model->add_boolean(
      EXPORTING
        i_attribute_name = 'isInterface'
        i_is_true        = true
    ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_association DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(r_id) TYPE i.
ENDCLASS.

CLASS cl_famix_association IMPLEMENTATION.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname     = elementname i_is_named_entity = false i_can_be_referenced_by_name = false ).
    mv_last_used_id = r_id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_access DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    "! Checks that accessor and variable of an access are a new access
    "! @parameter i_accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter i_variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS is_new_access
      IMPORTING
                i_accessor_id   TYPE i
                i_variable_id   TYPE i
      RETURNING VALUE(r_is_new) TYPE bool.
    "! defines accessor and variable of an access
    "! @parameter i_accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter i_variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS set_accessor_variable_relation
      IMPORTING
        i_accessor_id TYPE i
        i_variable_id TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_accessor_variable_id,
             accessor_id TYPE i,
             variable_id TYPE i,
           END OF  ty_accessor_variable_id.
    DATA: mt_accessor_variable_id TYPE HASHED TABLE OF ty_accessor_variable_id WITH UNIQUE KEY accessor_id variable_id.
ENDCLASS.

CLASS cl_famix_access IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Access'.
  ENDMETHOD.

  METHOD set_accessor_variable_relation.
    mt_accessor_variable_id = VALUE #( BASE mt_accessor_variable_id ( accessor_id = i_accessor_id variable_id = i_variable_id ) ).
    model->add_reference_by_id( EXPORTING i_attribute_name = 'accessor'
                                          i_reference_id   = i_accessor_id ).
    model->add_reference_by_id( EXPORTING i_attribute_name = 'variable'
                                          i_reference_id   = i_variable_id ).
  ENDMETHOD.

  METHOD is_new_access.
    READ TABLE mt_accessor_variable_id TRANSPORTING NO FIELDS WITH TABLE KEY accessor_id = i_accessor_id variable_id = i_variable_id.
    IF sy-subrc <> ok.
      r_is_new = true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_invocation DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.

    METHODS is_new_invocation_to_candidate
      IMPORTING
                i_sender_id     TYPE i
                i_candidates_id TYPE i
      RETURNING VALUE(r_is_new) TYPE bool.

    "! defines an invocation
    "! this also models standard call by functions or methods to objects other than attributes
    "! Us this method to reference the receiver using his id
    "! @parameter i_sender_id | the id of the sender or calling method or function
    "! @parameter i_candidates_id | the id of the candidate, this is the used method or function of type BehaviouralEntity in case of method or function usage
    "! @parameter i_receiver_id | optional the id of the receiver or called method or function
    "! @parameter i_signature | optional a signature
    "! @parameter i_receiver_source_code | optional a receiver source code
    METHODS set_invocation_by_reference
      IMPORTING
        i_sender_id            TYPE i
        i_candidates_id        TYPE i OPTIONAL
        i_receiver_id          TYPE i OPTIONAL
        i_signature            TYPE string OPTIONAL
        i_receiver_source_code TYPE string OPTIONAL.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sender_candidate,
             sender_id     TYPE i,
             candidates_id TYPE i,
           END OF ty_sender_candidate.

    DATA mt_sender_candidates TYPE HASHED TABLE OF ty_sender_candidate WITH UNIQUE KEY sender_id candidates_id.

ENDCLASS.

CLASS cl_famix_invocation IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Invocation'.
  ENDMETHOD.

  METHOD is_new_invocation_to_candidate.
    READ TABLE mt_sender_candidates TRANSPORTING NO FIELDS WITH TABLE KEY sender_id = i_sender_id candidates_id = i_candidates_id.
    IF sy-subrc <> ok.
      r_is_new = true.
    ENDIF.
  ENDMETHOD.

  METHOD set_invocation_by_reference.
    model->add_reference_by_id( EXPORTING i_attribute_name = 'sender'
                                          i_reference_id   = i_sender_id ).
    IF i_candidates_id IS SUPPLIED.
      mt_sender_candidates = VALUE #( BASE mt_sender_candidates ( sender_id = i_sender_id candidates_id = i_candidates_id ) ).
      model->add_reference_by_id( EXPORTING i_attribute_name = 'candidates'
                       i_reference_id   = i_candidates_id ).
    ENDIF.

    IF i_receiver_id IS SUPPLIED.
      model->add_reference_by_id( EXPORTING i_attribute_name = 'receiver'
                                            i_reference_id   = i_receiver_id ).
    ENDIF.
    IF i_signature IS SUPPLIED.
      model->add_string( EXPORTING i_attribute_name = 'signature'
                                   i_string         = i_signature ).
    ENDIF.
    IF i_receiver_source_code IS SUPPLIED.
      model->add_string( EXPORTING i_attribute_name = 'receiverSourceCode'
                                   i_string         = i_receiver_source_code ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_inheritance DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter i_subclass_element | the FAMIX element of the subclass Type
    "! @parameter i_subclass_name_group | the name group of the subclass
    "! @parameter i_subclass_name | the name of the subclass
    "! @parameter i_superclass_element | the FAMIX element of the subclass Type
    "! @parameter i_superclass_name_group | the name group
    "! @parameter i_superclass_name | the name of the subclass of the superclass
    METHODS set_sub_and_super_class
      IMPORTING
        i_subclass_element      TYPE string
        i_subclass_name_group   TYPE string
        i_subclass_name         TYPE string
        i_superclass_element    TYPE string
        i_superclass_name_group TYPE string
        i_superclass_name       TYPE string.

ENDCLASS.

CLASS cl_famix_inheritance IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Inheritance'.
  ENDMETHOD.
  METHOD set_sub_and_super_class.
    model->add_reference( EXPORTING  i_attribute_name    = 'subclass'
                                     i_elementname       = i_subclass_element
                                     i_name_group_of_reference = i_subclass_name_group
                                     i_name_of_reference = i_subclass_name ).
    model->add_reference( EXPORTING  i_attribute_name    = 'superclass'
                                     i_elementname       = i_superclass_element
                                     i_name_group_of_reference = i_superclass_name_group
                                     i_name_of_reference = i_superclass_name ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_reference DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter i_target_id | the FAMIX id of the target element
    "! @parameter i_source_id | the FAMIX id of the source element
    METHODS set_target_source
      IMPORTING
        i_target_id TYPE i
        i_source_id TYPE i.
ENDCLASS.

CLASS cl_famix_reference IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.Reference'.
  ENDMETHOD.

  METHOD set_target_source.

    model->add_reference_by_id( EXPORTING  i_attribute_name    = 'target'
                                           i_reference_id      = i_target_id ).
    model->add_reference_by_id( EXPORTING  i_attribute_name    = 'source'
                                           i_reference_id       = i_source_id ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_custom_source_lang DEFINITION INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! @parameter e_exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING i_name                          TYPE string
                EXPORTING VALUE(e_exists_already_with_id) TYPE i
                RETURNING VALUE(r_id)                     TYPE i.
    METHODS constructor IMPORTING i_model TYPE REF TO cl_model.
ENDCLASS.

CLASS cl_famix_custom_source_lang IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( i_model ).
    elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.

  METHOD add.
    r_id = model->add_entity( EXPORTING i_elementname = elementname i_is_named_entity = true i_can_be_referenced_by_name = true i_name = i_name
                              IMPORTING e_exists_already_with_id = e_exists_already_with_id ).
    mv_last_used_id = r_id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_make_demo_model DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS make
      EXPORTING
        et_model TYPE cl_model=>ty_lines.
ENDCLASS.

CLASS cl_make_demo_model IMPLEMENTATION.

  METHOD make.
    DATA(model) = NEW cl_model( ).

    DATA(famix_namespace) = NEW cl_famix_namespace( model ).
    DATA(famix_package) = NEW cl_famix_package( model ).
    DATA(famix_class) = NEW cl_famix_class( model ).
    DATA(famix_method) = NEW cl_famix_method( model ).
    DATA(famix_attribute) = NEW cl_famix_attribute( model ).
    DATA(famix_inheritance) = NEW cl_famix_inheritance( model ).

    famix_namespace->add( i_name = 'aNamespace' ).
    famix_package->add( i_name = 'aPackage' ).
    famix_package->add( i_name = 'anotherPackage' ).
    famix_package->set_parent_package( i_parent_package = 'aPackage' ).
    famix_class->add( i_name = 'ClassA' ).
    famix_class->set_container( EXPORTING i_container_element = 'FAMIX.Namespace'
                                          i_parent_container  = 'aNamespace').
    famix_class->set_parent_package( i_parent_package = 'aPackage' ).

    famix_method->add( i_name = 'methodA1' ).
    famix_method->set_signature( i_signature = 'methodA1()' ).
    famix_method->set_parent_type( i_parent_element = 'FAMIX.Class'
                                   i_parent_name    = 'ClassA' ).
    famix_attribute->add( i_name = 'attributeA1').
    famix_attribute->set_parent_type( i_parent_element = 'FAMIX.Class'
                                      i_parent_name    = 'ClassA' ).
    famix_class->add( i_name = 'ClassB').
    famix_class->set_container( i_container_element = 'FAMIX.Namespace'
                                i_parent_container  = 'aNamespace' ).
    famix_class->set_parent_package( i_parent_package = 'anotherPackage' ).

    famix_inheritance->add( ).
    famix_inheritance->set_sub_and_super_class( EXPORTING i_subclass_element   = 'FAMIX.Class'
                                                          i_subclass_name_group = ''
                                                          i_subclass_name      = 'ClassB'
                                                          i_superclass_element = 'FAMIX.Class'
                                                          i_superclass_name_group = ''
                                                          i_superclass_name    = 'ClassA' ).

*    DATA: lt_model TYPE cl_model=>ty_lines.

    model->make_mse( IMPORTING et_mse = et_model ).
  ENDMETHOD.

ENDCLASS.
******************************************** End Include Z_FAMIX_ABAP ******************************************************************************************