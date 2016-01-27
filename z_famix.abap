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
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
  PROTECTED SECTION.
    DATA g_elementname TYPE string.
    DATA g_model TYPE REF TO cl_model.
    DATA g_last_used_id TYPE i.
ENDCLASS.

CLASS cl_famix_entity IMPLEMENTATION.

  METHOD constructor.
    g_model = model.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_sourced_entity DEFINITION ABSTRACT INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! Declare source language
    "! @parameter source_language_element | the FAMIX element of the source language
    "! @parameter source_language_name | the name of the source language
    METHODS set_declared_source_language
      IMPORTING
        source_language_element TYPE string
        source_language_name    TYPE string.
ENDCLASS.

CLASS cl_famix_sourced_entity IMPLEMENTATION.

  METHOD set_declared_source_language.
    g_model->add_reference( EXPORTING attribute_name    = 'declaredSourceLanguage'
                                      elementname       = source_language_element
                                      name_of_reference = source_language_name ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_named_entity DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.

    "! Call once to create a new named entity
    "! @parameter exists_already_with_id | contains the id if entry already existed
    "! @parameter id | the id in model either if just created or already existing
    METHODS add
      IMPORTING name_group                    TYPE string OPTIONAL
                name                          TYPE string
      EXPORTING VALUE(exists_already_with_id) TYPE i
      RETURNING VALUE(id)                     TYPE i.
    "! Call once to set the parent package
    "! @parameter i_parent_package | the name of an element of type FAMIX.Package
    METHODS set_parent_package IMPORTING parent_package TYPE string.

  PROTECTED SECTION.

ENDCLASS.

CLASS cl_famix_named_entity IMPLEMENTATION.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name_group = name_group
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

  METHOD set_parent_package.
    g_model->add_reference( elementname       = 'FAMIX.Package'
                            name_of_reference = parent_package
                            attribute_name    = 'parentPackage' ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_parameter DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! Set the parent behavioural entity, either a method or a function
    "! @parameter i_parent_id | id of parent entity
    METHODS set_parent_behavioural_entity
      IMPORTING
        parent_id TYPE i.
ENDCLASS.

CLASS cl_famix_parameter IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Parameter'.
  ENDMETHOD.

  METHOD set_parent_behavioural_entity.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'parentBehaviouralEntity'
                                            reference_id   = parent_id ).
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = false
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_attribute DEFINITION INHERITING FROM cl_famix_named_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Store the relation between class, attribute name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    METHODS store_id
      IMPORTING
        class     TYPE string
        attribute TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter attribute | the attribute name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                class     TYPE string
                attribute TYPE string
      RETURNING VALUE(id) TYPE i.
    METHODS add REDEFINITION.

    "! set the parent type, for instance the class the method is contained in
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | the name of the parent element
    METHODS set_parent_type
      IMPORTING
        parent_element TYPE string
        parent_name    TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF attribute_id_type,
             class     TYPE string,
             attribute TYPE string,
             id        TYPE i,
           END OF attribute_id_type.
    DATA: g_attribute_ids TYPE HASHED TABLE OF attribute_id_type WITH UNIQUE KEY class attribute.
ENDCLASS.

CLASS cl_famix_attribute IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Attribute'.
  ENDMETHOD.
  METHOD set_parent_type.
    g_model->add_reference( EXPORTING attribute_name    = 'parentType'
                                      elementname       = parent_element
                                      name_of_reference = parent_name ).
  ENDMETHOD.
  METHOD add.
    id = g_model->add_entity( elementname = g_elementname
                              is_named_entity = true
                              can_be_referenced_by_name = false
                              name = name ).
    g_last_used_id = id.
  ENDMETHOD.


  METHOD store_id.
    g_attribute_ids = VALUE #( BASE g_attribute_ids ( id        = g_last_used_id
                                                    class     = class
                                                    attribute = attribute ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE g_attribute_ids ASSIGNING FIELD-SYMBOL(<attribute_id>) WITH TABLE KEY class = class attribute = attribute.
    IF sy-subrc EQ ok.
      id = <attribute_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_container_entity DEFINITION INHERITING FROM cl_famix_named_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the container an element is in
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container | the name of the Container
    METHODS set_container IMPORTING container_element TYPE string
                                    parent_container TYPE string.
    "! Set the container an element is in using the reference
    "! @parameter container_element | the FAMIX element of the Container
    "! @parameter parent_container_id | the id of the Container
    METHODS set_container_by_id IMPORTING container_element TYPE string
                                          parent_container_id TYPE i.
  PROTECTED SECTION.

ENDCLASS.

CLASS cl_famix_container_entity IMPLEMENTATION.

  METHOD set_container.
    g_model->add_reference( EXPORTING elementname       = container_element
                                      name_of_reference = parent_container
                                      attribute_name    = 'container' ).
  ENDMETHOD.

  METHOD set_container_by_id.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'container'
                                            reference_id   = parent_container_id ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_behavioural_entity DEFINITION INHERITING FROM cl_famix_container_entity ABSTRACT.
  PUBLIC SECTION.
    "! Set the signature of a method
    "! This might not be relevant for ABAP, but is contained here for completeness
    "! @parameter signature | The signature like myMethod( myParameters, ...)
    METHODS set_signature IMPORTING signature TYPE string.

ENDCLASS.

CLASS cl_famix_behavioural_entity IMPLEMENTATION.

  METHOD set_signature.
    g_model->add_string( EXPORTING attribute_name = 'signature'
                                   string         = signature ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_namespace DEFINITION INHERITING FROM cl_famix_container_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

ENDCLASS.

CLASS cl_famix_namespace IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Namespace'.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_package DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_package IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Package'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_module DEFINITION INHERITING FROM cl_famix_named_entity.

  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS add REDEFINITION.

ENDCLASS.

CLASS cl_famix_module IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Module'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_method DEFINITION INHERITING FROM cl_famix_behavioural_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    METHODS add REDEFINITION.
    "! set the parent type, for instance the class the method is contained in
    "! Provide either parent_name or parent_id
    "! @parameter parent_element | the FAMIX element of the parent Type
    "! @parameter parent_name | optional the name of the parent element
    "! @parameter parent_id | optional the id of the parent element
    METHODS set_parent_type
      IMPORTING
        parent_element TYPE string
        parent_name    TYPE string OPTIONAL
        parent_id      TYPE i OPTIONAL.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    METHODS store_id
      IMPORTING
        class  TYPE string
        method TYPE string.
    "! Returns the ID for a given method of a class
    "! Returns 0 if the class is not known
    "! @parameter class | the class of the method
    "! @parameter method | the method name
    "! @parameter id | the ID of the element
    METHODS get_id
      IMPORTING
                class     TYPE string
                method    TYPE string
      RETURNING VALUE(id) TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_method_id,
             class  TYPE string,
             method TYPE string,
             id     TYPE i,
           END OF ty_method_id.
    DATA: g_method_ids TYPE HASHED TABLE OF ty_method_id WITH UNIQUE KEY class method.
ENDCLASS.

CLASS cl_famix_method IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Method'.
  ENDMETHOD.

  METHOD set_parent_type.
    IF parent_name IS SUPPLIED.
      g_model->add_reference( EXPORTING attribute_name    = 'parentType'
                                        elementname       = parent_element
                                        name_of_reference = parent_name ).
    ELSEIF parent_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING attribute_name = 'parentType'
                                              reference_id   = parent_id ).
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( elementname               = g_elementname
                              is_named_entity           = true
                              can_be_referenced_by_name = false
                              name = name ).
    g_last_used_id = id.
  ENDMETHOD.

  METHOD store_id.
    g_method_ids = VALUE #( BASE g_method_ids ( id    = g_last_used_id
                                                class = class method = method ) ).
  ENDMETHOD.

  METHOD get_id.
    READ TABLE g_method_ids ASSIGNING FIELD-SYMBOL(<method_id>) WITH TABLE KEY class = class
                                                                                  method = method.
    IF sy-subrc EQ ok.
      id = <method_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_class DEFINITION INHERITING FROM cl_famix_container_entity.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Set if it is an interface
    METHODS is_interface.
ENDCLASS.

CLASS cl_famix_class IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Class'.
  ENDMETHOD.

  METHOD is_interface.
    g_model->add_boolean( EXPORTING attribute_name = 'isInterface'
                                    is_true        = true ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_association DEFINITION INHERITING FROM cl_famix_sourced_entity ABSTRACT.
  PUBLIC SECTION.
    METHODS add
      RETURNING VALUE(id) TYPE i.
ENDCLASS.

CLASS cl_famix_association IMPLEMENTATION.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname               = g_elementname
                                        is_named_entity           = false
                                        can_be_referenced_by_name = false ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_access DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! Checks that accessor and variable of an access are a new access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS is_new_access
      IMPORTING
                accessor_id   TYPE i
                variable_id   TYPE i
      RETURNING VALUE(is_new) TYPE bool.
    "! defines accessor and variable of an access
    "! @parameter accessor_id | the accessing method or function (type BehaviouralEntity)
    "! @parameter variable_id | the accessed parameter, variable ... (type StructuralEntity)
    METHODS set_accessor_variable_relation
      IMPORTING
        accessor_id TYPE i
        variable_id TYPE i.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_accessor_variable_id,
             accessor_id TYPE i,
             variable_id TYPE i,
           END OF  ty_accessor_variable_id.
    DATA: g_accessor_variable_ids TYPE HASHED TABLE OF ty_accessor_variable_id WITH UNIQUE KEY accessor_id variable_id.
ENDCLASS.

CLASS cl_famix_access IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Access'.
  ENDMETHOD.

  METHOD set_accessor_variable_relation.
    g_accessor_variable_ids = VALUE #( BASE g_accessor_variable_ids ( accessor_id = accessor_id variable_id = variable_id ) ).
    g_model->add_reference_by_id( EXPORTING attribute_name = 'accessor'
                                            reference_id   = accessor_id ).
    g_model->add_reference_by_id( EXPORTING attribute_name = 'variable'
                                            reference_id   = variable_id ).
  ENDMETHOD.

  METHOD is_new_access.
    READ TABLE g_accessor_variable_ids TRANSPORTING NO FIELDS WITH TABLE KEY accessor_id = accessor_id variable_id = variable_id.
    IF sy-subrc <> ok.
      is_new = true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_invocation DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.

    METHODS is_new_invocation_to_candidate
      IMPORTING
                sender_id     TYPE i
                candidates_id TYPE i
      RETURNING VALUE(is_new) TYPE bool.

    "! defines an invocation
    "! this also models standard call by functions or methods to objects other than attributes
    "! Us this method to reference the receiver using his id
    "! @parameter sender_id | the id of the sender or calling method or function
    "! @parameter candidates_id | the id of the candidate, this is the used method or function of type BehaviouralEntity in case of method or function usage
    "! @parameter receiver_id | optional the id of the receiver or called method or function
    "! @parameter signature | optional a signature
    "! @parameter receiver_source_code | optional a receiver source code
    METHODS set_invocation_by_reference
      IMPORTING
        sender_id            TYPE i
        candidates_id        TYPE i OPTIONAL
        receiver_id          TYPE i OPTIONAL
        signature            TYPE string OPTIONAL
        receiver_source_code TYPE string OPTIONAL.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sender_candidate,
             sender_id     TYPE i,
             candidates_id TYPE i,
           END OF ty_sender_candidate.

    DATA g_sender_candidates TYPE HASHED TABLE OF ty_sender_candidate WITH UNIQUE KEY sender_id candidates_id.

ENDCLASS.

CLASS cl_famix_invocation IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Invocation'.
  ENDMETHOD.

  METHOD is_new_invocation_to_candidate.
    READ TABLE g_sender_candidates TRANSPORTING NO FIELDS WITH TABLE KEY sender_id = sender_id candidates_id = candidates_id.
    IF sy-subrc <> ok.
      is_new = true.
    ENDIF.
  ENDMETHOD.

  METHOD set_invocation_by_reference.
    g_model->add_reference_by_id( EXPORTING attribute_name = 'sender'
                                            reference_id   = sender_id ).
    IF candidates_id IS SUPPLIED.
      g_sender_candidates = VALUE #( BASE g_sender_candidates ( sender_id = sender_id candidates_id = candidates_id ) ).
      g_model->add_reference_by_id( EXPORTING attribute_name = 'candidates'
                                              reference_id   = candidates_id ).
    ENDIF.

    IF receiver_id IS SUPPLIED.
      g_model->add_reference_by_id( EXPORTING attribute_name = 'receiver'
                                              reference_id   = receiver_id ).
    ENDIF.
    IF signature IS SUPPLIED.
      g_model->add_string( EXPORTING attribute_name = 'signature'
                                     string         = signature ).
    ENDIF.
    IF receiver_source_code IS SUPPLIED.
      g_model->add_string( EXPORTING attribute_name = 'receiverSourceCode'
                                     string         = receiver_source_code ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_inheritance DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter subclass_element | the FAMIX element of the subclass Type
    "! @parameter subclass_name_group | the name group of the subclass
    "! @parameter subclass_name | the name of the subclass
    "! @parameter superclass_element | the FAMIX element of the subclass Type
    "! @parameter superclass_name_group | the name group
    "! @parameter superclass_name | the name of the subclass of the superclass
    METHODS set_sub_and_super_class
      IMPORTING
        subclass_element      TYPE string
        subclass_name_group   TYPE string
        subclass_name         TYPE string
        superclass_element    TYPE string
        superclass_name_group TYPE string
        superclass_name       TYPE string.

ENDCLASS.

CLASS cl_famix_inheritance IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Inheritance'.
  ENDMETHOD.
  METHOD set_sub_and_super_class.
    g_model->add_reference( EXPORTING attribute_name          = 'subclass'
                                      elementname             = subclass_element
                                      name_group_of_reference = subclass_name_group
                                      name_of_reference       = subclass_name ).
    g_model->add_reference( EXPORTING attribute_name          = 'superclass'
                                      elementname             = superclass_element
                                      name_group_of_reference = superclass_name_group
                                      name_of_reference       = superclass_name ).

  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_reference DEFINITION INHERITING FROM cl_famix_association.
  PUBLIC SECTION.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
    "! defines an inheritance
    "! @parameter target_id | the FAMIX id of the target element
    "! @parameter source_id | the FAMIX id of the source element
    METHODS set_target_source
      IMPORTING
        target_id TYPE i
        source_id TYPE i.
ENDCLASS.

CLASS cl_famix_reference IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.Reference'.
  ENDMETHOD.

  METHOD set_target_source.

    g_model->add_reference_by_id( EXPORTING attribute_name    = 'target'
                                            reference_id      = target_id ).
    g_model->add_reference_by_id( EXPORTING attribute_name    = 'source'
                                            reference_id       = source_id ).
  ENDMETHOD.

ENDCLASS.

CLASS cl_famix_custom_source_lang DEFINITION INHERITING FROM cl_famix_entity.
  PUBLIC SECTION.
    "! @parameter exists_already_with_id | contains the id if entry already existed
    METHODS add IMPORTING name                          TYPE string
                EXPORTING VALUE(exists_already_with_id) TYPE i
                RETURNING VALUE(id)                     TYPE i.
    METHODS constructor IMPORTING model TYPE REF TO cl_model.
ENDCLASS.

CLASS cl_famix_custom_source_lang IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor( model ).
    g_elementname = 'FAMIX.CustomSourceLanguage'.
  ENDMETHOD.

  METHOD add.
    id = g_model->add_entity( EXPORTING elementname = g_elementname
                                        is_named_entity = true
                                        can_be_referenced_by_name = true
                                        name = name
                              IMPORTING exists_already_with_id = exists_already_with_id ).
    g_last_used_id = id.
  ENDMETHOD.

ENDCLASS.

CLASS cl_make_demo_model DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS make
      EXPORTING
        mse_model TYPE cl_model=>lines_type.
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

    famix_namespace->add( name = 'aNamespace' ).
    famix_package->add( name = 'aPackage' ).
    famix_package->add( name = 'anotherPackage' ).
    famix_package->set_parent_package( parent_package = 'aPackage' ).
    famix_class->add( name = 'ClassA' ).
    famix_class->set_container( EXPORTING container_element = 'FAMIX.Namespace'
                                          parent_container  = 'aNamespace').
    famix_class->set_parent_package( parent_package = 'aPackage' ).

    famix_method->add( name = 'methodA1' ).
    famix_method->set_signature( signature = 'methodA1()' ).
    famix_method->set_parent_type( parent_element = 'FAMIX.Class'
                                   parent_name    = 'ClassA' ).
    famix_attribute->add( name = 'attributeA1').
    famix_attribute->set_parent_type( parent_element = 'FAMIX.Class'
                                      parent_name    = 'ClassA' ).
    famix_class->add( name = 'ClassB').
    famix_class->set_container( container_element = 'FAMIX.Namespace'
                                parent_container  = 'aNamespace' ).
    famix_class->set_parent_package( parent_package = 'anotherPackage' ).

    famix_inheritance->add( ).
    famix_inheritance->set_sub_and_super_class( EXPORTING subclass_element   = 'FAMIX.Class'
                                                          subclass_name_group = ''
                                                          subclass_name      = 'ClassB'
                                                          superclass_element = 'FAMIX.Class'
                                                          superclass_name_group = ''
                                                          superclass_name    = 'ClassA' ).

    model->make_mse( IMPORTING mse_model = mse_model ).
  ENDMETHOD.

ENDCLASS.
******************************************** End Include Z_FAMIX_ABAP ******************************************************************************************