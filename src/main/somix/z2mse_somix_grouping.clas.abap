class Z2MSE_SOMIX_GROUPING definition
  public
  inheriting from Z2MSE_SOMIX_ELEMENT
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !MODEL type ref to Z2MSE_MODEL .
    "! Returns the ID for a given code. May use a grouping it is contained in.
    "! Returns 0 if the data is not known
    "! @parameter grouping_name_group | the name group of the grouping
    "! @parameter grouping | the grouping
    "! @parameter code_name_group | the name group of the code
    "! @parameter code | the ID of the element
    METHODS get_id
      IMPORTING grouping_name_group TYPE clike
                grouping            TYPE clike
      RETURNING VALUE(id)           TYPE i.
    "! Store the relation between class, method name and id in internal table to enable associations
    "! Call before performing the next time the method add, because the ID is stored internally after creating an element
    "! @parameter grouping_name_group | the name group of the grouping
    "! @parameter grouping | the grouping
    "! @parameter code_name_group | the name group of the code
    "! @parameter code | the ID of the element
    METHODS store_id
      IMPORTING grouping_name_group TYPE clike
                grouping            TYPE clike.
protected section.
private section.
    TYPES: BEGIN OF ty_grouping_id,
             grouping_name_group TYPE string,
             grouping            TYPE string,
             id                  TYPE i,
           END OF ty_grouping_id.
    DATA: g_grouping_ids TYPE HASHED TABLE OF ty_grouping_id WITH UNIQUE KEY grouping_name_group grouping.
ENDCLASS.



CLASS Z2MSE_SOMIX_GROUPING IMPLEMENTATION.


  method CONSTRUCTOR.
    CALL METHOD super->constructor( model ).
    g_elementname = 'SOMIX.Grouping'.
  endmethod.
  METHOD GET_ID.
    FIELD-SYMBOLS <grouping_id> LIKE LINE OF g_grouping_ids.

    READ TABLE g_grouping_ids ASSIGNING <grouping_id> WITH TABLE KEY grouping_name_group = grouping_name_group
                                                                     grouping = grouping.
    IF sy-subrc EQ 0. "OK
      id = <grouping_id>-id.
    ELSE.
      id = 0.
    ENDIF.
  ENDMETHOD.

  METHOD STORE_ID.
    DATA ls_grouping_id LIKE LINE OF g_grouping_ids. " ABAP 7.31 use prefix ls_ to prevent shadowing after conversion
    CLEAR ls_grouping_id.
    ls_grouping_id-id                  = g_last_used_id.
    ls_grouping_id-grouping_name_group = grouping_name_group.
    ls_grouping_id-grouping            = grouping.
    INSERT ls_grouping_id INTO TABLE g_grouping_ids.
  ENDMETHOD.

ENDCLASS.
