"! Analyses an mse file
"! See https://hal.inria.fr/hal-00646884/fr/ 2.2.1 for the specification of the grammar. This class follows the names given there.
CLASS ltcl_main DEFINITION DEFERRED.
CLASS z2mse_somix_harmonize DEFINITION LOCAL FRIENDS ltcl_main.
CLASS ltcl_main DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      equalize_harmonized FOR TESTING RAISING cx_static_check,
      somix_1_harmonized FOR TESTING RAISING cx_static_check,
      somix_2_harmonized FOR TESTING RAISING cx_static_check,
      somix_3_harmonized FOR TESTING RAISING cx_static_check,
      somix_4_harmonized FOR TESTING RAISING cx_static_check,
      _make_list_of_element_nodes FOR TESTING RAISING cx_static_check,
      _add_element_node FOR TESTING RAISING cx_static_check,
      _extract_element_node FOR TESTING RAISING cx_static_check,
      _ext_serial_attribute_nodes FOR TESTING RAISING cx_static_check,
      _ext_serial_attribute_nodes2 FOR TESTING RAISING cx_static_check,
      _ext_valuenodes FOR TESTING RAISING cx_static_check,
      _remove_apostroph FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_main IMPLEMENTATION.

  METHOD somix_1_harmonized.

    DATA: mse                          TYPE z2mse_somix_harmonize=>string_table,
          equalized_harmonized_mse_act TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_somix_harmonize=>harmonized_mse.

    mse = VALUE #( ( |( ( SOMIX.Grouping (id: 1)| )
                   ( |  (name 'Pack_1')| )
                   ( |  (uniqueName 'sap.pack_1')| )
                   ( |  (technicalType 'ABAPPackage'))| )
                  ).

    equalized_harmonized_mse_exp = VALUE #( ( |SOMIX.Grouping ABAPPackage.sap.pack_1 name Pack_1| )
                                             ).

    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( string_table = mse ).
    z2mse_somix_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonize simple Package specification correctly' ).


  ENDMETHOD.

  METHOD somix_2_harmonized.

    DATA: mse                          TYPE z2mse_somix_harmonize=>string_table,
          equalized_harmonized_mse_act TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_somix_harmonize=>harmonized_mse.

    mse = VALUE #( ( |( ( SOMIX.Data (id: 1)| )
                   ( |  (name 'attr')| )
                   ( |  (uniqueName 'sap.myclass.attr')| )
                   ( |  (technicalType 'ABAPClassAttribute'))| )
                  ).

    equalized_harmonized_mse_exp = VALUE #( ( |SOMIX.Data ABAPClassAttribute.sap.myclass.attr name attr| )
                                             ).

    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( string_table = mse ).
    z2mse_somix_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonize simple Package specification correctly' ).


  ENDMETHOD.

  METHOD somix_3_harmonized.

    DATA: mse                          TYPE z2mse_somix_harmonize=>string_table,
          equalized_harmonized_mse_act TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_somix_harmonize=>harmonized_mse.

    mse = VALUE #( ( |( ( SOMIX.Code (id: 1)| )
                   ( |  (name 'meth1')| )
                   ( |  (uniqueName 'sap.myclass.meth1')| )
                   ( |  (technicalType 'ABAPClassMethod'))| )
                  ).

    equalized_harmonized_mse_exp = VALUE #( ( |SOMIX.Code ABAPClassMethod.sap.myclass.meth1 name meth1| )
                                             ).

    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( string_table = mse ).
    z2mse_somix_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonize simple Package specification correctly' ).


  ENDMETHOD.

  METHOD somix_4_harmonized.

    DATA: mse                          TYPE z2mse_somix_harmonize=>string_table,
          equalized_harmonized_mse_act TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_somix_harmonize=>harmonized_mse.

    mse = VALUE #( ( |(| )
                   ( |( SOMIX.Grouping (id: 1)| )
                   ( |  (name 'Class_1')| )
                   ( |  (uniqueName 'sap.class_1')| )
                   ( |  (technicalType 'ABAPClass'))| )
                   ( |( SOMIX.Code (id: 2)| )
                   ( |  (name 'meth1')| )
                   ( |  (uniqueName 'sap.class_1.meth1')| )
                   ( |  (technicalType 'ABAPClassMethod'))| )
                   ( |( SOMIX.ParentChild| )
                   ( |  (parent (ref: 1))| )
                   ( |  (child (ref: 2)))| )
                   ( |)| )
                  ).

    equalized_harmonized_mse_exp = VALUE #(
                                            ( |SOMIX.Grouping ABAPClass.sap.class_1 name Class_1| )
                                            ( |SOMIX.Code ABAPClassMethod.sap.class_1.meth1 name meth1| )
                                            ( |SOMIX.ParentChild parent ABAPClass.sap.class_1 child ABAPClassMethod.sap.class_1.meth1| )
                                           ).

    equalized_harmonized_mse_act = z2mse_somix_harmonize=>mse_2_harmonized( string_table = mse ).
    z2mse_somix_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_exp ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonize simple Package specification correctly' ).


  ENDMETHOD.

  METHOD _add_element_node.

    DATA: element_node      TYPE string,
          element_nodes_act TYPE z2mse_somix_harmonize=>string_table,
          element_nodes_exp TYPE z2mse_somix_harmonize=>string_table.

    element_node = | |.

    z2mse_somix_harmonize=>_add_element_node( CHANGING element_node  = element_node
                                                     element_nodes = element_nodes_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = element_nodes_act
                                                  exp = element_nodes_exp
                                                  msg = 'Do not add a single blank' ).

  ENDMETHOD.

  METHOD _make_list_of_element_nodes.

    DATA msestr TYPE string.
    DATA element_nodes_act TYPE z2mse_somix_harmonize=>string_table.
    DATA element_nodes_exp TYPE z2mse_somix_harmonize=>string_table.

    msestr = |( (e1) (e2 2) (e3 a ( ) ) )|.
    element_nodes_exp = VALUE #( ( |(e1)| )
                                 ( |(e2 2)| )
                                 ( |(e3 a ( ) )| ) ).

    element_nodes_act = z2mse_somix_harmonize=>_make_list_of_element_nodes( i_msestr = msestr ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = element_nodes_act
                                                  exp = element_nodes_exp
                                                  msg = 'Expect correct list of element nodes' ).

  ENDMETHOD.

  METHOD _extract_element_node.

    DATA element_node TYPE string.
    DATA: elementname_act TYPE string,
          elementname_exp TYPE string.
    DATA: serial_attribute_nodes_act TYPE z2mse_somix_harmonize=>string_table,
          serial_attribute_nodes_exp TYPE z2mse_somix_harmonize=>string_table.

    CLEAR serial_attribute_nodes_act.

    element_node = |( ELEMENT (Serial) (AttributeNode) )|.

    z2mse_somix_harmonize=>_extract_element_node( EXPORTING element_node    = element_node
                                                IMPORTING elementname     = elementname_act
                                                          serial_attribute_nodes = serial_attribute_nodes_act ).
    elementname_exp = |ELEMENT|.
    serial_attribute_nodes_exp = VALUE #( ( |(Serial)| )
                                          ( |(AttributeNode)| ) ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = elementname_act
                                                  exp = elementname_exp
                                                  msg = 'Expect correct elementname' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = serial_attribute_nodes_act
                                                  exp = serial_attribute_nodes_exp
                                                  msg = 'Expect correct list of serial and attribute nodes' ).

  ENDMETHOD.

  METHOD _ext_serial_attribute_nodes.

    DATA serial_attribute_node  TYPE string.

    DATA is_serial_act TYPE abap_bool.
    DATA is_serial_exp TYPE abap_bool.
    DATA serial_id_act TYPE i.
    DATA serial_id_exp TYPE i.
    DATA simplename_act TYPE string.
    DATA simplename_exp TYPE string.
    DATA valuenodes_act TYPE z2mse_somix_harmonize=>string_table.
    DATA valuenodes_exp TYPE z2mse_somix_harmonize=>string_table.

    serial_attribute_node = |( id: 123 )|.
    is_serial_exp = abap_true.
    serial_id_exp = 123.
    simplename_exp = VALUE #( ).
    valuenodes_exp = VALUE #( ).

    z2mse_somix_harmonize=>_ext_serial_attribute_nodes( EXPORTING serial_attribute_node = serial_attribute_node
                                                      IMPORTING is_serial             = is_serial_act
                                                                serial_id             = serial_id_act
                                                                attributename            = simplename_act
                                                                valuenodes            = valuenodes_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = is_serial_act
                                                  exp = is_serial_exp
                                                  msg = 'Expect correct is_serial' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = serial_id_act
                                                  exp = serial_id_exp
                                                  msg = 'Expect correct serial_id' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = simplename_act
                                                  exp = simplename_exp
                                                  msg = 'Expect correct simplename' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = valuenodes_act
                                                  exp = valuenodes_exp
                                                  msg = 'Expect correct valuenodes' ).

  ENDMETHOD.

  METHOD _ext_serial_attribute_nodes2.

    DATA serial_attribute_node  TYPE string.

    DATA is_serial_act TYPE abap_bool.
    DATA is_serial_exp TYPE abap_bool.
    DATA serial_id_act TYPE i.
    DATA serial_id_exp TYPE i.
    DATA simplename_act TYPE string.
    DATA simplename_exp TYPE string.
    DATA valuenodes_act TYPE z2mse_somix_harmonize=>string_table.
    DATA valuenodes_exp TYPE z2mse_somix_harmonize=>string_table.

    serial_attribute_node = |( SimpleName ( ValueNode 1 )   ( ValueNode 2 ) )|.
    is_serial_exp = abap_false.
    serial_id_exp = VALUE #( ).
    simplename_exp = 'SimpleName'.
    valuenodes_exp = VALUE #( ( |( ValueNode 1 )| )
                              ( |( ValueNode 2 )| ) ).

    z2mse_somix_harmonize=>_ext_serial_attribute_nodes( EXPORTING serial_attribute_node = serial_attribute_node
                                                      IMPORTING is_serial             = is_serial_act
                                                                serial_id             = serial_id_act
                                                                attributename            = simplename_act
                                                                valuenodes            = valuenodes_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = is_serial_act
                                                  exp = is_serial_exp
                                                  msg = 'Expect correct is_serial' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = serial_id_act
                                                  exp = serial_id_exp
                                                  msg = 'Expect correct serial_id' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = simplename_act
                                                  exp = simplename_exp
                                                  msg = 'Expect correct simplename' ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = valuenodes_act
                                                  exp = valuenodes_exp
                                                  msg = 'Expect correct valuenodes' ).

  ENDMETHOD.

  METHOD _ext_valuenodes.


    DATA valuenode TYPE string.

    DATA is_primitive_act TYPE abap_bool.
    DATA primitive_act TYPE string.
    DATA is_integer_reference_act TYPE abap_bool.
    DATA integer_act TYPE i.
    DATA is_name_reference_act TYPE abap_bool.
    DATA ref_elementname_act TYPE string.
    DATA is_elementnode_act TYPE abap_bool.

    DATA is_primitive_exp TYPE abap_bool.
    DATA primitive_exp TYPE string.
    DATA is_integer_reference_exp TYPE abap_bool.
    DATA integer_exp TYPE i.
    DATA is_name_reference_exp TYPE abap_bool.
    DATA ref_elementname_exp TYPE string.
    DATA is_elementnode_exp TYPE abap_bool.

    DATA count TYPE i.

    DATA(repeat) = abap_true.

    WHILE repeat EQ abap_true.

      is_primitive_exp = VALUE #( ).
      primitive_exp = VALUE #( ).
      is_integer_reference_exp = VALUE #( ).
      integer_exp = VALUE #( ).
      is_name_reference_exp = VALUE #( ).
      ref_elementname_exp = VALUE #( ).
      is_elementnode_exp = VALUE #( ).

      ADD 1 TO count.

      CASE count.
        WHEN 1.
          valuenode = |123|.
          is_primitive_exp = abap_true.
          primitive_exp = |123|.
        WHEN 2.
          valuenode = |'ab'|.
          is_primitive_exp = abap_true.
          primitive_exp = |'ab'|.
        WHEN 3.
          valuenode = |true|.
          is_primitive_exp = abap_true.
          primitive_exp = |true|.
        WHEN 4.
          valuenode = |(ref: 123)|.
          is_integer_reference_exp = abap_true.
          integer_exp = 123.
        WHEN 5.
          valuenode = |(ref: ElementName)|.
          is_name_reference_exp = abap_true.
          ref_elementname_exp = |ElementName|.
        WHEN 6.
          valuenode = |(ElementNode)|.
          is_elementnode_exp = abap_true.
        WHEN OTHERS.
          repeat = abap_true.
          EXIT.
      ENDCASE.



      z2mse_somix_harmonize=>_ext_valuenodes( EXPORTING valuenode = valuenode
                                            IMPORTING is_primitive = is_primitive_act
                                                      primitive = primitive_act
                                                      is_integer_reference = is_integer_reference_act
                                                      integer_reference = integer_act
                                                      is_name_reference = is_name_reference_act
                                                      ref_elementname = ref_elementname_act
                                                      is_elementnode = is_elementnode_act ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = is_primitive_act
                                                    exp = is_primitive_exp
                                                    msg = |Expect correct is_primitive : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = primitive_act
                                                    exp = primitive_exp
                                                    msg = |Expect correct primitive : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = is_integer_reference_act
                                                    exp = is_integer_reference_exp
                                                    msg = |Expect correct is_integer_reference : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = integer_act
                                                    exp = integer_exp
                                                    msg = |Expect correct is_integer_reference : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = is_name_reference_act
                                                    exp = is_name_reference_exp
                                                    msg = |Expect correct is_name_reference : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = ref_elementname_act
                                                    exp = ref_elementname_exp
                                                    msg = |Expect correct is_name_reference : | && |{ valuenode }| ).

      cl_abap_unit_assert=>assert_equals( EXPORTING act = is_elementnode_act
                                                    exp = is_elementnode_exp
                                                    msg = |Expect correct is_elementnode : | && |{ valuenode }| ).

    ENDWHILE.

  ENDMETHOD.

  METHOD equalize_harmonized.
    DATA: harmonized_mse               TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_act TYPE z2mse_somix_harmonize=>harmonized_mse,
          equalized_harmonized_mse_exp TYPE z2mse_somix_harmonize=>harmonized_mse.

    harmonized_mse = VALUE #( ( |B      B| ) "Many blanks between both B
                              ( |A| )
                              (  ) ).

    equalized_harmonized_mse_exp = VALUE #( ( |A| )
                                            ( |B B| ) ). "Only a single blank between both B
    equalized_harmonized_mse_act = harmonized_mse.
    z2mse_somix_harmonize=>equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = equalized_harmonized_mse_act
                                                  exp = equalized_harmonized_mse_exp
                                                  msg = 'Harmonized mse has to be strictly sorted alphabetically by method equalize_harmonized. Multiple blanks are removed' ).

  ENDMETHOD.

  METHOD _remove_apostroph.

    DATA: value_act TYPE string,
          value_exp TYPE string.

    value_act = |'AString'|.

    value_exp = |AString|.

    z2mse_somix_harmonize=>_remove_apostroph( CHANGING string = value_act ).

    cl_abap_unit_assert=>assert_equals( EXPORTING act = value_act
                                                  exp = value_exp
                                                  msg = 'Apostrophs are to be removed' ).


  ENDMETHOD.

ENDCLASS.
