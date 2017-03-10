"! Used in Unit Tests to simplify checking for correct Moose models.
"! Will not be delivered as local class, thus use new ABAP statements.
"! Is not optimized to handle general mse files fast but only to support the unit test in this application.
CLASS z2mse_mse_harmonize DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES mse            TYPE TABLE OF string WITH DEFAULT KEY.
    TYPES harmonized_mse TYPE TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS:
      mse_2_harmonized
        IMPORTING
                  string_table                    TYPE mse OPTIONAL
                  mse                             TYPE z2mse_model=>lines_type OPTIONAL
        RETURNING VALUE(equalized_harmonized_mse) TYPE harmonized_mse,
      equalize_harmonized
        CHANGING harmonized_mse TYPE harmonized_mse.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES string_table TYPE TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS _add_element_node
      CHANGING
        element_node  TYPE string
        element_nodes TYPE string_table.
    CLASS-METHODS _make_list_of_element_nodes
      IMPORTING
        i_msestr               TYPE string
      RETURNING
        VALUE(r_element_nodes) TYPE string_table.
    CLASS-METHODS _extract_element_node
      IMPORTING
        element_node                  TYPE string
      EXPORTING
        VALUE(elementname)            TYPE string
        VALUE(serial_attribute_nodes) TYPE string_table.
    CLASS-METHODS _ext_serial_attribute_nodes
      IMPORTING
        serial_attribute_node TYPE string
      EXPORTING
        VALUE(is_serial)      TYPE abap_bool
        VALUE(serial_id)      TYPE i
        VALUE(attributename)  TYPE string
        VALUE(valuenodes)     TYPE string_table.
    CLASS-METHODS _ext_valuenodes
      IMPORTING
        valuenode                   TYPE string
      EXPORTING
        VALUE(is_primitive)         TYPE abap_bool
        VALUE(primitive)            TYPE string
        VALUE(is_integer_reference) TYPE abap_bool
        VALUE(integer_reference)    TYPE i
        VALUE(is_name_reference)    TYPE abap_bool
        VALUE(ref_elementname)      TYPE string
        VALUE(is_elementnode)       TYPE abap_bool.
    CLASS-METHODS _remove_apostroph
      CHANGING string TYPE string.
ENDCLASS.



CLASS z2mse_mse_harmonize IMPLEMENTATION.


  METHOD equalize_harmonized.

    SORT harmonized_mse.
    LOOP AT harmonized_mse ASSIGNING FIELD-SYMBOL(<eq>).

      CONDENSE <eq>.
      IF <eq> eq ''.
        DELETE harmonized_mse.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD mse_2_harmonized.

    DATA msestr TYPE string.
    " Make flat string where linebreaks are replaced by blanks
    IF mse IS SUPPLIED.
      LOOP AT mse INTO DATA(line).
        msestr = msestr && ' ' && line-line.
      ENDLOOP.
    ELSEIF string_table IS SUPPLIED.
      LOOP AT string_table INTO DATA(string_line).
        msestr = msestr && ' ' && string_line.
      ENDLOOP.
    ELSE.
      ASSERT 1 = 2.
    ENDIF.
    " Counts element nodes
    DATA element_node TYPE string.
    DATA element_nodes TYPE string_table.

    element_nodes = _make_list_of_element_nodes( msestr ).

    TYPES: BEGIN OF id_to_name,
             id                TYPE i,
             simple_name       TYPE string,
             parent_id         TYPE i,
             concatenated_name TYPE string,
           END OF id_to_name.

    DATA: id_to_name  TYPE id_to_name,
          id_to_names TYPE HASHED TABLE OF id_to_name WITH UNIQUE KEY id.

    TYPES: BEGIN OF element,
             elementname               TYPE string,
             element_id                TYPE i,
             concatenated_name         TYPE string,
             has_no_attribute          TYPE abap_bool,
             attribute                 TYPE string,
             is_integer_reference      TYPE abap_bool,
             integer_reference         TYPE i,
             value                     TYPE string,
             access_accessor_ref       TYPE i,
             access_variable_ref       TYPE i,
             invocation_sender_ref     TYPE i,
             invocation_candidates_ref TYPE i,
             invocation_signatur       TYPE string,
           END OF element.

    DATA: element  TYPE element,
          elements TYPE STANDARD TABLE OF element WITH DEFAULT KEY.


    LOOP AT element_nodes INTO element_node.

      CLEAR id_to_name.
      CLEAR element.

      DATA serial TYPE string.
      CLEAR serial.

      DATA serial_attribute_nodes TYPE string_table.
      CLEAR serial_attribute_nodes.

      DATA elementname TYPE string.
      CLEAR elementname.

      _extract_element_node( EXPORTING element_node    = element_node
                             IMPORTING elementname     = elementname
                                       serial_attribute_nodes = serial_attribute_nodes ).

      DATA serial_attribute_node TYPE string.
      DATA(has_attribute) = abap_false.

      element-elementname = elementname.

      LOOP AT serial_attribute_nodes INTO serial_attribute_node.

        DATA serial_id TYPE i.
        DATA attributename TYPE string.
        DATA valuenodes TYPE string_table.
        DATA is_serial TYPE abap_bool.

        _ext_serial_attribute_nodes( EXPORTING serial_attribute_node = serial_attribute_node
                                     IMPORTING is_serial             = is_serial
                                               serial_id             = serial_id
                                               attributename            = attributename
                                               valuenodes            = valuenodes ).

        IF is_serial EQ abap_true.

          id_to_name-id = serial_id.
          element-element_id = serial_id.

        ELSE.

          element-attribute = attributename.

          DATA valuenode TYPE string.
          LOOP AT valuenodes INTO valuenode.

            DATA is_primitive TYPE abap_bool.
            DATA primitive TYPE string.
            DATA is_integer_reference TYPE abap_bool.
            DATA integer_reference TYPE i.
            DATA is_name_reference TYPE abap_bool.
            DATA ref_elementname TYPE string.
            DATA is_elementnode TYPE abap_bool.

*            element-access_accessor_ref = VALUE #( ).
*            element-access_variable_ref = VALUE #( )..
*            element-invocation_sender_ref = VALUE #( ).
*            element-invocation_candidates_ref = VALUE #( ).
*            element-invocation_signatur = VALUE #( ).
            element-is_integer_reference = VALUE #( ).
            element-integer_reference = VALUE #( ).
            element-value = VALUE #( ).

            _ext_valuenodes( EXPORTING valuenode = valuenode
                             IMPORTING is_primitive = is_primitive
                                       primitive = primitive
                                       is_integer_reference = is_integer_reference
                                       integer_reference = integer_reference
                                       is_name_reference = is_name_reference
                                       ref_elementname = ref_elementname
                                       is_elementnode = is_elementnode ).

            IF attributename EQ 'name' AND
               is_primitive EQ abap_true.
              id_to_name-simple_name = primitive.

              _remove_apostroph( CHANGING string = id_to_name-simple_name ).

            ELSEIF ( attributename EQ 'parentType' OR
                     attributename EQ 'parentPackage' ) AND
               is_integer_reference EQ abap_true AND
               ( element-elementname <> 'FAMIX.Class' AND
                 element-elementname <> 'FAMIX.Package' ) .
              id_to_name-parent_id = integer_reference.
            ELSE.
*            ENDIF.
*
*            IF simplename <> 'name'.

              CASE element-elementname.
                WHEN 'FAMIX.Access'.
                  CASE attributename.
                    WHEN 'accessor'.
                      element-access_accessor_ref = integer_reference.
                    WHEN 'variable'.
                      element-access_variable_ref = integer_reference.
                  ENDCASE.
                WHEN 'FAMIX.Invocation'.
                  CASE attributename.
                    WHEN 'sender'.
                      element-invocation_sender_ref = integer_reference.
                    WHEN 'candidates'.
                      element-invocation_candidates_ref = integer_reference.
                    WHEN 'signature'.
                      element-invocation_signatur = primitive.
                  ENDCASE.
                WHEN OTHERS.



                  IF is_integer_reference EQ abap_true.
                    element-is_integer_reference = abap_true.
                    element-integer_reference = integer_reference.
                  ELSEIF is_name_reference EQ abap_true.
                    element-value = is_name_reference.
                  ELSEIF is_primitive EQ abap_true.
                    element-value = primitive.
                  ENDIF.
                  IF element-elementname EQ 'FAMIX.Method' AND element-attribute EQ 'parentType'.
                    " Not required
                  ELSE.
                    INSERT element INTO TABLE elements.
                    has_attribute = abap_true.
                  ENDIF.

              ENDCASE.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDLOOP.


      IF has_attribute EQ abap_false.
        element-has_no_attribute = abap_true.
        INSERT element INTO TABLE elements.
      ENDIF.

      IF id_to_name-simple_name IS NOT INITIAL.
        INSERT id_to_name INTO TABLE id_to_names.
      ENDIF.

    ENDLOOP.

    " Get concatenated names

    LOOP AT id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name>).
      READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name2>) WITH TABLE KEY id = <id_to_name>-parent_id.
      IF sy-subrc EQ 0.
        <id_to_name>-concatenated_name = |{ <id_to_name2>-simple_name }| && |>>| && |{ <id_to_name>-simple_name }|.
      ENDIF.
    ENDLOOP.

    " Find concatenated names

    LOOP AT elements ASSIGNING FIELD-SYMBOL(<element>).
      IF <element>-element_id IS NOT INITIAL.
        READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name3>) WITH TABLE KEY id = <element>-element_id.
        IF sy-subrc EQ 0.
          IF <id_to_name3>-simple_name IS NOT INITIAL
          AND <element>-elementname EQ 'FAMIX.Class' OR <element>-elementname EQ 'FAMIX.Package'.
            <element>-concatenated_name = <id_to_name3>-simple_name.
          ELSE.
            <element>-concatenated_name = <id_to_name3>-concatenated_name.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Build result

    DATA: result     TYPE string,
          accessor   TYPE string,
          variable   TYPE string,
          sender     TYPE string,
          candidates TYPE string,
          signature  TYPE string,
          value      TYPE string.

    LOOP AT elements INTO element.
      CLEAR: result, accessor, variable, sender, candidates, value.

      IF  element-elementname EQ 'FAMIX.Access'.

        READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name4>) WITH TABLE KEY id = element-access_accessor_ref.
        IF sy-subrc EQ 0.
          accessor = <id_to_name4>-concatenated_name.
        ENDIF.

        READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name5>) WITH TABLE KEY id = element-access_variable_ref.
        IF sy-subrc EQ 0.
          variable = <id_to_name5>-concatenated_name.
        ENDIF.

        result = |{ element-elementname } accessor | && |{ accessor } variable | && |{ variable }|.

      ELSEIF element-elementname EQ 'FAMIX.Invocation'.

        READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name6>) WITH TABLE KEY id = element-invocation_sender_ref.
        IF sy-subrc EQ 0.
          sender = <id_to_name6>-concatenated_name.
        ENDIF.

        READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name7>) WITH TABLE KEY id = element-invocation_candidates_ref.
        IF sy-subrc EQ 0.
          candidates = <id_to_name7>-concatenated_name.
        ENDIF.

        _remove_apostroph( CHANGING string = element-invocation_signatur ).

        result = |{ element-elementname } sender | && |{ sender } candidates | && |{ candidates } signature | && |{ element-invocation_signatur }|.

      ELSE.

        IF element-has_no_attribute EQ abap_true.

          result = |{ element-elementname } | && |{ element-concatenated_name }|.

        ELSE.

          IF element-is_integer_reference EQ abap_true.

            READ TABLE id_to_names ASSIGNING FIELD-SYMBOL(<id_to_name8>) WITH TABLE KEY id = element-integer_reference.
            IF sy-subrc EQ 0.
              IF <id_to_name8>-concatenated_name IS NOT INITIAL.
                value = <id_to_name8>-concatenated_name.
              ELSE.
                value = <id_to_name8>-simple_name.
              ENDIF.

              result = |{ element-elementname } | && |{ element-concatenated_name } | && |{ element-attribute } | && |{ value }|.

            ENDIF.

          ELSE.

            _remove_apostroph( CHANGING string = element-value ).

            result = |{ element-elementname } | && |{ element-concatenated_name } | && |{ element-attribute } | && |{ element-value }|.

          ENDIF.

        ENDIF.

      ENDIF.

      INSERT result INTO TABLE equalized_harmonized_mse.

    ENDLOOP.

    equalize_harmonized( CHANGING harmonized_mse = equalized_harmonized_mse ).

  ENDMETHOD.


  METHOD _add_element_node.

    DATA temp TYPE string.

    temp = element_node.
    CONDENSE temp.

    " Add element node
    IF temp IS NOT INITIAL.
      INSERT element_node INTO TABLE element_nodes.
    ENDIF.
    CLEAR element_node.

  ENDMETHOD.


  METHOD _extract_element_node.

    DATA(len) = strlen( element_node ).


    DATA(count) = 0.
    DATA(level) = 0.
    DATA(component_count) = 1.
    DATA component TYPE string.

    DO len TIMES.
      DATA(char) = element_node+count(1).

      IF level EQ 1 AND ( char EQ '(' OR char EQ ')' ).
        IF component_count EQ 1.
          elementname = component.
          CONDENSE elementname.
        ELSE.
          INSERT component INTO TABLE serial_attribute_nodes.
        ENDIF.
        CLEAR component.

        ADD 1 TO component_count.

      ENDIF.
      CASE char.
        WHEN '('.
          ADD 1 TO level.
          IF component_count EQ 1.
            DATA(just_level1) = abap_true.
          ENDIF.
        WHEN ')'.
          SUBTRACT 1 FROM level.
          ADD 1 TO component_count.
          IF level EQ 1.
          ENDIF.
      ENDCASE.
      IF level >= 1 AND just_level1 EQ abap_false.
        IF level EQ 1 AND char EQ | |.
          "Ignore
        ELSE.
          component = component && char.
        ENDIF.
      ENDIF.
      ADD 1 TO count.
      just_level1 = abap_false.
    ENDDO.

  ENDMETHOD.


  METHOD _ext_serial_attribute_nodes.

    DATA(len) = strlen( serial_attribute_node ).

    DATA(count) = 0.
    DATA(level) = 0.
    DATA(component_count) = 1.
    DATA component TYPE string.

    DO len TIMES.
      DATA(char) = serial_attribute_node+count(1).

      IF level EQ 1 AND ( char EQ '(' OR char EQ ')' OR char EQ | | ).
        IF component IS NOT INITIAL.
          IF component_count EQ 1.
            CONDENSE component.
            IF component EQ |id:|.
              is_serial = abap_true.
            ELSE.
              attributename = component.
            ENDIF.
          ELSE.
            IF is_serial EQ abap_false.
              INSERT component INTO TABLE valuenodes.
            ELSE.
              serial_id = component.
            ENDIF.
          ENDIF.
          CLEAR component.

          ADD 1 TO component_count.
        ENDIF.

      ENDIF.
      CASE char.
        WHEN '('.
          ADD 1 TO level.
          IF component_count EQ 1.
            DATA(just_level1) = abap_true.
          ENDIF.
        WHEN ')'.
          SUBTRACT 1 FROM level.
          ADD 1 TO component_count.
          IF level EQ 1.
          ENDIF.
      ENDCASE.
      IF level >= 1 AND just_level1 EQ abap_false.
        IF level EQ 1 AND char EQ | |.
          "Ignore
        ELSE.
          component = component && char.
        ENDIF.
      ENDIF.
      ADD 1 TO count.
      just_level1 = abap_false.
    ENDDO.

  ENDMETHOD.


  METHOD _ext_valuenodes.

    DATA(len) = strlen( valuenode  ).
    DATA is_ref TYPE abap_bool.

    DATA(count) = 0.
    DATA(level) = 0.
    DATA(component_count) = 1.
    DATA component TYPE string.

    DO len TIMES.
      DATA(char) = valuenode+count(1).
      IF char EQ '('.
        DATA(not_primitive) = abap_true.
      ENDIF.
      IF level EQ 1 AND ( char EQ '(' OR char EQ ')' OR char EQ | | ).
        IF component IS NOT INITIAL.
          IF component_count EQ 1.
            CONDENSE component.
            IF component EQ |ref:|.
              is_ref = abap_true.
            ELSE.
              is_elementnode = abap_true.
              RETURN.
            ENDIF.
          ELSEIF component_count EQ 2.

            IF component CO '+-0123456789eE'.
              is_integer_reference = abap_true.
              integer_reference = component.
              RETURN.
            ELSE.
              is_name_reference = abap_true.
              ref_elementname = component.
              RETURN.
            ENDIF.
*            IF is_serial EQ abap_false.
*              INSERT component INTO TABLE valuenodes.
*            ELSE.
*              serial_id = component.
*            ENDIF.
          ELSE.
            "! TBD Add error handling
          ENDIF.
          CLEAR component.

          ADD 1 TO component_count.
        ENDIF.

      ENDIF.
      CASE char.
        WHEN '('.
          ADD 1 TO level.
          IF component_count EQ 1.
            DATA(just_level1) = abap_true.
          ENDIF.
        WHEN ')'.
          SUBTRACT 1 FROM level.
          ADD 1 TO component_count.
          IF level EQ 1.
          ENDIF.
      ENDCASE.
      IF level EQ 1 AND just_level1 EQ abap_false.
        IF level EQ 1 AND char EQ | |.
          "Ignore
        ELSEIF level = 0 OR ( ( level EQ 1 AND char <> | | ) OR level > 1 ).
          component = component && char.
        ENDIF.
      ENDIF.
      ADD 1 TO count.
      just_level1 = abap_false.
    ENDDO.
    IF not_primitive EQ abap_false.
      is_primitive = abap_true.
      primitive = valuenode.
    ELSE.
      is_elementnode = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _make_list_of_element_nodes.

    DATA element_node TYPE string.

    " Make list of element_nodes


    DATA(len) = strlen( i_msestr ).

    DATA(count) = 0.
    DATA(level) = 0.
    DO len TIMES.

      DATA(char) = i_msestr+count(1).
      CASE char.
        WHEN '('.
          ADD 1 TO level.
          IF level EQ 2.
            _add_element_node( CHANGING element_node  = element_node
                                        element_nodes = r_element_nodes ).
          ENDIF.

        WHEN ')'.
          SUBTRACT 1 FROM level.
          IF level EQ 0.

*            element_node = element_node && char.
*            _add_element_node( CHANGING element_node  = element_node
*                                        element_nodes = element_nodes ).

          ENDIF.

      ENDCASE.


      IF level EQ 1 AND r_element_nodes IS INITIAL AND char EQ '('.
        " Ignore starting (
      ELSEIF level EQ 0 AND char EQ ')'.
        " Ignore ending )
      ELSEIF level EQ 1 AND char EQ | |.
        " Ignore blanks on level 1
      ELSE.
        element_node = element_node && char.
      ENDIF.


      ADD 1 TO count.

    ENDDO.

    _add_element_node( CHANGING element_node  = element_node
                                element_nodes = r_element_nodes ).

  ENDMETHOD.


  METHOD _remove_apostroph.

    REPLACE ALL OCCURRENCES OF |'| IN string WITH ||.

  ENDMETHOD.
ENDCLASS.
