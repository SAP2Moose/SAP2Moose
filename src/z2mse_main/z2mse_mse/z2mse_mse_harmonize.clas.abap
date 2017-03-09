"! Used in Unit Tests to simplify checking for correct Moose models
"! Will not be delivered as local class, thus use new ABAP statements
CLASS z2mse_mse_harmonize DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES mse            TYPE TABLE OF string WITH DEFAULT KEY.
    TYPES harmonized_mse TYPE TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS:
      mse_2_harmonized
        IMPORTING mse                             TYPE mse
        RETURNING VALUE(equalized_harmonized_mse) TYPE harmonized_mse,
      equalize_harmonized
        IMPORTING harmonized_mse                  TYPE harmonized_mse
        RETURNING VALUE(equalized_harmonized_mse) TYPE harmonized_mse.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES string_table TYPE TABLE OF string WITH DEFAULT KEY.
    CLASS-METHODS _add_element_node
      CHANGING
        element_node  TYPE string
        element_nodes TYPE z2mse_mse_harmonize=>string_table.
    CLASS-METHODS _make_list_of_element_nodes
      IMPORTING
        i_msestr               TYPE string
      RETURNING
        VALUE(r_element_nodes) TYPE z2mse_mse_harmonize=>string_table.
ENDCLASS.



CLASS z2mse_mse_harmonize IMPLEMENTATION.

  METHOD mse_2_harmonized.


    DATA msestr TYPE string.
    " Make flat string where linebreaks are replaced by blanks
    LOOP AT mse INTO DATA(line).
      msestr = msestr && ' ' && line.
    ENDLOOP.
    DATA content TYPE string.
    " Counts element nodes
    DATA element_node TYPE string.
    DATA element_nodes TYPE string_table.

    element_nodes = _make_list_of_element_nodes( msestr ).

    LOOP AT element_nodes INTO element_node.
    ENDLOOP.

  ENDMETHOD.



  METHOD equalize_harmonized.

    equalized_harmonized_mse = harmonized_mse.
    SORT equalized_harmonized_mse.
    LOOP AT equalized_harmonized_mse ASSIGNING FIELD-SYMBOL(<eq>).
      CONDENSE <eq>.
    ENDLOOP.

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
      elseif level eq 1 and char eq | |.
      " Ignore blanks on level 1
      ELSE.
        element_node = element_node && char.
      ENDIF.


      ADD 1 TO count.

    ENDDO.

    _add_element_node( CHANGING element_node  = element_node
                                element_nodes = r_element_nodes ).

  ENDMETHOD.

ENDCLASS.
