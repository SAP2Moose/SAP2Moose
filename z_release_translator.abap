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

"! Last activation:
"! 17.03.2016 17:39 issue17 Rainer Winkler
"!
"! Keep logic compatible to ABAP 7.31 to allow also conversion into the other direction
REPORT z_release_translator.

"! Redefines abap_bool to simplify coding (Not always reading abap_...)
TYPES bool TYPE abap_bool.
CONSTANTS:
  "! Redefines abap_true to simplify coding (Not always reading abap_...)
  true  TYPE bool VALUE abap_true,
  "! Redefines abap_false to simplify coding (Not always reading abap_...)
  false TYPE bool VALUE abap_false.

TYPES: stringtable TYPE STANDARD TABLE OF string WITH DEFAULT KEY.



CLASS cl_read DEFINITION.
  PUBLIC SECTION.
    METHODS do
      RETURNING VALUE(source) TYPE stringtable.
ENDCLASS.

CLASS cl_read IMPLEMENTATION.

  METHOD do.
    READ REPORT 'YRW1_MOOSE_EXTRACTOR' INTO source.
  ENDMETHOD.

ENDCLASS.

CLASS cl_download DEFINITION.
  PUBLIC SECTION.
    METHODS do
      IMPORTING source TYPE stringtable.
ENDCLASS.

CLASS cl_download IMPLEMENTATION.

  METHOD do.
    " Download the file

    DATA: filename    TYPE string,
          pathname    TYPE string,
          fullpath    TYPE string,
          user_action TYPE i.



    cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension = 'abap'
                                                CHANGING  filename    = filename       " File Name to Save
                                                          path        = pathname       " Path to File
                                                          fullpath    = fullpath       " Path + File Name
                                                          user_action = user_action ). " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)

    IF user_action = cl_gui_frontend_services=>action_cancel.
      WRITE: / 'Canceled by user'.
    ELSE.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename = fullpath
        TABLES
          data_tab = source.

    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS cl_conversion DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF codeline_type,
             code      TYPE string,
             condensed TYPE string,
           END OF codeline_type.
    TYPES: codelines_type TYPE STANDARD TABLE OF codeline_type WITH KEY code.
    TYPES: BEGIN OF replace_type,
             replace_id TYPE i,
             abap_740   TYPE codelines_type,
             abap_731   TYPE codelines_type,
           END OF replace_type.
    TYPES: replaces_type TYPE STANDARD TABLE OF replace_type WITH KEY replace_id.
    DATA: g_replaces TYPE replaces_type.
    METHODS constructor.
    METHODS get_conversion
      RETURNING VALUE(replaces) TYPE replaces_type.
ENDCLASS.

CLASS cl_conversion IMPLEMENTATION.

  METHOD constructor.

    DATA: codeline           TYPE codeline_type,
          codelines_abap_740 TYPE codelines_type,
          codelines_abap_731 TYPE codelines_type,
          replace            TYPE replace_type,
          replace_id         TYPE i.

    replace_id = 1.

    DEFINE start_building_table.
      CLEAR codelines_abap_740.
      CLEAR codelines_abap_731.
    END-OF-DEFINITION.

    DEFINE add_abap_740.
      codeline-code = &1.
      codeline-condensed = &1.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed to UPPER CASE.
      APPEND codeline TO codelines_abap_740.
    END-OF-DEFINITION.

    DEFINE add_abap_731.
      codeline-code = &1.
      codeline-condensed = &1.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed to UPPER CASE.
      APPEND codeline TO codelines_abap_731.
    END-OF-DEFINITION.

    DEFINE add_replace.
      CLEAR replace.
      replace-replace_id = replace_id.
      replace-abap_740 = codelines_abap_740.
      replace-abap_731 = codelines_abap_731.

      g_replaces = value #( base g_replaces ( replace ) ).

      ADD 1 TO replace_id.
    END-OF-DEFINITION.

    start_building_table.
    add_abap_740 '      EXPORTING VALUE(exists_already_with_id) TYPE i'.
    add_abap_740 '      RETURNING VALUE(processed_id)           TYPE i.'.

    add_abap_731 '      EXPORTING value(exists_already_with_id) TYPE i'.
    add_abap_731 '                value(processed_id)           TYPE i.'.
    add_replace.

  ENDMETHOD.

  METHOD get_conversion.

    replaces = g_replaces.

  ENDMETHOD.

ENDCLASS.

CLASS cl_convert DEFINITION.
  PUBLIC SECTION.
    DATA: g_conversion TYPE REF TO cl_conversion.
    METHODS constructor.
    METHODS do
      CHANGING source TYPE stringtable.
  PRIVATE SECTION.

    METHODS _read_first_line_to_compare
      IMPORTING
        replace           TYPE cl_conversion=>replace_type
      EXPORTING
        code_index        TYPE i
        abap_740_codeline TYPE cl_conversion=>codeline_type.
ENDCLASS.

CLASS cl_convert IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT g_conversion.

  ENDMETHOD.

  METHOD do.

    DATA: replaces    TYPE g_conversion->replaces_type,
          replace     TYPE g_conversion->replace_type,
          codeline    TYPE g_conversion->codeline_type,
          "! All code lines to be replaced
          codelines   TYPE g_conversion->codelines_type,
          "! The code lines after processing a scan for a single replacement
          codelines_2 TYPE g_conversion->codelines_type,
          "! A temporary list for code lines where at least part of the lines fit to the actual searched replace
          codelines_3 TYPE g_conversion->codelines_type,
          line        TYPE string,
          code_index  TYPE i.

    replaces = g_conversion->get_conversion( ).

    LOOP AT source INTO line.

      codeline-code = line.
      codeline-condensed = line.
      CONDENSE codeline-condensed.
      TRANSLATE codeline-condensed TO UPPER CASE.
      APPEND codeline TO codelines.

    ENDLOOP.

    " The replace is now done the following way.

    " general, compared are always the condensed lines, replaced is the not condensed line.

    " For each entry in table replaces:

    " Loop over code lines and transfer line by line into codelines_2
    "   but only if a line is not equal to the first line in replace-abap_740

    "   if it is equal it will be transfered to codelines_3
    "   in that case all lines of replace-abap_740 have to be found
    "   if not all lines are equal to replace-abap_740, codelines_3 is appended to codelines_2 and scan is proceeded normally
    "   if all lines are equal, replace-abap_730 is appended to codelines_2 and codelines_3 is cleared than scan is proceeded normally


    LOOP AT replaces INTO replace.

      DATA: abap_740_codeline TYPE g_conversion->codeline_type,
            line_is_equal     TYPE bool,
            "! True if comparison for a replacement started. This is the case if the first line was found in the code.
            first_line_equal  TYPE bool.

      _read_first_line_to_compare( EXPORTING replace           = replace
                                   IMPORTING code_index        = code_index
                                             abap_740_codeline = abap_740_codeline ).

      codelines_2 = VALUE #( ).

      LOOP AT codelines INTO codeline.

        IF first_line_equal EQ true.
          ADD 1 TO code_index.
          READ TABLE replace-abap_740 INTO abap_740_codeline INDEX code_index.
          IF sy-subrc EQ 0. " Row is found

          ELSEIF sy-subrc EQ 4. " Row is not found
            codelines_2 = VALUE #( BASE codelines_2 ( LINES OF replace-abap_731 ) ).
            codelines_3 = VALUE #( ).
            first_line_equal = false.

            _read_first_line_to_compare( EXPORTING replace           = replace
                                         IMPORTING code_index        = code_index
                                                   abap_740_codeline = abap_740_codeline ).


          ELSE. " Occurs if comparing statement or binary search is used, not supported here
            ASSERT 1 = 2.
          ENDIF.
        ENDIF.

        IF codeline-condensed EQ abap_740_codeline-condensed.

          IF code_index EQ 1.
            first_line_equal = true.
            codelines_3 = VALUE #( ( codeline ) ).
          ELSE.
            codelines_3 = VALUE #( BASE codelines_3 ( codeline ) ).
          ENDIF.

          line_is_equal = true.
        ELSE.

          IF first_line_equal EQ false.

            codelines_2 = VALUE #( BASE codelines_2 ( codeline ) ).

          ELSE.

            codelines_2 = VALUE #( BASE codelines_2 ( LINES OF codelines_3 ) ).
            codelines_3 = VALUE #( ).
            first_line_equal = false.

            _read_first_line_to_compare( EXPORTING replace           = replace
                                         IMPORTING code_index        = code_index
                                                   abap_740_codeline = abap_740_codeline ).

          ENDIF.

          line_is_equal = false.
        ENDIF.

        " TBD finalize logic

      ENDLOOP.

      codelines_2 = VALUE #( BASE codelines_2 ( LINES OF codelines_3 ) ).

      codelines = codelines_2.

    ENDLOOP.

    source = VALUE #( ).

    LOOP AT codelines INTO codeline.

      APPEND codeline-code TO source.

    ENDLOOP.

  ENDMETHOD.


  METHOD _read_first_line_to_compare.

    code_index = 1.
    READ TABLE replace-abap_740 INTO abap_740_codeline INDEX code_index.
    ASSERT sy-subrc EQ 0. " Row has to be found

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  DATA(read) = NEW cl_read( ).
  DATA(convert) = NEW cl_convert( ).
  DATA(download) = NEW cl_download( ).

  DATA(source) = read->do( ).
  convert->do( CHANGING source = source ).
  download->do( source = source ).