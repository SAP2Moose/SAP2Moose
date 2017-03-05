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

" Generated 21.11.2016 17:49

REPORT z2mse_translate_to_local.

" The template
" It is a working program that uses the global classes.
" It has comments that mark where to insert the global classes as local.
" Example:
* REPLACE NameOfGlobalClass
" To replace only the definition or implementation use:
* REPLACE_DEFINITION NameOfGlobalClass
* REPLACE_IMPLEMENTATION NameOfGlobalClass

PARAMETERS: p_templ TYPE c LENGTH 30 DEFAULT 'Z2MSE_MOOSE_EXTRACTOR2'.

" Only to have a "more beautiful" program after conversion.
" If not empty replace all occurences of prefix with cl_ in the target program
PARAMETERS: p_prefix TYPE c LENGTH 30 DEFAULT 'Z2MSE_'.

TYPES: stringtable TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

CLASS cl_read_template DEFINITION.
  PUBLIC SECTION.
    METHODS do
      RETURNING VALUE(source) TYPE stringtable.
ENDCLASS.

CLASS cl_read_template IMPLEMENTATION.

  METHOD do.
    READ REPORT p_templ INTO source.
  ENDMETHOD.

ENDCLASS.

CLASS cl_read_class DEFINITION.
  PUBLIC SECTION.
    METHODS do
      IMPORTING clsname             TYPE seoclsname
                read_definition     TYPE abap_bool
                read_implementation TYPE abap_bool
      RETURNING VALUE(r_source)     TYPE stringtable.
ENDCLASS.

CLASS cl_read_class IMPLEMENTATION.

  METHOD do.

    " Copied from report SEO_CLASS_OUTPUT

    DATA:
      clstype     TYPE seoclstype,
      source      TYPE seop_source_string,
      pool_source TYPE seop_source_string,
      source_line TYPE LINE OF seop_source_string,
      tabix       TYPE sytabix,
      includes    TYPE seop_methods_w_include,
      include     TYPE seop_method_w_include,
      cifkey      TYPE seoclskey,
      cifref      TYPE REF TO if_oo_clif_incl_naming,
      clsref      TYPE REF TO if_oo_class_incl_naming,
      intref      TYPE REF TO if_oo_interface_incl_naming.

    DATA: l_string TYPE string.

    cifkey-clsname = clsname.

    CALL METHOD cl_oo_include_naming=>get_instance_by_cifkey
      EXPORTING
        cifkey = cifkey
      RECEIVING
        cifref = cifref
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      ASSERT 1 = 2.
    ENDIF.

    CASE cifref->clstype.
      WHEN seoc_clstype_class.
        clsref ?= cifref.
        READ REPORT clsref->class_pool
          INTO pool_source.
        IF read_definition EQ abap_true.
          LOOP AT pool_source INTO source_line.
            IF source_line CS 'CLASS-POOL'
              OR source_line CS 'class-pool'.
              " r_source = VALUE #( BASE r_source ( source_line ) ).
              tabix = sy-tabix.
              EXIT.
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->locals_old
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*' AND source_line IS NOT INITIAL.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->locals_def
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*' AND source_line IS NOT INITIAL.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->locals_imp
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*' AND source_line IS NOT INITIAL.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->macros
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*' AND source_line IS NOT INITIAL.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->public_section
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*'.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->protected_section
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*'.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          READ REPORT clsref->private_section
            INTO source.
          LOOP AT source
            INTO source_line.
            IF source_line NS '*"*'.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDIF.
          ENDLOOP.
          r_source = VALUE #( BASE r_source ( CONV #( 'ENDCLASS.' ) ) ).
        ENDIF.
        IF read_implementation EQ abap_true.
          CONCATENATE 'CLASS' cifkey 'IMPLEMENTATION.' INTO l_string SEPARATED BY space.
          r_source = VALUE #( BASE r_source ( CONV #( l_string ) ) ).
* method implementation
          includes = clsref->get_all_method_includes( ).
          LOOP AT includes
            INTO include.
            READ REPORT include-incname
              INTO source.
            LOOP AT source
              INTO source_line.
              r_source = VALUE #( BASE r_source ( source_line ) ).
            ENDLOOP.
          ENDLOOP.
          r_source = VALUE #( BASE r_source ( CONV #( 'ENDCLASS.' ) ) ).
        ENDIF.

      WHEN seoc_clstype_interface.
        intref ?= cifref.
        IF read_definition EQ abap_true.
          READ REPORT intref->interface_pool
            INTO source.
          LOOP AT source INTO source_line.
            r_source = VALUE #( BASE r_source ( source_line ) ).
          ENDLOOP.
          READ REPORT intref->public_section
            INTO source.
          LOOP AT source INTO source_line.
            r_source = VALUE #( BASE r_source ( source_line ) ).
          ENDLOOP.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS cl_global_to_local DEFINITION.
  PUBLIC SECTION.
    METHODS do
      IMPORTING template_source TYPE stringtable
      RETURNING VALUE(r_source) TYPE stringtable.
ENDCLASS.

CLASS cl_global_to_local IMPLEMENTATION.

  METHOD do.

    DATA(read_class) = NEW cl_read_class( ).

    LOOP AT template_source INTO DATA(template_line).

      SPLIT template_line AT ' ' INTO DATA(part_1) DATA(part_2) DATA(part_3) DATA(part_4).
      TRANSLATE part_2 TO UPPER CASE.
      IF part_1 EQ '*' AND ( part_2 EQ 'REPLACE' ) OR ( part_2 EQ 'REPLACE_DEFINITION' ) OR ( part_2 EQ 'REPLACE_IMPLEMENTATION' ) .

        ASSERT part_4 IS INITIAL.

        IF part_2 EQ 'REPLACE'.
          DATA(read_definition) = abap_true.
          DATA(read_implementation) = abap_true.
        ENDIF.

        IF part_2 EQ 'REPLACE_DEFINITION'.
          read_definition = abap_true.
          read_implementation = abap_false.
        ENDIF.

        IF part_2 EQ 'REPLACE_IMPLEMENTATION'.
          read_definition = abap_false.
          read_implementation = abap_true.
        ENDIF.

        TRANSLATE part_3 TO UPPER CASE.

        DATA(class_source) = read_class->do( clsname = CONV #( part_3 )
                                             read_definition = read_definition
                                             read_implementation = read_implementation ).

        LOOP AT class_source INTO DATA(class_line).

          r_source = VALUE #( BASE r_source ( class_line ) ).

        ENDLOOP.

      ELSE.
        r_source = VALUE #( BASE r_source ( template_line ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS cl_replace_prefix DEFINITION.
  PUBLIC SECTION.
    METHODS do
      CHANGING source TYPE stringtable.

ENDCLASS.

CLASS cl_replace_prefix IMPLEMENTATION.

  METHOD do.

    LOOP AT source ASSIGNING FIELD-SYMBOL(<line>).

      SPLIT <line> AT ' ' INTO DATA(part_1) DATA(part_2).

      TRANSLATE part_1 TO UPPER CASE.
      IF part_1 EQ 'REPORT'.
        CONTINUE.
      ENDIF.

      DATA prefix_lower_case TYPE c LENGTH 30.
      DATA prefix_upper_case TYPE c LENGTH 30.

      prefix_lower_case = p_prefix.
      prefix_upper_case = p_prefix.

      TRANSLATE prefix_lower_case TO LOWER CASE.
      TRANSLATE prefix_upper_case TO UPPER CASE.
      REPLACE ALL OCCURRENCES OF prefix_lower_case IN <line> WITH 'cl_'.
      IF sy-subrc EQ 0.
        WRITE: / <line>.
      ENDIF.
      REPLACE ALL OCCURRENCES OF prefix_upper_case IN <line> WITH 'CL_'.
      IF sy-subrc EQ 0.
        WRITE: / <line>.
      ENDIF.

      REPLACE ALL OCCURRENCES OF p_prefix IN <line> WITH 'cl_' IGNORING CASE.
      IF sy-subrc EQ 0.
        WRITE: / <line>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

"! Remove statements that cannot be used in a local class
CLASS cl_remove_global_only DEFINITION.
  PUBLIC SECTION.
    METHODS do
      CHANGING source TYPE stringtable.

ENDCLASS.

CLASS cl_remove_global_only IMPLEMENTATION.

  METHOD do.

    DATA: left_justified_line TYPE string.

    LOOP AT source ASSIGNING FIELD-SYMBOL(<line>).
      left_justified_line = <line>.
      CONDENSE left_justified_line.
      SPLIT left_justified_line AT ' ' INTO DATA(part_1) DATA(part_2) DATA(part_3) DATA(part_4).

      " Begin of CLASS DEFINITION found.
      TRANSLATE part_1 TO UPPER CASE.
      TRANSLATE part_2 TO UPPER CASE.
      TRANSLATE part_3 TO UPPER CASE.
      IF part_1 EQ 'CLASS' AND part_3 EQ 'DEFINITION'.
        DATA(class_definition_started) = abap_true.
      ENDIF.


      IF part_1 EQ 'PUBLIC' AND part_2 EQ 'SECTION'.
        class_definition_started = abap_false.
      ENDIF.

      IF class_definition_started EQ abap_true.

        IF part_1 EQ 'PUBLIC' AND part_2 IS INITIAL.
          DELETE source.
          CONTINUE.
        ELSEIF part_1 EQ 'PUBLIC' AND part_2 EQ 'ABSTRACT'.
          DELETE source.
          CONTINUE.
        ELSEIF ( part_1 EQ 'CREATE' AND part_2 EQ 'PUBLIC.' AND part_3 IS INITIAL ) OR
               ( part_1 EQ 'CREATE' AND part_2 EQ 'PUBLIC' AND part_3 EQ '.' AND part_4 IS INITIAL ).
          <line> = '.'.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

"! Remove statement TEST-SEAM. It is not needed in local classes and would lead to syntax errors in older releases
CLASS cl_remove_test_seam DEFINITION.
  PUBLIC SECTION.
    METHODS do
      CHANGING source TYPE stringtable.

ENDCLASS.

CLASS cl_remove_test_seam IMPLEMENTATION.

  METHOD do.

    DATA: left_justified_line TYPE string.

    LOOP AT source ASSIGNING FIELD-SYMBOL(<line>).
      left_justified_line = <line>.
      CONDENSE left_justified_line.
      SPLIT left_justified_line AT ' ' INTO DATA(part_1) DATA(part_2).

      " Begin of CLASS DEFINITION found.
      TRANSLATE part_1 TO UPPER CASE.

      IF part_1 EQ 'TEST-SEAM'.
        DELETE source.
        CONTINUE.
      ENDIF.

      FIND 'END-TEST-SEAM' IN part_1.
      IF sy-subrc EQ 0.
        DELETE source.
        CONTINUE.
      ENDIF.

    ENDLOOP.

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

START-OF-SELECTION.

  DATA(read_template) = NEW cl_read_template( ).
  DATA(template_source) = read_template->do( ).

  DATA(global_to_local) = NEW cl_global_to_local( ).
  DATA(target_source) = global_to_local->do( template_source = template_source ).

  DATA(remove_global_only) = NEW cl_remove_global_only( ).
  remove_global_only->do( CHANGING source = target_source ).

  DATA(remove_test_seam) = NEW cl_remove_test_seam( ).
  remove_test_seam->do( CHANGING source = target_source ).

  DATA(replace_prefix) = NEW cl_replace_prefix( ).
  replace_prefix->do( CHANGING source = target_source ).

  DATA(download) = NEW cl_download( ).
  download->do( source = target_source ).
