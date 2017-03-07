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

"! This is an experimental prototype, that has errors
"!
"! The latest version are available on https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor
"!
"! The program follows the naming conventions proposed in the ABAP Programming Guidelines from 2009.
"!
"! Semantics of variables and parameters have to be very strict
"! The code shall be able to be read near to as fluent as a well written English text
"! Use ABAP Doc comments if the technical name is not precise enough
"! Tables are in most cases specified with plural (classes). But not always, mse_model is a table.
"!
"! Prefixes are omitted if the reading is simplified
"!
"! Classes are prefixed with cl_ the instances have no prefixes
"! Global attributes are normally prefixed with g_
"! Instances are normally instanciated only once, to simplify coding no singleton pattern is used
"!
"! Object shall be used only for real classes
"! Component shall be used for programming entities like function, class, method, program, database table, attribute, ...
"!
"! Short abbreviations are used if only locally used, in that case an ABAP Doc comments explains the variable
"! See the start of the report for this
"!
"! This is the original version since 23 March 2016 maintained in ABAP 7.31
"!
"! Thanks to Enno Wulff for providing the initial ABAP 7.31 version
"!
"! Last activation:
"! Generated 07.03.2017
"! Contains commit a82e81c9285a353cf0c2a8661808e99ad08ca653
"!
"! This is version 0.2.0. It will be much better covered with unit tests and end-to-end tests than the first version. It is currently incomplete.
"! Includes fix for #44 ignore SAP interfaces in Where-Used
"!
REPORT z2mse_moose_extractor2.
TABLES tadir. "So that select-options work

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE TEXT-001.


SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE TEXT-002.

*PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_wdyn AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_intf AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_tables AS CHECKBOX DEFAULT 'X'. "Analyze database tables
*PARAMETERS: p_prog AS CHECKBOX DEFAULT 'X'.
*PARAMETERS: p_iprog AS CHECKBOX DEFAULT ' '. "Internal parts of reports
*
*
*PARAMETERS: rb_fpack RADIOBUTTON GROUP rbsl DEFAULT 'X'.
*"! Filter using package
*DATA g_filter_using_package TYPE abap_bool.
*g_filter_using_package = rb_fpack.
*
*PARAMETERS: p_pack TYPE parentcl DEFAULT ''.
*"! Package to be analyzed
*DATA g_parameter_package_to_analyze TYPE parentcl.
*g_parameter_package_to_analyze = p_pack.
*
*PARAMETERS: rb_fname RADIOBUTTON GROUP rbsl.
*"! Filter using name
*DATA g_filter_using_name TYPE abap_bool.
*g_filter_using_name = rb_fname.

SELECT-OPTIONS s_pack FOR tadir-devclass.
SELECT-OPTIONS s_spack FOR tadir-devclass.
PARAMETERS p_sub AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_up TYPE i DEFAULT -1.
"Exclude interfaces in sap name space when found via where used analysis
PARAMETERS p_ex AS CHECKBOX DEFAULT 'X'.

*SELECT-OPTIONS s_compsn FOR tadir-obj_name.

SELECTION-SCREEN END OF BLOCK block_selct_sap_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_using_comp WITH FRAME TITLE TEXT-003.

*PARAMETERS: p_dm AS CHECKBOX DEFAULT ' '.
*"! Usages outside package grouped
*"! If false, a recursive search for using components is performed until no further using components are found
*DATA g_param_usage_outpack_groupd TYPE abap_bool.
*g_param_usage_outpack_groupd = p_dm.

SELECTION-SCREEN END OF BLOCK block_using_comp.

SELECTION-SCREEN BEGIN OF BLOCK block_infos WITH FRAME TITLE TEXT-004.

*PARAMETERS: p_list AS CHECKBOX DEFAULT ' '.
*"! List Tokens of selected programs
*DATA g_parameter_list_tokens TYPE abap_bool.
*g_parameter_list_tokens = p_list.

SELECTION-SCREEN END OF BLOCK block_infos.

" include z_mse.
******************************************** Begin Include Z_MSE_ABAP *****************************
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

SELECTION-SCREEN BEGIN OF BLOCK bl_model_settings WITH FRAME TITLE TEXT-100.

PARAMETERS p_down AS CHECKBOX DEFAULT 'X'.
*"! Download model to file
*DATA g_parameter_download_file TYPE abap_bool.
*g_parameter_download_file = p_down.
SELECTION-SCREEN END OF BLOCK bl_model_settings.


* REPLACE Z2MSE_model

* REPLACE Z2MSE_OUTPUT_MODEL

******************************************** End Include Z_MSE_ABAP *******************************

" include z_famix.
******************************************** Begin Include Z_FAMIX_ABAP ***************************
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

* REPLACE Z2MSE_FAMIX_ENTITY

* REPLACE Z2MSE_FAMIX_SOURCED_ENTITY

* REPLACE Z2MSE_FAMIX_NAMED_ENTITY

* REPLACE Z2MSE_FAMIX_ATTRIBUTE

* REPLACE Z2MSE_FAMIX_CONTAINER_ENTITY

* REPLACE Z2MSE_FAMIX_BEHAVIOURAL_ENTTY

* REPLACE Z2MSE_FAMIX_PACKAGE

* REPLACE Z2MSE_FAMIX_METHOD

* REPLACE Z2MSE_FAMIX_CLASS

* REPLACE Z2MSE_FAMIX_ASSOCIATION

* REPLACE Z2MSE_FAMIX_ACCESS

* REPLACE Z2MSE_FAMIX_INVOCATION

* REPLACE Z2MSE_FAMIX_INHERITANCE

* REPLACE Z2MSE_FAMIX_CUSTOM_SOURCE_LNG

* REPLACE Z2MSE_CHECK_FAMIX_MODEL

" Obsolete:

* REPLACE Z2MSE_FAMIX_MODULE

******************************************** End Include Z_FAMIX_ABAP *****************************

" include z_sap_2_famix
******************************************** Begin Include Z_SAP_2_FAMIX ****************************

* REPLACE Z2MSE_SAP

* REPLACE Z2MSE_SAP_PACKAGE

* REPLACE Z2MSE_SAP_CLASS

* REPLACE Z2MSE_SAP_DB_TABLE

* REPLACE Z2MSE_SAP_ATTRIBUTE

* REPLACE Z2MSE_SAP_METHOD

* REPLACE Z2MSE_SAP_INHERITANCE

* REPLACE Z2MSE_SAP_INVOCATION

* REPLACE Z2MSE_SAP_ACCESS

* REPLACE Z2MSE_SAP_PROGRAM

******************************************** End Include Z_SAP_2_FAMIX ******************************

* REPLACE_DEFINITION Z2MSE_EXTR_PACKAGES

* REPLACE_DEFINITION Z2MSE_EXTR_CLASSES

* REPLACE_DEFINITION Z2MSE_EXTR_TABLES

* REPLACE_DEFINITION Z2MSE_EXTR_WEB_DYNPRO

* REPLACE_DEFINITION Z2MSE_EXTR_WHERE_USED

* REPLACE_DEFINITION Z2MSE_EXTR_WHERE_USED_CLASSES

* REPLACE_DEFINITION Z2MSE_EXTR_WHERE_USED_TABLES

* REPLACE_DEFINITION Z2MSE_EXTRACT_SAP2

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_PACKAGES

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_CLASSES

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_TABLES

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_WEB_DYNPRO

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_WHERE_USED

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_WHERE_USED_CLASSES

* REPLACE_IMPLEMENTATION Z2MSE_EXTR_WHERE_USED_TABLES

* REPLACE_IMPLEMENTATION Z2MSE_EXTRACT_SAP2

START-OF-SELECTION.

  DATA: mse_model TYPE z2mse_model=>lines_type.

  DATA sap_extractor TYPE REF TO z2mse_extract_sap2.

  DATA: ls_pack_line LIKE LINE OF s_pack.
  DATA: ls_pack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_pack INTO ls_pack_line.
    APPEND ls_pack_line TO ls_pack.
  ENDLOOP.

  DATA: ls_spack_line LIKE LINE OF s_pack.
  DATA: ls_spack TYPE sap_extractor->ty_s_pack.
  LOOP AT s_spack INTO ls_spack_line.
    APPEND ls_spack_line TO ls_spack.
  ENDLOOP.

  CREATE OBJECT sap_extractor.

  DATA nothing_done TYPE boolean.
  sap_extractor->extract( EXPORTING i_top_packages           = ls_pack
                                    i_sub_packages_filter    = ls_spack
                                    i_search_sub_packages    = p_sub
                                    i_search_up              = p_up
                                    i_exclude_found_sap_intf = p_ex
                          IMPORTING mse_model             = mse_model
                                    nothing_done          = nothing_done ).

  IF nothing_done EQ abap_true.
    RETURN.
  ENDIF.

  DATA model_outputer TYPE REF TO z2mse_output_model.
  CREATE OBJECT model_outputer.
  model_outputer->make( mse_model = mse_model g_parameter_download_file = p_down ).
