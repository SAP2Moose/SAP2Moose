*
* This is version 0.4.0 (in development)
*
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

"! The latest version are available on https://github.com/RainerWinkler/Moose-FAMIX-SAP-Extractor
"!
"! Thanks to Enno Wulff for providing the initial ABAP 7.31 version
"!
REPORT z2mse_moose_extractor2.
TABLES tadir. "So that select-options work

SELECTION-SCREEN BEGIN OF BLOCK block_global_source WITH FRAME TITLE TEXT-001.


SELECTION-SCREEN END OF BLOCK block_global_source.

SELECTION-SCREEN BEGIN OF BLOCK block_selct_sap_comp WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS s_pack FOR tadir-devclass.
SELECT-OPTIONS s_spack FOR tadir-devclass.
PARAMETERS p_sub AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_nup TYPE i DEFAULT -1.
parameters p_ndown TYPE i DEFAULT -1.
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

" Obsolete:

* REPLACE Z2MSE_FAMIX_MODULE

******************************************** End Include Z_FAMIX_ABAP *****************************

* REMOVE_COMMENT CLASS Z2MSE_EXTR3_ACCESS_OR_INVOCATN DEFINITION DEFERRED.
* REMOVE_COMMENT CLASS Z2MSE_EXTR3_ASSOCIATION DEFINITION DEFERRED.
* REMOVE_COMMENT CLASS Z2MSE_EXTR3_MODEL_BUILDER DEFINITION DEFERRED.
* REMOVE_COMMENT CLASS Z2MSE_EXTR3_ELEMENTS DEFINITION DEFERRED.

* REPLACE_DEFINITION Z2MSE_EXTR3_ELEMENT_MANAGER
* REPLACE_DEFINITION Z2MSE_EXTR3
* REPLACE_DEFINITION Z2MSE_EXTR3_ASSOCIATION
* REPLACE_DEFINITION Z2MSE_EXTR3_ACCESS_OR_INVOCATN
* REPLACE_DEFINITION Z2MSE_EXTR3_ACCESS
* REPLACE_DEFINITION Z2MSE_EXTR3_ELEMENTS
* REPLACE_DEFINITION Z2MSE_EXTR3_INVOCATION
* REPLACE_DEFINITION Z2MSE_EXTR3_PARENT_PACKAGE
* REPLACE_DEFINITION Z2MSE_EXTR3_ASSOCIATION_BUILD
* REPLACE_DEFINITION Z2MSE_EXTR3_CLASSES
* REPLACE_DEFINITION Z2MSE_EXTR3_PACKAGES
* REPLACE_DEFINITION Z2MSE_EXTR3_PROGRAMS
* REPLACE_DEFINITION Z2MSE_EXTR3_TABLES
* REPLACE_DEFINITION Z2MSE_EXTR3_WEB_DYNPRO_COMP
* REPLACE_DEFINITION Z2MSE_EXTR3_TADIR_BUILDER
* REPLACE_DEFINITION Z2MSE_EXTR3_WHERE_USED_BUILDER
* REPLACE_DEFINITION Z2MSE_EXTR3_INITIAL_ELEMENTS
* REPLACE_DEFINITION Z2MSE_EXTR3_MODEL_BUILDER
* REPLACE_DEFINITION Z2MSE_EXTRACT3


* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ACCESS
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ACCESS_OR_INVOCATN
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ASSOCIATION
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_INVOCATION
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_PARENT_PACKAGE
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ASSOCIATION_BUILD
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_TADIR_BUILDER
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_WHERE_USED_BUILDER
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_CLASSES
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ELEMENTS
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_PACKAGES
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_PROGRAMS
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_TABLES
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_WEB_DYNPRO_COMP
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_ELEMENT_MANAGER
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_INITIAL_ELEMENTS
* REPLACE_IMPLEMENTATION Z2MSE_EXTR3_MODEL_BUILDER
* REPLACE_IMPLEMENTATION Z2MSE_EXTRACT3

START-OF-SELECTION.

  DATA: mse_model TYPE z2mse_model=>lines_type.

  DATA sap_extractor TYPE REF TO z2mse_extract3.

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




  DATA: initial_elements TYPE REF TO z2mse_extr3_initial_elements.
  initial_elements = NEW #( ).
  initial_elements->select_packages( EXPORTING top_packages           = ls_pack
                                               sub_packages_filter    = ls_spack
                                               including_sub_packages = abap_true ).

  CREATE OBJECT sap_extractor.

  DATA nothing_done TYPE boolean.
  sap_extractor->extract( EXPORTING initial_elements         = initial_elements
                                    i_search_up              = p_nup
                                    i_search_down            = p_ndown
                                    i_exclude_found_sap_intf = abap_true
                          IMPORTING mse_model                = mse_model
                                    nothing_done             = nothing_done ).

  IF nothing_done EQ abap_true.
    RETURN.
  ENDIF.

  DATA model_outputer TYPE REF TO z2mse_output_model.
  CREATE OBJECT model_outputer.
  model_outputer->make( mse_model = mse_model g_parameter_download_file = p_down ).
