CLASS z2mse_output_model DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS make
      IMPORTING
        mse_model                 TYPE z2mse_model=>lines_type
        g_parameter_download_file TYPE abap_bool
        i_default_prefix          TYPE string.
ENDCLASS.

CLASS z2mse_output_model IMPLEMENTATION.

  METHOD make.
    " Download the file

    DATA: filename          TYPE string,
          default_file_name TYPE string,
          pathname          TYPE string,
          fullpath          TYPE string,
          user_action       TYPE i.

    default_file_name = |{ i_default_prefix }_{ sy-sysid }_{ sy-datum }_{ sy-uzeit }|.

    IF g_parameter_download_file EQ abap_true.

      cl_gui_frontend_services=>file_save_dialog( EXPORTING default_extension = 'mse'
                                                            default_file_name = default_file_name
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
            data_tab = mse_model.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


