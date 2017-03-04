*&---------------------------------------------------------------------*
*& Report z2mse_test_model_downloader
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z2mse_test_model_downloader.

PARAMETERS: p_down  AS CHECKBOX DEFAULT 'X',
            p_testc TYPE z2mse_chartest-testcase.

DATA: mse_model      TYPE z2mse_model=>lines_type,
      test_mse_model TYPE STANDARD TABLE OF z2mse_chartest WITH DEFAULT KEY.

SELECT * FROM z2mse_chartest INTO TABLE test_mse_model WHERE testcase = p_testc ORDER BY line.

LOOP AT test_mse_model INTO DATA(line).

  mse_model = VALUE #( BASE mse_model ( line = line-content ) ).

ENDLOOP.
DATA model_outputer TYPE REF TO z2mse_output_model.
CREATE OBJECT model_outputer.
model_outputer->make( mse_model = mse_model g_parameter_download_file = p_down ).
