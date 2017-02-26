

CLASS z2mse_check_famix_model DEFINITION
  PUBLIC
  CREATE PUBLIC.
  PUBLIC SECTION.
    "! Checks a model regarding the FAMIX attributes
    METHODS check IMPORTING model TYPE REF TO z2mse_model.
  PRIVATE SECTION.

    METHODS _check_attribute_parentpackage
      IMPORTING
        is_public_elements TYPE z2mse_model=>public_element_type.
    METHODS _check_attribute_name
      IMPORTING
        is_public_elements TYPE z2mse_model=>public_element_type.
ENDCLASS.



CLASS z2mse_check_famix_model IMPLEMENTATION.


  METHOD check.

    DATA public_elements TYPE model->public_elements_type.

    public_elements = model->get_model( ).
    DATA ls_public_elements TYPE model->public_element_type.
    DATA ls_public_attribute TYPE  model->public_attribute_type.
    LOOP AT public_elements INTO ls_public_elements.

      _check_attribute_parentpackage( ls_public_elements ).

      _check_attribute_name( ls_public_elements ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _check_attribute_name.

    DATA ls_public_attribute TYPE z2mse_model=>public_attribute_type.

    " Check attribute parentPackage
    DATA count_name TYPE i.
    CLEAR count_name.
    LOOP AT is_public_elements-public_attributes INTO ls_public_attribute WHERE attribute_type = 'name'.

      ADD 1 TO count_name.

      CONDENSE ls_public_attribute-string.
      TEST-SEAM check_name.
      END-TEST-SEAM.

      IF ls_public_attribute-string IS INITIAL.
        TEST-SEAM name1.
          FORMAT COLOR COL_NEGATIVE.

          " SAP_2_FAMIX_51        Return a message if the attribute name is empty
          WRITE: / 'Element ', is_public_elements-element_id, ' has an attribute name that is empty'.

          FORMAT COLOR COL_BACKGROUND.
        END-TEST-SEAM.
      ENDIF.

    ENDLOOP.

    " SAP_2_FAMIX_50        Return a message if the attribute name occurs more than once

    IF count_name > 1.

      TEST-SEAM name2.
        FORMAT COLOR COL_NEGATIVE.

        WRITE: / 'Element ', is_public_elements-element_id, ' has more than a single attribute name'.

        FORMAT COLOR COL_BACKGROUND.
      END-TEST-SEAM.
    ENDIF.

  ENDMETHOD.


  METHOD _check_attribute_parentpackage.

    DATA ls_public_attribute TYPE z2mse_model=>public_attribute_type.

    " Check attribute parentPackage
    DATA count_parent_packages TYPE i.
    CLEAR count_parent_packages.
    LOOP AT is_public_elements-public_attributes INTO ls_public_attribute WHERE attribute_type = 'parentPackage'.

      ADD 1 TO count_parent_packages.

    ENDLOOP.

    " SAP_2_FAMIX_49        Return a message if the attribute parent package occurs more than once
    TEST-SEAM check_parent_package.
    END-TEST-SEAM.

    IF count_parent_packages > 1.
      TEST-SEAM parent_package.
        FORMAT COLOR COL_NEGATIVE.

        WRITE: / 'Package ', is_public_elements-element_id, ' has more than a single parent package'.

        FORMAT COLOR COL_BACKGROUND.
      END-TEST-SEAM.
    ENDIF.

    " SAP_2_FAMIX_62        Return a message if a class has no parent package assigned
    IF is_public_elements-element_type EQ 'FAMIX.Class'.
      IF count_parent_packages EQ 0.
        TEST-SEAM parent_package2.
          FORMAT COLOR COL_NEGATIVE.

          WRITE: / 'Class ', is_public_elements-element_id, ' has no parent package'.

          FORMAT COLOR COL_BACKGROUND.
        END-TEST-SEAM.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
