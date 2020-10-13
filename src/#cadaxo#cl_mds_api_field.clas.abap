CLASS /cadaxo/cl_mds_api_field DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES /cadaxo/if_mds_api_field.

    CLASS-METHODS get_instance IMPORTING i_field_id        TYPE /cadaxo/mds_field_id
                                         i_data            TYPE /cadaxo/if_mds_api_field~ty_data OPTIONAL
                               RETURNING VALUE(e_instance) TYPE REF TO /cadaxo/if_mds_api_field .
    METHODS constructor IMPORTING i_field_id TYPE /cadaxo/mds_field_id.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_instance,
             field_id TYPE /cadaxo/mds_field_id,
             instance TYPE REF TO /cadaxo/if_mds_api_field,
           END OF ty_instance,
           ty_instances TYPE SORTED TABLE OF ty_instance WITH UNIQUE KEY field_id.
    CLASS-DATA instances TYPE ty_instances.

    DATA: field TYPE /cadaxo/if_mds_api_field~ty_field.

    METHODS set_data IMPORTING i_data TYPE /cadaxo/if_mds_api_field~ty_data.
ENDCLASS.



CLASS /cadaxo/cl_mds_api_field IMPLEMENTATION.

  METHOD get_instance.

    DATA: field_instance TYPE REF TO /cadaxo/if_mds_api_field.

    IF NOT line_exists( instances[ field_id = i_field_id ] ).

      INSERT VALUE #( field_id = i_field_id
                      instance = NEW /cadaxo/cl_mds_api_field( i_field_id ) ) INTO TABLE instances ASSIGNING FIELD-SYMBOL(<instance>).
    ELSE.
      ASSIGN instances[ field_id = i_field_id ] TO <instance>.
    ENDIF.

    IF i_data IS SUPPLIED.
      CAST /cadaxo/cl_mds_api_field( <instance>-instance )->set_data( i_data ).
    ENDIF.

    e_instance = <instance>-instance.

  ENDMETHOD.

  METHOD constructor.

    SELECT SINGLE  ds_id, field_name
           FROM /cadaxo/mds_fd
           WHERE field_id = @i_field_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    field-field_id = i_field_id.
    field-semkey = semkey.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_field~get_as_structure.

    e_field = field.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_field~get_id.

    e_field_id = field-field_id.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_field~get_semantic_key.

    e_semantic_key = field-semkey.

  ENDMETHOD.

  METHOD set_data.

    field-data = i_data.

  ENDMETHOD.

ENDCLASS.
