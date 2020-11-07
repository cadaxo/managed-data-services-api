CLASS /cadaxo/cl_mds_id DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_instance RETURNING VALUE(e_id_handler) TYPE REF TO /cadaxo/cl_mds_id.
    METHODS get_ds_semkey IMPORTING i_ds_id               TYPE /cadaxo/mds_ds_id
                          RETURNING VALUE(r_semantic_key) TYPE /cadaxo/mds_ds_semkey
                          RAISING   /cadaxo/cx_mds_id.
    METHODS get_link_semkey IMPORTING i_link_id             TYPE /cadaxo/mds_link_id
                            RETURNING VALUE(r_semantic_key) TYPE /cadaxo/mds_lk_semkey
                            RAISING   /cadaxo/cx_mds_id.
    METHODS get_parameter_semkey IMPORTING i_parameter_id        TYPE /cadaxo/mds_parameter_id
                                 RETURNING VALUE(r_semantic_key) TYPE /cadaxo/mds_pr_semkey
                                 RAISING   /cadaxo/cx_mds_id.
    METHODS get_property_semkey IMPORTING i_property_id         TYPE /cadaxo/mds_property_id
                                RETURNING VALUE(r_semantic_key) TYPE /cadaxo/mds_py_semkey
                                RAISING   /cadaxo/cx_mds_id.
    CLASS-METHODS class_constructor.

    METHODS build_hash IMPORTING i_semkey           TYPE any
                       RETURNING VALUE(r_object_id) TYPE /cadaxo/mds_object_id.
  PROTECTED SECTION.

    CLASS-DATA instance TYPE REF TO /cadaxo/cl_mds_id.
    CLASS-DATA convert_out TYPE REF TO cl_abap_conv_out_ce.

    DATA datasource_buffers TYPE SORTED TABLE OF /cadaxo/mds_ds WITH UNIQUE KEY ds_id.
    DATA link_buffers TYPE SORTED TABLE OF /cadaxo/mds_lk WITH UNIQUE KEY link_id.
    DATA parameter_buffers TYPE SORTED TABLE OF /cadaxo/mds_pr WITH UNIQUE KEY parameter_id.
    DATA property_buffers TYPE SORTED TABLE OF /cadaxo/mds_py WITH UNIQUE KEY property_id.

    METHODS string_to_xstring IMPORTING i_semkey_string         TYPE string
                              RETURNING VALUE(r_semkey_xstring) TYPE xstring.
    METHODS is_structure_supported IMPORTING i_structure_name   TYPE string
                                   RETURNING VALUE(r_supported) TYPE abap_bool.

    METHODS set_ds_buffer IMPORTING i_ds_id        TYPE /cadaxo/mds_ds_id
                                    i_semantic_key TYPE /cadaxo/mds_ds_semkey.
    METHODS set_link_buffer IMPORTING i_link_id      TYPE /cadaxo/mds_link_id
                                      i_semantic_key TYPE /cadaxo/mds_lk_semkey.
    METHODS set_parameter_buffer IMPORTING i_parameter_id TYPE /cadaxo/mds_parameter_id
                                           i_semantic_key TYPE /cadaxo/mds_pr_semkey.
    METHODS set_property_buffer IMPORTING i_property_id  TYPE /cadaxo/mds_property_id
                                          i_semantic_key TYPE /cadaxo/mds_py_semkey.
ENDCLASS.



CLASS /cadaxo/cl_mds_id IMPLEMENTATION.

  METHOD class_constructor.

    convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.

  METHOD get_link_semkey.

    IF line_exists( link_buffers[ link_id = i_link_id ] ).
      r_semantic_key = link_buffers[ link_id = i_link_id ]-semkey.
    ELSE.
      SELECT SINGLE object_id1, object_id2
             FROM /cadaxo/mds_lk
             WHERE link_id = @i_link_id
             INTO @r_semantic_key.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /cadaxo/cx_mds_id.
      ENDIF.

      set_link_buffer( i_link_id      = i_link_id
                       i_semantic_key = r_semantic_key ).
    ENDIF.

  ENDMETHOD.

  METHOD set_link_buffer.

    INSERT VALUE #( link_id = i_link_id semkey = i_semantic_key ) INTO TABLE link_buffers.

  ENDMETHOD.


  METHOD get_instance.


    IF instance IS INITIAL.
      instance = NEW /cadaxo/cl_mds_id( ).
    ENDIF.

    e_id_handler = instance.

  ENDMETHOD.

  METHOD get_ds_semkey.

    IF line_exists( datasource_buffers[ ds_id = i_ds_id ] ).
      r_semantic_key = datasource_buffers[ ds_id = i_ds_id ]-semkey.
    ELSE.
      SELECT SINGLE type, name
             FROM /cadaxo/mds_ds
             WHERE ds_id = @i_ds_id
             INTO @r_semantic_key.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /cadaxo/cx_mds_id.
      ENDIF.

      set_ds_buffer( i_ds_id        = i_ds_id
                     i_semantic_key = r_semantic_key ).
    ENDIF.

  ENDMETHOD.

  METHOD set_ds_buffer.

    INSERT VALUE #( ds_id = i_ds_id semkey = i_semantic_key ) INTO TABLE datasource_buffers.

  ENDMETHOD.

  METHOD is_structure_supported.

    r_supported = abap_true.

    CASE i_structure_name.
      WHEN '/CADAXO/MDS_DS_SEMKEY'.
      WHEN '/CADAXO/MDS_LK_SEMKEY'.
      WHEN '/CADAXO/MDS_FD_SEMKEY'.
      WHEN '/CADAXO/MDS_AN_SEMKEY'.
      WHEN '/CADAXO/MDS_PR_SEMKEY'.
      WHEN '/CADAXO/MDS_PY_SEMKEY'.
      WHEN OTHERS.
        r_supported = abap_false..
        MESSAGE '' TYPE 'X'.
    ENDCASE.

  ENDMETHOD.


  METHOD build_hash.

    FIELD-SYMBOLS: <buffer> TYPE any.

    DATA(structure) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( i_semkey ) ).
    DATA(structure_name) = structure->get_relative_name( ).
    IF me->is_structure_supported( structure_name ).

      TRY.

          cl_abap_hmac=>calculate_hmac_for_raw( EXPORTING if_key        = CONV #( '' )
                                                          if_data       = string_to_xstring( CONV #( i_semkey ) )
                                                IMPORTING ef_hmacstring = DATA(hashstring) ).
          r_object_id = hashstring.


**** WIP !!!
          CASE structure_name.
            WHEN '/CADAXO/MDS_DS_SEMKEY'.
              TRY.
                  me->get_ds_semkey( r_object_id ).

                CATCH /cadaxo/cx_mds_id.
                  DATA(ds_buffer) = VALUE /cadaxo/mds_ds( ds_id = r_object_id semkey = i_semkey ).
                  ASSIGN ds_buffer TO <buffer>.
                  set_ds_buffer( i_ds_id        = ds_buffer-ds_id
                                 i_semantic_key = ds_buffer-semkey ).
              ENDTRY.

            WHEN '/CADAXO/MDS_LK_SEMKEY'.
              TRY.
                  me->get_link_semkey( r_object_id ).

                CATCH /cadaxo/cx_mds_id.
                  DATA(link_buffer) = VALUE /cadaxo/mds_lk( link_id = r_object_id semkey = i_semkey ).
                  ASSIGN link_buffer TO <buffer>.
                  set_link_buffer( i_link_id      = link_buffer-link_id
                                   i_semantic_key = link_buffer-semkey ).
              ENDTRY.

            WHEN '/CADAXO/MDS_FD_SEMKEY'.
              DATA(field_buffer) = VALUE /cadaxo/mds_fd( field_id = r_object_id semkey = i_semkey ).
              ASSIGN field_buffer TO <buffer>.
            WHEN '/CADAXO/MDS_AN_SEMKEY'.
              DATA(annotation_buffer) = VALUE /cadaxo/mds_an( annotation_id = r_object_id semkey = i_semkey ).
              ASSIGN annotation_buffer TO <buffer>.
            WHEN '/CADAXO/MDS_PR_SEMKEY'.
              DATA(parameter_buffer) = VALUE /cadaxo/mds_pr( parameter_id = r_object_id semkey = i_semkey ).
              ASSIGN parameter_buffer TO <buffer>.
            WHEN OTHERS.
              MESSAGE '' TYPE 'X'.
          ENDCASE.

          IF <buffer> IS ASSIGNED.
            DATA(buffer_table_name) = CONV tabname( structure_name(14) ).

            MODIFY (buffer_table_name) FROM <buffer>.
            IF sy-subrc <> 0.
              MESSAGE 'BUFFER ERROR:' && buffer_table_name TYPE 'X'.
            ENDIF.
          ENDIF.
        CATCH /cadaxo/cx_mds_id.

        CATCH cx_abap_message_digest.
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD string_to_xstring.

    TRY.

        convert_out->convert( EXPORTING data   = i_semkey_string
                              IMPORTING buffer = r_semkey_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
    ENDTRY.

  ENDMETHOD.

  METHOD get_parameter_semkey.

    IF line_exists( parameter_buffers[ parameter_id = i_parameter_id ] ).
      r_semantic_key = parameter_buffers[ parameter_id = i_parameter_id ]-semkey.
    ELSE.
      SELECT SINGLE ds_id, parameter_name
             FROM /cadaxo/mds_pr
             WHERE parameter_id = @i_parameter_id
             INTO @r_semantic_key.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /cadaxo/cx_mds_id.
      ENDIF.

      set_parameter_buffer( i_parameter_id = i_parameter_id
                            i_semantic_key = r_semantic_key ).
    ENDIF.

  ENDMETHOD.

  METHOD get_property_semkey.

    IF line_exists( property_buffers[ property_id = i_property_id ] ).
      r_semantic_key = property_buffers[ property_id = i_property_id ]-semkey.
    ELSE.
      SELECT SINGLE object_id, property_name
             FROM /cadaxo/mds_py
             WHERE property_id = @i_property_id
             INTO @r_semantic_key.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /cadaxo/cx_mds_id.
      ENDIF.

      set_property_buffer( i_property_id = i_property_id
                            i_semantic_key = r_semantic_key ).
    ENDIF.

  ENDMETHOD.

  METHOD set_parameter_buffer.

    INSERT VALUE #( parameter_id = i_parameter_id semkey = i_semantic_key ) INTO TABLE parameter_buffers.

  ENDMETHOD.

  METHOD set_property_buffer.

    INSERT VALUE #( property_id = i_property_id semkey = i_semantic_key ) INTO TABLE property_buffers.

  ENDMETHOD.

ENDCLASS.
