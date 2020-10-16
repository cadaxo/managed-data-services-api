CLASS /cadaxo/cl_mds_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /cadaxo/if_mds_api.

    CLASS-METHODS get_instance RETURNING VALUE(e_api) TYPE REF TO /cadaxo/if_mds_api.

    CLASS-METHODS class_constructor.
    CLASS-METHODS build_object_id IMPORTING i_semkey    TYPE any
                                  RETURNING VALUE(e_id) TYPE /cadaxo/mds_object_id.
  PROTECTED SECTION.

    CLASS-DATA instance TYPE REF TO /cadaxo/if_mds_api.
  PRIVATE SECTION.
    CLASS-DATA convert_out TYPE REF TO cl_abap_conv_out_ce.
    METHODS get_ds_reader IMPORTING i_ds_id            TYPE /cadaxo/mds_ds_id
                          RETURNING VALUE(r_ds_reader) TYPE REF TO /cadaxo/if_mds_api_datasource.
    CLASS-METHODS string_to_xstring IMPORTING i_semkey_string         TYPE string
                                    RETURNING VALUE(r_semkey_xstring) TYPE xstring.

ENDCLASS.



CLASS /cadaxo/cl_mds_api IMPLEMENTATION.


  METHOD /cadaxo/if_mds_api~get_annotations_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_annotations = ds_reader->get_annotations( ).

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_annotations_by_fieldid.

    SELECT SINGLE ds_id, field_name
           FROM /cadaxo/mds_fd
           WHERE field_id = @i_field_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(annotations) = /cadaxo/if_mds_api~get_annotations_by_dsid( semkey-ds_id ).


  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_annotation_by_id.

    SELECT SINGLE object_id, annotation
           FROM /cadaxo/mds_an
           WHERE annotation_id = @i_annotation_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(annotations) = /cadaxo/if_mds_api~get_annotations_by_dsid( semkey-object_id ).

    r_annotation = annotations[ annotation_id = i_annotation_id ].

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_datasources_by_id.

    DATA(read_depth) = COND int4( WHEN i_read_depth > 0 THEN i_read_depth - 1
                                  WHEN i_read_depth = 0 THEN 0
                                  ELSE -1 ).

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    APPEND ds_reader->get_datasource( ) TO r_datasources.

    IF read_depth >= 0.

      IF read_depth = 0.
        read_depth = -1.
      ENDIF.

      ds_reader->build_related_entities(  ).

      DATA(relations) = ds_reader->get_relations( ).
      LOOP AT relations ASSIGNING FIELD-SYMBOL(<relation>).

        DATA(related_dss) = /cadaxo/if_mds_api~get_datasources_by_id( i_ds_id = <relation>-object_id2 i_read_depth = read_depth ).

        LOOP AT related_dss ASSIGNING FIELD-SYMBOL(<related_ds>).

          IF NOT line_exists( r_datasources[ ds_id = <related_ds>-ds_id ] ).
            APPEND <related_ds> TO r_datasources.
          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_datasources_by_semkey.

    DATA(id) = me->build_object_id( i_ds_semkey ).

    r_datasources = me->/cadaxo/if_mds_api~get_datasources_by_id( i_ds_id      = id
                                                                  i_read_depth = i_read_depth ).

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_datasource_by_id.

    DATA(datasources) = /cadaxo/if_mds_api~get_datasources_by_id( i_ds_id       = i_ds_id
                                                                  i_read_depth  = 1 ).

    r_datasource = datasources[ ds_id = i_ds_id ].

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_fields_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_fields = ds_reader->get_fields( ).

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_field_by_id.

    SELECT SINGLE ds_id, field_name
           FROM /cadaxo/mds_fd
           WHERE field_id = @i_field_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(fields) = /cadaxo/if_mds_api~get_fields_by_dsid( semkey-ds_id ).

    r_field = fields[ field_id = i_field_id ].

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_links_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_relations = ds_reader->get_relations( ).

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_link_by_id.

    SELECT SINGLE object_id1 AS ds_id, object_id2
           FROM /cadaxo/mds_lk
           WHERE link_id = @i_link_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    DATA(links) = /cadaxo/if_mds_api~get_links_by_dsid( semkey-ds_id ).

    r_link = links[ link_id = i_link_id ].

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_parameters_by_dsid.

    DATA(ds_reader) = me->get_ds_reader( i_ds_id ).

    ds_reader->build_related_entities( ).

    r_parameters = ds_reader->get_parameters( ).

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_parameters_by_fieldid.

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api~get_parameter_by_id.

  ENDMETHOD.


  METHOD build_object_id.

    FIELD-SYMBOLS: <buffer> TYPE any.

    TRY.

        DATA(structure) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( i_semkey ) ).
        DATA(structure_name) = structure->get_relative_name( ).
        CASE structure_name.
          WHEN '/CADAXO/MDS_DS_SEMKEY'.
          WHEN '/CADAXO/MDS_LK_SEMKEY'.
          WHEN '/CADAXO/MDS_FD_SEMKEY'.
          WHEN '/CADAXO/MDS_AN_SEMKEY'.
          WHEN '/CADAXO/MDS_PR_SEMKEY'.
          WHEN OTHERS.
            MESSAGE '' TYPE 'X'.
        ENDCASE.

        cl_abap_hmac=>calculate_hmac_for_raw( EXPORTING if_key        = CONV #( '' )
                                                        if_data       = string_to_xstring( CONV #( i_semkey ) )
                                              IMPORTING ef_hmacstring = DATA(hashstring) ).

        e_id = hashstring.

**** WIP !!!
        CASE structure_name.
          WHEN '/CADAXO/MDS_DS_SEMKEY'.
            DATA(ds_buffer) = VALUE /cadaxo/mds_ds( ds_id = e_id semkey = i_semkey ).
            ASSIGN ds_buffer TO <buffer>.
          WHEN '/CADAXO/MDS_LK_SEMKEY'.
            DATA(link_buffer) = VALUE /cadaxo/mds_lk( link_id = e_id semkey = i_semkey ).
            ASSIGN link_buffer TO <buffer>.
          WHEN '/CADAXO/MDS_FD_SEMKEY'.
            DATA(field_buffer) = VALUE /cadaxo/mds_fd( field_id = e_id semkey = i_semkey ).
            ASSIGN field_buffer TO <buffer>.
          WHEN '/CADAXO/MDS_AN_SEMKEY'.
            DATA(annotation_buffer) = VALUE /cadaxo/mds_an( annotation_id = e_id semkey = i_semkey ).
            ASSIGN annotation_buffer TO <buffer>.
          WHEN '/CADAXO/MDS_PR_SEMKEY'.
            DATA(parameter_buffer) = VALUE /cadaxo/mds_pr( parameter_id = e_id semkey = i_semkey ).
            ASSIGN parameter_buffer TO <buffer>.
          WHEN OTHERS.
            MESSAGE '' TYPE 'X'.
        ENDCASE.

        DATA(buffer_table_name) = CONV tabname( structure_name(14) ).

        MODIFY (buffer_table_name) FROM <buffer>.
        IF sy-subrc <> 0.
          MESSAGE 'BUFFER ERROR:' && buffer_table_name TYPE 'X'.
        ENDIF.

      CATCH cx_abap_message_digest.
    ENDTRY.

  ENDMETHOD.


  METHOD class_constructor.

    convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.


  METHOD get_ds_reader.

    r_ds_reader = /cadaxo/cl_mds_api_ds=>get_instance( i_ds_id ).

  ENDMETHOD.


  METHOD get_instance.

    IF instance IS INITIAL.
      instance = NEW /cadaxo/cl_mds_api( ).
    ENDIF.

    e_api = instance.

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
ENDCLASS.
