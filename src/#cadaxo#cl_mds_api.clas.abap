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

    CLASS-DATA id_handler TYPE REF TO /cadaxo/cl_mds_id.
    METHODS get_ds_reader IMPORTING i_ds_id            TYPE /cadaxo/mds_ds_id
                          RETURNING VALUE(r_ds_reader) TYPE REF TO /cadaxo/if_mds_api_datasource.
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


    DATA(links) = /cadaxo/if_mds_api~get_links_by_dsid( id_handler->get_link_semkey( i_link_id )-object_id1 ).

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

      e_id = id_handler->build_hash( i_semkey ).

  ENDMETHOD.

  METHOD class_constructor.

    id_handler = /cadaxo/cl_mds_id=>get_instance( ).

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



ENDCLASS.
