CLASS /cadaxo/cl_mds_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /cadaxo/if_mds_api.

    CLASS-METHODS get_instance RETURNING VALUE(e_api) TYPE REF TO /cadaxo/if_mds_api.

    CLASS-METHODS class_constructor.
    CLASS-METHODS build_object_id IMPORTING i_ds_semkey TYPE /cadaxo/mds_ds_semkey
                                  RETURNING VALUE(e_id) TYPE /cadaxo/mds_ds_id.
  PROTECTED SECTION.

    CLASS-DATA instance TYPE REF TO /cadaxo/if_mds_api.
  PRIVATE SECTION.
    CLASS-DATA convert_out TYPE REF TO cl_abap_conv_out_ce.
    METHODS get_ds_reader IMPORTING i_ds_semkey        TYPE /cadaxo/mds_ds_semkey
                          RETURNING VALUE(r_ds_reader) TYPE REF TO /cadaxo/if_mds_api_datasource.
    CLASS-METHODS string_to_xstring IMPORTING i_semkey_string         TYPE string
                                    RETURNING VALUE(r_semkey_xstring) TYPE xstring.

ENDCLASS.


CLASS /cadaxo/cl_mds_api IMPLEMENTATION.

  METHOD class_constructor.

    convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

  ENDMETHOD.

  METHOD get_instance.

    IF instance IS INITIAL.
      instance = NEW /cadaxo/cl_mds_api( ).
    ENDIF.

    e_api = instance.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api~get_datasources_by_id.

    SELECT SINGLE type, name
           FROM /cadaxo/mds_ds
           WHERE ds_id = @i_ds_id
           INTO @DATA(semkey).
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

    r_datasources = me->/cadaxo/if_mds_api~get_datasources_by_semkey( i_ds_semkey = semkey ).

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api~get_datasources_by_semkey.

    DATA: id TYPE /cadaxo/mds_ds_id.

    id = me->build_object_id( i_ds_semkey ).

    DATA(ds_reader) = me->get_ds_reader( i_ds_semkey ).

    ds_reader->build_related_entities(  ).

    DATA(datasource) = ds_reader->get_datasource( ).
    APPEND CORRESPONDING #( datasource ) TO r_datasources.

    DATA(relations) = ds_reader->get_relations( ).
    LOOP AT relations ASSIGNING FIELD-SYMBOL(<relation>).

      DATA(related_datasource) = me->get_ds_reader( <relation>-semkey ).
      APPEND CORRESPONDING #( related_datasource->get_datasource( ) ) TO r_datasources.

    ENDLOOP.



*    CASE i_object.
*      WHEN 'DDLS'.
*        get_related_entities_ddlb(
*          EXPORTING i_obj_name = CONV #( i_obj_name )
*                    i_object = i_object
*                    i_node_id_from = l_node-id ).
*      WHEN 'TABL'.
*    ENDCASE.
*
*    l_node-http_link = |/sap/bc/adt/ddic/ddl/sources/{ l_node-name }/source/main?version=active&sap-client={ sy-mandt }|.
*    l_node-adt_link = |adt://{ sy-sysid }/sap/bc/adt/ddic/ddl/sources/{ l_node-name }|.
*
*    LOOP AT gt_nodes ASSIGNING FIELD-SYMBOL(<node>).
*      CASE <node>-object.
*        WHEN 'DDLS'.
*          <node>-http_link = |/sap/bc/adt/ddic/ddl/sources/{ <node>-name }/source/main?version=active&sap-client={ sy-mandt }|.
*          <node>-adt_link = |adt://{ sy-sysid }/sap/bc/adt/ddic/ddl/sources/{ <node>-name }|.
*        WHEN 'TABL'.
*          <node>-http_link = |/sap/bc/adt/ddic/structures/{ <node>-name }/source/main?version=active&sap-client={ sy-mandt }|.
*          <node>-adt_link = |adt://{ sy-sysid }/sap/bc/adt/vit/wb/object_type/tabldt/object_name/{ <node>-name }|.
*      ENDCASE.
*    ENDLOOP.
*
*
*    t_nodes = gt_nodes.
*    t_relations = gt_relations.
*
*
  ENDMETHOD.

  METHOD get_ds_reader.

    r_ds_reader = /cadaxo/cl_mds_api_ds=>get_instance( i_ds_semkey ).

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

  METHOD build_object_id.

    TRY.

        cl_abap_hmac=>calculate_hmac_for_raw( EXPORTING if_key        = CONV #( '' )
                                                        if_data       = string_to_xstring( CONV #( i_ds_semkey ) )
                                              IMPORTING ef_hmacstring = DATA(hashstring) ).

        e_id = hashstring.

      CATCH cx_abap_message_digest.
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
