CLASS /cadaxo/cl_mds_api_ddls DEFINITION INHERITING FROM /cadaxo/cl_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor IMPORTING i_sematic_key TYPE /cadaxo/mds_ds_semkey.
    METHODS /cadaxo/if_mds_api_datasource~build_related_entities REDEFINITION.
    METHODS /cadaxo/if_mds_api_datasource~get_fields REDEFINITION.
    METHODS /cadaxo/if_mds_api_datasource~get_parameters REDEFINITION.
    METHODS /cadaxo/if_mds_api_datasource~get_annotations REDEFINITION.

ENDCLASS.



CLASS /cadaxo/cl_mds_api_ddls IMPLEMENTATION.


  METHOD /cadaxo/if_mds_api_datasource~build_related_entities.

*    " STRUCOBJCLASS = APPEND bei APPEND Views
*
*    SELECT SINGLE *
*      FROM ddddlsrc
*      INTO @DATA(stob_source)
*      WHERE ddlname  = @me->/cadaxo/if_mds_api_datasource~object_name
*        AND as4local = @version-active.
*
*    SELECT SINGLE *
*      FROM ddtypes
*      INTO @DATA(ddtypes)
*      WHERE typename = @me->/cadaxo/if_mds_api_datasource~object_name
*        AND state    = @version-active.


* get associations
    SELECT associationname, typekind_t, assorigin, strucobjn_t, card_min, card_max
      FROM dd08b
      INTO TABLE @DATA(stob_associations)
      WHERE strucobjn = @me->/cadaxo/if_mds_api_datasource~header-name
        AND as4local  = @version-active.

    LOOP AT stob_associations ASSIGNING FIELD-SYMBOL(<stob_association>).

      APPEND VALUE #( link_id       = 'GET_ID'
                      object_id1    = me->/cadaxo/if_mds_api_datasource~header-ds_id
                      object_id2    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_ds_semkey( type = SWITCH #( <stob_association>-typekind_t
                                                                                                                            WHEN 'T' THEN /cadaxo/if_mds_api_datasource~types-table
                                                                                                                            WHEN 'B' THEN  /cadaxo/if_mds_api_datasource~types-datadefinition )
                                                                                                        name = <stob_association>-strucobjn_t ) )
                      description   = <stob_association>-associationname
                      card_min      = <stob_association>-card_min
                      card_max      = <stob_association>-card_max
                      relation_type = SWITCH #( <stob_association>-assorigin WHEN 'E' THEN 'EXTERNAL_ASSOCIATION'
                                                                                      ELSE 'ASSOCIATION' ) ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.


    SELECT tabname
           FROM dd26s
           WHERE viewname = @/cadaxo/if_mds_api_datasource~header-sqlviewname
             AND as4local = @version-active
           INTO TABLE @DATA(base_tables).

    LOOP AT base_tables ASSIGNING FIELD-SYMBOL(<base_table>).

      APPEND VALUE #( link_id       = 'GET_ID'
                      object_id1    = me->/cadaxo/if_mds_api_datasource~header-ds_id
                      object_id2    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_ds_semkey(  type = /cadaxo/if_mds_api_datasource~types-table
                                                                                                         name = <base_table>-tabname ) )
                      description   = 'as select from'
                      relation_type = 'BASE' ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.

** get extensions
*    SELECT *
*           FROM ddlx_rt_header AS a
*        INNER JOIN ddlxsrc AS b ON b~uuid = a~dt_uuid
*                                AND b~ddlxname = a~ddlxname
*        LEFT OUTER JOIN ddlxsrct AS c ON c~ddlxname = a~ddlxname
*                                      AND c~version = b~version
*                                      AND c~language = @sy-langu
*        INTO TABLE @DATA(metadataextensions)
*        WHERE a~extended_artifact = @me->/cadaxo/if_mds_api_datasource~object_name
*          AND b~version = a.
*    LOOP AT metadataextensions ASSIGNING FIELD-SYMBOL(<extension>).
*      CLEAR l_node.
*      l_node-object = 'DDLX'.
*      l_node-id = get_id_by_object( EXPORTING i_obj_name = CONV #( <extension>-a-ddlxname )
*                                              i_object = CONV #( l_node-object )
*                                    IMPORTING e_created = created_flag ).
*      l_node-name = <extension>-a-ddlxname.
*      l_node-description = <extension>-c-description.
*      IF created_flag = abap_true.
*        APPEND l_node TO gt_nodes.
*      ENDIF.
*
*      l_relation-idfrom = i_node_id_from.
*      l_relation-idto = l_node-id.
*      l_relation-description = 'extended'.
*      l_relation-card_min = 1.
*      l_relation-card_max = 1.
*      l_relation-type = 'EXTENSION'.
*      APPEND l_relation TO gt_relations.
*
*    ENDLOOP.

* get enhancements
    SELECT *
           FROM dd02b
           WHERE parentname    = @me->/cadaxo/if_mds_api_datasource~header-name
             AND as4local      = @version-active
             AND strucobjclass = 'APPEND'
           INTO TABLE @DATA(stob_append_headers).

    LOOP AT stob_append_headers ASSIGNING FIELD-SYMBOL(<stob_append_header>).

      APPEND VALUE #( link_id       = 'GET_ID'
                      object_id1    = me->/cadaxo/if_mds_api_datasource~header-ds_id
                      object_id2    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_ds_semkey(  type = /cadaxo/if_mds_api_datasource~types-datadefinition
                                                                                                         name = <stob_append_header>-strucobjn ) )
                      description   = 'enhanced'
                      card_min      = 1
                      card_max      = 1
                      relation_type = 'ENHANCEMENT' ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.


* is base table of
    SELECT b~strucobjn
           FROM dd26s AS a
           INNER JOIN dd02bnd AS b
                 ON b~dbtabname = a~viewname
           WHERE a~tabname  = @/cadaxo/if_mds_api_datasource~header-sqlviewname
             AND b~as4local = @version-active
           INTO TABLE @DATA(table_sources).
    SELECT strucobjn
      FROM dd08b
      APPENDING TABLE @table_sources
      WHERE strucobjn_t = @me->/cadaxo/if_mds_api_datasource~header-name
        AND as4local  = @version-active.
    LOOP AT table_sources ASSIGNING FIELD-SYMBOL(<table_source>).

      APPEND VALUE #( link_id       = 'GET_ID'
                      object_id1    = me->/cadaxo/if_mds_api_datasource~header-ds_id
                      object_id2    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_ds_semkey(  type = /cadaxo/if_mds_api_datasource~types-datadefinition
                                                                                                         name = <table_source>-strucobjn ) )
                      description   = 'is used in'
                      relation_type = 'ISUSED' ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.


    LOOP AT me->/cadaxo/if_mds_api_datasource~relations ASSIGNING FIELD-SYMBOL(<relation>) WHERE link_id = 'GET_ID'.
      <relation>-link_id = /cadaxo/cl_mds_api=>build_object_id( <relation>-semkey ).
    ENDLOOP.

*data(save) = me->/cadaxo/if_mds_api_datasource~relations.
*    data(before) = lines( me->/cadaxo/if_mds_api_datasource~relations ).
*    sort me->/cadaxo/if_mds_api_datasource~relations by link_id.
*    delete ADJACENT DUPLICATES FROM me->/cadaxo/if_mds_api_datasource~relations comparing link_id.
*    data(after) = lines( me->/cadaxo/if_mds_api_datasource~relations ).
*if before <> after.
*data(diff) = before - after.
*endif.
  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~get_annotations.

    SELECT name AS annotation, position, value
         FROM ddheadanno
         WHERE strucobjn = @me->/cadaxo/if_mds_api_datasource~header-name
         ORDER BY position
         into table @DATA(annotations).

    LOOP AT annotations ASSIGNING FIELD-SYMBOL(<annotation>).
      APPEND VALUE #( annotation_id    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_an_semkey( object_id  = me->/cadaxo/if_mds_api_datasource~header-ds_id
                                                                                                           annotation = <annotation>-annotation ) )
                      object_id   = me->/cadaxo/if_mds_api_datasource~header-ds_id
                      annotation  = <annotation>-annotation
                      value       = <annotation>-value
                      position    = <annotation>-position ) TO r_annotations.
    ENDLOOP.

*        SELECT lfieldname AS fieldname, name, position, value
*               FROM ddfieldanno
*               WHERE strucobjn = @name
*               ORDER BY position
*               APPENDING CORRESPONDING FIELDS OF TABLE @e_annotations.
  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~get_fields.

    SELECT cdsfields~fieldname AS field_name, cdsfields~fieldname_raw AS field_alias, cdsfields~position,
           sqlviewfields~tabname AS base_tabable, sqlviewfields~fieldname AS base_field_name
           FROM dd03nd AS cdsfields
           INNER JOIN dd27s AS sqlviewfields
                 ON  sqlviewfields~viewname  = @me->/cadaxo/if_mds_api_datasource~header-sqlviewname
                 AND sqlviewfields~viewfield = cdsfields~fieldname
                 AND sqlviewfields~as4local  = cdsfields~as4local
           WHERE strucobjn = @me->/cadaxo/if_mds_api_datasource~header-name
             AND cdsfields~as4local  = @/cadaxo/cl_mds_api_ds=>version-active
           ORDER BY position
           into table @DATA(cds_fields).

    LOOP AT cds_fields ASSIGNING FIELD-SYMBOL(<cds_field>).

      DATA(field) = /cadaxo/cl_mds_api_field=>get_instance( i_field_id =  /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_fd_semkey( ds_id      = me->/cadaxo/if_mds_api_datasource~header-ds_id
                                                                                                                                            field_name = <cds_field>-field_name ) )
                                                            i_data = CORRESPONDING #( <cds_field> ) ).

      APPEND VALUE #( field_id = field->get_id( )
                      api      = field ) TO r_fields.

    ENDLOOP.

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~get_parameters.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE head~strucobjn AS name,
                  head~chguser AS changed_by,
*                  head~chgdate as changed_at,
                  text~ddtext  AS description,
                  dbtabname AS sqlviewname
           FROM dd02b AS head
           INNER JOIN dd02bnd AS sqlview
                 ON  sqlview~strucobjn = head~strucobjn
                 AND sqlview~as4local  = head~as4local
           LEFT OUTER JOIN dd02bt AS text
                ON  text~strucobjn = head~strucobjn
                AND text~as4local  = head~as4local
           INTO CORRESPONDING FIELDS OF @me->/cadaxo/if_mds_api_datasource~header
           WHERE head~strucobjn = @i_sematic_key-name
             AND head~as4local  = @version-active.
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

*          <node>-http_link = |/sap/bc/adt/ddic/ddl/sources/{ <node>-name }/source/main?version=active&sap-client={ sy-mandt }|.
*          <node>-adt_link = |adt://{ sy-sysid }/sap/bc/adt/ddic/ddl/sources/{ <node>-name }|.

  ENDMETHOD.
ENDCLASS.
