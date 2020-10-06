CLASS /cadaxo/cl_mds_api_ddls DEFINITION INHERITING FROM /cadaxo/cl_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor IMPORTING i_sematic_key TYPE /cadaxo/mds_ds_semkey.
    METHODS /cadaxo/if_mds_api_datasource~build_related_entities REDEFINITION.

ENDCLASS.


CLASS /cadaxo/cl_mds_api_ddls IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE head~strucobjn AS name,
                  head~chguser AS changed_by,
*                  head~chgdate as changed_at,
                  text~ddtext  AS description
                  FROM dd02b AS head
                  LEFT OUTER JOIN dd02bt AS text
                    ON  text~strucobjn = head~strucobjn
                    AND text~as4local  = head~as4local
                  INTO CORRESPONDING FIELDS OF @me->/cadaxo/if_mds_api_datasource~header
                  WHERE head~strucobjn = @i_sematic_key-name
                    AND head~as4local  = @version-active.
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.


*    APPEND VALUE #( to          = me->/cadaxo/if_mds_api_datasource~header-ds_id
*                    object_type = me->/cadaxo/if_mds_api_datasource~header-type
*                    object_name = me->/cadaxo/if_mds_api_datasource~header-name
*                    description = me->/cadaxo/if_mds_api_datasource~header-description
*                    type        = 'MAIN' ) TO me->/cadaxo/if_mds_api_datasource~relations.

  ENDMETHOD.


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

* get db view
    SELECT SINGLE *
       FROM dd02bnd
       INTO @DATA(stob_nodes)
        WHERE strucobjn = @me->/cadaxo/if_mds_api_datasource~header-name
          AND as4local  = @version-active.

* get associations
    SELECT *
      FROM dd08b
      INTO TABLE @DATA(stob_associations)
      WHERE strucobjn = @me->/cadaxo/if_mds_api_datasource~header-name
        AND as4local  = @version-active.

    LOOP AT stob_associations ASSIGNING FIELD-SYMBOL(<stob_association>).

      APPEND VALUE #( to            = 'GET_ID'
                      type          = SWITCH #( <stob_association>-typekind_t WHEN 'T' THEN 'TABL'
                                                                              WHEN 'B' THEN 'DDLS' )
                      name          = <stob_association>-strucobjn_t
                      description   = <stob_association>-associationname
                      card_min      = <stob_association>-card_min
                      card_max      = <stob_association>-card_max
                      relation_type = SWITCH #( <stob_association>-assorigin WHEN 'E' THEN 'EXTERNAL_ASSOCIATION'
                                                                                      ELSE 'ASSOCIATION' ) ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.


    SELECT *
           FROM dd26s
           WHERE viewname = @stob_nodes-dbtabname
             AND as4local = @version-active
           INTO TABLE @DATA(base_tables).

    LOOP AT base_tables ASSIGNING FIELD-SYMBOL(<base_table>).

      APPEND VALUE #( to            = 'GET_ID'
                      type          = 'TABL'
                      name          = <base_table>-tabname
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

      APPEND VALUE #( to            = 'GET_ID'
                      type          = 'DDLS'
                      name          = <stob_append_header>-strucobjn
                      description   = 'enhanced'
                      card_min      = 1
                      card_max      = 1
                      relation_type = 'ENHANCEMENT' ) TO me->/cadaxo/if_mds_api_datasource~relations.

    ENDLOOP.

    LOOP AT me->/cadaxo/if_mds_api_datasource~relations ASSIGNING FIELD-SYMBOL(<relatiion>) WHERE to = 'GET_ID'.
      <relatiion>-to = /cadaxo/cl_mds_api=>build_object_id( <relatiion>-semkey ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
