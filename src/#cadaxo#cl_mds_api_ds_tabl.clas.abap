CLASS /cadaxo/cl_mds_api_ds_tabl DEFINITION INHERITING FROM /cadaxo/cl_mds_api_ds
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor IMPORTING i_sematic_key TYPE /cadaxo/mds_ds_semkey.
    METHODS /cadaxo/if_mds_api_datasource~build_related_entities REDEFINITION.
    METHODS /cadaxo/if_mds_api_datasource~get_fields REDEFINITION.
    METHODS /cadaxo/if_mds_api_datasource~get_action_links REDEFINITION.

ENDCLASS.


CLASS /cadaxo/cl_mds_api_ds_tabl IMPLEMENTATION.

  METHOD constructor.

    super->constructor( i_sematic_key ).

    SELECT SINGLE head~tabname  AS name,
                  head~as4user AS changed_by,
*                  head~chgdate as changed_at,
                  text~ddtext  AS description
                  FROM dd02l AS head
                  LEFT OUTER JOIN dd02t AS text
                    ON  text~tabname  = head~tabname
                    AND text~as4local = head~as4local
                  INTO CORRESPONDING FIELDS OF @me->/cadaxo/if_mds_api_datasource~header
                  WHERE head~tabname  = @i_sematic_key-name
                    AND head~as4local = @version-active.
    IF sy-subrc <> 0.
      MESSAGE '' TYPE 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~build_related_entities.

    IF me->related_read = abap_false.

      IF me->/cadaxo/if_mds_api_datasource~header-role >= /cadaxo/if_mds_api=>ds_role-main.
        SELECT b~strucobjn
               FROM dd26s AS a
               INNER JOIN dd02bnd AS b
                     ON b~dbtabname = a~viewname
               WHERE a~tabname  = @/cadaxo/if_mds_api_datasource~header-name
                 AND b~as4local = @version-active
               INTO TABLE @DATA(base_tables).

        LOOP AT base_tables ASSIGNING FIELD-SYMBOL(<base_table>).

          APPEND VALUE #( link_id       = 'GET_ID'
                          object_id1    = me->/cadaxo/if_mds_api_datasource~header-ds_id
                          object_id2    = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_ds_semkey(  type = /cadaxo/if_mds_api_datasource~type-datadefinition
                                                                                                             name = <base_table>-strucobjn ) )
                          description   = relation_cust-isused-description
                          relation_type = relation_cust-isused-type ) TO me->/cadaxo/if_mds_api_datasource~relations.

        ENDLOOP.

        LOOP AT me->/cadaxo/if_mds_api_datasource~relations ASSIGNING FIELD-SYMBOL(<relation>) WHERE link_id = 'GET_ID'.
          <relation>-link_id = /cadaxo/cl_mds_api=>build_object_id( <relation>-semkey ).
        ENDLOOP.

      ENDIF.

      me->related_read = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_fields.

    SELECT fields~fieldname AS field_name,
           fields~position,
           fields~datatype,
           fields~leng,
           fields~decimals,
           fields~rollname AS data_element
           FROM dd03l AS fields
           WHERE tabname  = @me->/cadaxo/if_mds_api_datasource~header-name
             AND as4local = 'A'
           ORDER BY position
           into table @DATA(fields).

    me->ds_fields = CORRESPONDING #( fields ).

    LOOP AT fields ASSIGNING FIELD-SYMBOL(<ds_field>) WHERE field_name NP '.INC*'.

      DATA(field_data) = CORRESPONDING /cadaxo/if_mds_api_field=>ty_data( <ds_field> ).
      field_data-length = |{ <ds_field>-leng ALPHA = OUT },{ <ds_field>-decimals ALPHA = OUT }|.

      DATA(field) = /cadaxo/cl_mds_api_field=>get_instance( i_field_id =  /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_fd_semkey( ds_id      = me->/cadaxo/if_mds_api_datasource~header-ds_id
                                                                                                                                            field_name = <ds_field>-field_name ) )
                                                            i_data = field_data ).

      APPEND VALUE #( field_id = field->get_id( )
                      api      = field ) TO r_fields.

    ENDLOOP.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_action_links.

    r_links_action = super->/cadaxo/if_mds_api_datasource~get_action_links( ).

    r_links_action-display = |/sap/bc/adt/ddic/structures/{ me->/cadaxo/if_mds_api_datasource~header-name }/source/main?version=active&sap-client={ sy-mandt }|.

    IF r_links_action-edit IS INITIAL.
      r_links_action-edit = |adt://{ sy-sysid }/sap/bc/adt/vit/wb/object_type/tabldt/object_name/{ me->/cadaxo/if_mds_api_datasource~header-name }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
