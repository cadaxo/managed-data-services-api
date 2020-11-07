CLASS /cadaxo/cl_mds_api_ds DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES /cadaxo/if_mds_api_datasource.

    CLASS-METHODS class_constructor.
    CLASS-METHODS get_instance IMPORTING i_ds_id           TYPE /cadaxo/mds_ds_id
                               RETURNING VALUE(e_instance) TYPE REF TO /cadaxo/if_mds_api_datasource.
    METHODS constructor IMPORTING i_sematic_key TYPE /cadaxo/mds_ds_semkey.

  PROTECTED SECTION.

    TYPES: BEGIN OF ty_ds_field,
             field_name      TYPE fieldname,
             field_alias     TYPE fieldname_raw,
             position        TYPE  ddfdpos,
             base_tabable    TYPE vibastab,
             base_field_name TYPE vibasfld,
           END OF ty_ds_field,
           ty_ds_fields TYPE STANDARD TABLE OF ty_ds_field.

    TYPES: BEGIN OF ty_instance,
             ds_id    TYPE /cadaxo/mds_ds_id,
             instance TYPE REF TO /cadaxo/if_mds_api_datasource,
           END OF ty_instance,
           ty_instances TYPE SORTED TABLE OF ty_instance WITH UNIQUE KEY ds_id.

    CONSTANTS: BEGIN OF version,
                 active TYPE as4local VALUE 'A',
               END OF version.

    CONSTANTS: BEGIN OF relation_cust,
                 BEGIN OF base,
                   type        TYPE string VALUE 'BASE',
                   description TYPE string VALUE 'as select from',
                   role        TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE /cadaxo/if_mds_api=>ds_role-parent,
                 END OF base,
                 BEGIN OF enhancement,
                   type        TYPE string VALUE 'ENHANCEMENT',
                   description TYPE string VALUE 'enhances',
                   role        TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE /cadaxo/if_mds_api=>ds_role-parent,
                 END OF enhancement,
                 BEGIN OF isused,
                   type        TYPE string VALUE 'ISUSED',
                   description TYPE string VALUE 'is used in',
                   role        TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE /cadaxo/if_mds_api=>ds_role-child,
                 END OF isused,
                 BEGIN OF sqlview,
                   type        TYPE string VALUE 'SQLVIEW',
                   description TYPE string VALUE 'has SQL View',
                   role        TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE /cadaxo/if_mds_api=>ds_role-parent,
                 END OF sqlview,
                 BEGIN OF metaextension,
                   type        TYPE string VALUE 'METADATAEXTENSION',
                   description TYPE string VALUE 'has Metadata Extension',
                   role        TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE /cadaxo/if_mds_api=>ds_role-parent,
                 END OF metaextension,
               END OF relation_cust.

    CLASS-DATA instances TYPE ty_instances.
    CLASS-DATA id_handler TYPE REF TO /cadaxo/cl_mds_id.

    DATA: ds_fields     TYPE ty_ds_fields.
    DATA: related_read  TYPE abap_bool.

ENDCLASS.


CLASS /cadaxo/cl_mds_api_ds IMPLEMENTATION.

  METHOD class_constructor.
    id_handler = /cadaxo/cl_mds_id=>get_instance( ).
  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_datasource.
    r_datasource = me->/cadaxo/if_mds_api_datasource~header.
  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~get_relations.
    r_relations = me->/cadaxo/if_mds_api_datasource~relations.
  ENDMETHOD.

  METHOD constructor.

    me->/cadaxo/if_mds_api_datasource~header-semkey = i_sematic_key.
    me->/cadaxo/if_mds_api_datasource~header-ds_id = /cadaxo/cl_mds_api=>build_object_id( me->/cadaxo/if_mds_api_datasource~header-semkey ).
    me->/cadaxo/if_mds_api_datasource~header-api = me.

  ENDMETHOD.

  METHOD get_instance.

    DATA: ds_instance TYPE REF TO /cadaxo/if_mds_api_datasource.

    IF NOT line_exists( instances[ ds_id = i_ds_id ] ).

      DATA(semkey) = id_handler->get_ds_semkey( i_ds_id ).

      DATA(ds_class_name) = '/CADAXO/CL_MDS_API_DS_' && semkey-type.
      CREATE OBJECT ds_instance TYPE (ds_class_name) EXPORTING i_sematic_key = semkey.

      IF instances IS INITIAL.
        ds_instance->set_role( /cadaxo/if_mds_api=>ds_role-main ).
      ENDIF.

      INSERT VALUE #( ds_id    = i_ds_id
                      instance = ds_instance ) INTO TABLE instances ASSIGNING FIELD-SYMBOL(<instance>).


    ELSE.
      ASSIGN instances[ ds_id = i_ds_id ] TO <instance>.
    ENDIF.

    e_instance = <instance>-instance.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~build_related_entities.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_fields.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_annotations.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_parameters.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_action_links.
*      lr_object = cl_wb_object=>create_from_transport_key( p_object = 'DDLS' p_obj_name = CONV #( g_ddlname ) ).
*      lr_adt_objref = cl_adt_tools_core_factory=>get_instance( )->get_uri_mapper( )->map_wb_object_to_objref( lr_object ).
*      g_adt_link = |{ 'adt://' }{ to_lower( sy-sysid ) }{ lr_adt_objref->ref_data-uri }|.
  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~set_role.

    me->/cadaxo/if_mds_api_datasource~header-depth = 0.
    IF me->/cadaxo/if_mds_api_datasource~header-role = /cadaxo/if_mds_api=>ds_role-main.
      me->/cadaxo/if_mds_api_datasource~header-role  = i_role.
    ENDIF.

  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~has_field.

    IF i_fieldname_search-search_object_name IS NOT INITIAL.
      IF me->/cadaxo/if_mds_api_datasource~header-name <> i_fieldname_search-search_object_name
         AND me->/cadaxo/if_mds_api_datasource~header-sqlviewname <> i_fieldname_search-search_object_name.
        RETURN.
      ENDIF.
    ENDIF.
    IF me->ds_fields IS INITIAL.
      me->/cadaxo/if_mds_api_datasource~get_fields( ).
    ENDIF.

    IF line_exists( ds_fields[ field_name = i_fieldname_search-search_field_name ] ).
      ASSIGN ds_fields[ field_name = i_fieldname_search-search_field_name ] TO FIELD-SYMBOL(<field>).
    ELSEIF line_exists( ds_fields[ field_alias = i_fieldname_search-search_field_name ] ).
      ASSIGN ds_fields[ field_alias = i_fieldname_search-search_field_name ] TO <field>.
    ENDIF.
* search_field_name  type fieldname
* base_tabable  type vibastab
* base_field_name  type vibasfld

    IF <field> IS ASSIGNED.
      me->/cadaxo/if_mds_api_datasource~header-field_search-search_field_name = <field>-field_name.
      IF me->/cadaxo/if_mds_api_datasource~header-sqlviewname IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-search_object_name = me->/cadaxo/if_mds_api_datasource~header-sqlviewname.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-search_object_name = me->/cadaxo/if_mds_api_datasource~header-name.
      ENDIF.
      IF <field>-base_field_name IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-base_field_name = <field>-base_field_name.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-base_field_name = i_fieldname_search-search_field_name.
      ENDIF.
      IF <field>-base_tabable IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-base_object_name = <field>-base_tabable.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-base_object_name = me->/cadaxo/if_mds_api_datasource~header-name.
      ENDIF.
    ENDIF.
    r_field_source_ds = /cadaxo/if_mds_api_datasource~header-field_search.
  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~uses_field.

*    IF i_fieldname_search-base_object_name IS NOT INITIAL.
*      IF me->/cadaxo/if_mds_api_datasource~header-name <> i_fieldname_search-base_object_name
*         AND me->/cadaxo/if_mds_api_datasource~header-sqlviewname <> i_fieldname_search-base_object_name.
*        RETURN.
*      ENDIF.
*    ENDIF.
    IF me->ds_fields IS INITIAL.
      me->/cadaxo/if_mds_api_datasource~get_fields( ).
    ENDIF.

    IF line_exists( ds_fields[ base_field_name = i_fieldname_search-base_field_name
                               base_tabable    = i_fieldname_search-base_object_name ] ).
      ASSIGN ds_fields[ base_field_name = i_fieldname_search-base_field_name
                        base_tabable    = i_fieldname_search-base_object_name ] TO FIELD-SYMBOL(<field>).

      me->/cadaxo/if_mds_api_datasource~header-field_search-search_field_name = <field>-field_name.
      IF me->/cadaxo/if_mds_api_datasource~header-sqlviewname IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-search_object_name = me->/cadaxo/if_mds_api_datasource~header-sqlviewname.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-search_object_name = me->/cadaxo/if_mds_api_datasource~header-name.
      ENDIF.
      IF <field>-base_field_name IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-base_field_name = <field>-base_field_name.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-base_field_name = i_fieldname_search-search_field_name.
      ENDIF.
      IF <field>-base_tabable IS NOT INITIAL.
        /cadaxo/if_mds_api_datasource~header-field_search-base_object_name = <field>-base_tabable.
      ELSE.
        /cadaxo/if_mds_api_datasource~header-field_search-base_object_name = me->/cadaxo/if_mds_api_datasource~header-name.
      ENDIF.
    ENDIF.
    r_field_source_ds = /cadaxo/if_mds_api_datasource~header-field_search.
  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_properties.

      APPEND VALUE #( property_id = /cadaxo/cl_mds_api=>build_object_id( VALUE /cadaxo/mds_py_semkey( object_id     = me->/cadaxo/if_mds_api_datasource~header-ds_id
                                                                                                      property_name = 'DSID' ) )
                      object_id   = me->/cadaxo/if_mds_api_datasource~header-ds_id ) TO r_properties.
  ENDMETHOD.

ENDCLASS.
