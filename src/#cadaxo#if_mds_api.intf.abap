INTERFACE /cadaxo/if_mds_api
  PUBLIC .

  TYPES: BEGIN OF ty_field,
           field_id TYPE /cadaxo/mds_field_id,
           api      TYPE REF TO /cadaxo/if_mds_api_field,
         END OF ty_field,
         ty_fields TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_annotation.
  INCLUDE TYPE /cadaxo/mds_an_semkey AS semkey.
  TYPES:
    annotation_id TYPE /cadaxo/mds_annotation_id,
    position      TYPE int4,
    value         TYPE char100,
    object_state  TYPE /cadaxo/mds_object_state,
    END OF ty_annotation,
    ty_annotations TYPE STANDARD TABLE OF ty_annotation WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_parameter.
  INCLUDE TYPE /cadaxo/mds_pr_semkey AS semkey.
  TYPES:
    parameter_id TYPE /cadaxo/mds_parameter_id,
    position     TYPE int4,
    description  TYPE /cadaxo/mds_description,
    object_state TYPE /cadaxo/mds_object_state,
    datatype     TYPE datatype_d,
    length       TYPE ddleng,
    decimals     TYPE decimals,
    data_element TYPE rollname,
    END OF ty_parameter,
    ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_property.
  INCLUDE TYPE /cadaxo/mds_py_semkey AS semkey.
  TYPES:
    property_id  TYPE /cadaxo/mds_property_id,
    position     TYPE int4,
    description  TYPE /cadaxo/mds_description,
    object_state TYPE /cadaxo/mds_object_state,
    END OF ty_property,
    ty_properties TYPE STANDARD TABLE OF ty_property WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_relation.
  INCLUDE TYPE /cadaxo/mds_lk_semkey AS semkey.
  TYPES:
    link_id       TYPE /cadaxo/mds_link_id,
    relation_type TYPE string,
    cardinality   TYPE string,
    description   TYPE string,
    card_min      TYPE ddcardinality,
    card_max      TYPE ddcardinality,
    END OF ty_relation,
    ty_relations TYPE STANDARD TABLE OF ty_relation WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_datasource.
  INCLUDE TYPE /cadaxo/mds_ds_semkey AS semkey.
  TYPES:
    ds_id       TYPE /cadaxo/mds_ds_id,
    changed_by  TYPE as4user,
    changed_at  TYPE timestampl,
    description TYPE as4text,
    sqlviewname TYPE tabname,
    cs_name     TYPE /cadaxo/mds_object_name.
  INCLUDE TYPE /cadaxo/mds_field_search AS field_search.
  TYPES:
    depth TYPE i,
    role  TYPE int4,
    api   TYPE REF TO /cadaxo/if_mds_api_datasource,
    END OF ty_datasource,
    ty_datasources TYPE STANDARD TABLE OF ty_datasource WITH DEFAULT KEY.


  TYPES: BEGIN OF ty_action_link,
           display TYPE string,
           edit    TYPE string,
         END OF ty_action_link.

  TYPES: ty_ds_role TYPE i.

  TYPES: BEGIN OF ty_dashboard.
  TYPES:
    object_type  TYPE /CADAXO/MDS_OBJECT_TYPE,
    object_name  TYPE /CADAXO/MDS_OBJECT_NAME,
    count  TYPE /CADAXO/MDS_OBJECT_COUNT,
    total_count TYPE /CADAXO/MDS_OBJECT_COUNT,
    END OF ty_dashboard,
    ty_dashboard_t TYPE STANDARD TABLE OF ty_dashboard WITH DEFAULT KEY.



  CONSTANTS: BEGIN OF ds_role,
               main   TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE 0,
               parent TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE -1,
               child  TYPE /cadaxo/if_mds_api=>ty_ds_role VALUE 1,
             END OF ds_role.

  METHODS get_datasources_by_semkey IMPORTING i_ds_semkey          TYPE /cadaxo/mds_ds_semkey
                                              i_filter_fieldname   TYPE fieldname OPTIONAL
                                              i_filter_datasource  TYPE /cadaxo/mds_object_name OPTIONAL
                                    RETURNING VALUE(r_datasources) TYPE ty_datasources
                                    RAISING /cadaxo/cx_mds_id.
  METHODS get_datasources_by_id IMPORTING i_ds_id              TYPE /cadaxo/mds_ds_id
                                          i_as_role            TYPE ty_ds_role DEFAULT ds_role-main
                                          i_filter_fieldname   TYPE fieldname OPTIONAL
                                          i_filter_datasource  TYPE /cadaxo/mds_object_name OPTIONAL
                                RETURNING VALUE(r_datasources) TYPE ty_datasources
                                RAISING /cadaxo/cx_mds_id.
  METHODS get_datasource_by_id IMPORTING i_ds_id             TYPE /cadaxo/mds_ds_id
                               RETURNING VALUE(r_datasource) TYPE ty_datasource
                               RAISING /cadaxo/cx_mds_id.
  METHODS get_links_by_dsid IMPORTING i_ds_id            TYPE /cadaxo/mds_ds_id
                            RETURNING VALUE(r_relations) TYPE ty_relations.
  METHODS get_link_by_id IMPORTING i_link_id     TYPE /cadaxo/mds_link_id
                         RETURNING VALUE(r_link) TYPE ty_relation.
  METHODS get_fields_by_dsid IMPORTING i_ds_id         TYPE /cadaxo/mds_ds_id
                             RETURNING VALUE(r_fields) TYPE ty_fields.
  METHODS get_field_by_id IMPORTING i_field_id     TYPE /cadaxo/mds_field_id
                          RETURNING VALUE(r_field) TYPE ty_field.
  METHODS get_annotations_by_dsid IMPORTING i_ds_id              TYPE /cadaxo/mds_ds_id
                                  RETURNING VALUE(r_annotations) TYPE ty_annotations.
  METHODS get_annotations_by_fieldid IMPORTING i_field_id           TYPE /cadaxo/mds_field_id
                                     RETURNING VALUE(r_annotations) TYPE ty_annotations.
  METHODS get_annotation_by_id IMPORTING i_annotation_id     TYPE /cadaxo/mds_annotation_id
                               RETURNING VALUE(r_annotation) TYPE ty_annotation.
  METHODS get_parameters_by_dsid IMPORTING i_ds_id             TYPE /cadaxo/mds_ds_id
                                 RETURNING VALUE(r_parameters) TYPE ty_parameters.
  METHODS get_parameter_by_id IMPORTING i_parameter_id     TYPE /cadaxo/mds_parameter_id
                              RETURNING VALUE(r_parameter) TYPE ty_parameter.
  METHODS get_properties_by_dsid IMPORTING i_ds_id             TYPE /cadaxo/mds_ds_id
                                 RETURNING VALUE(r_properties) TYPE ty_properties.
  METHODS get_properties_by_fieldid IMPORTING i_field_id          TYPE /cadaxo/mds_field_id
                                    RETURNING VALUE(r_properties) TYPE ty_properties.
  METHODS get_property_by_id IMPORTING i_property_id     TYPE /cadaxo/mds_property_id
                             RETURNING VALUE(r_property) TYPE ty_property.
  METHODS get_dashboard_data IMPORTING i_custom_objects TYPE abap_bool
                                       i_last_week_data TYPE abap_bool
                             RETURNING VALUE(r_dashobard_data) TYPE ty_dashboard_t.

ENDINTERFACE.
