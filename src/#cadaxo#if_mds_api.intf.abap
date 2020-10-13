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
    object_state  TYPE int4,
    END OF ty_annotation,
    ty_annotations TYPE STANDARD TABLE OF ty_annotation WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_parameter.
  INCLUDE TYPE /cadaxo/mds_pr_semkey AS semkey.
  TYPES:
    parameter_id TYPE /cadaxo/mds_parameter_id,
    position     TYPE int4,
    value        TYPE char100,
    object_state TYPE int4,
    END OF ty_parameter,
    ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH DEFAULT KEY.

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
    END OF ty_datasource,
    ty_datasources TYPE STANDARD TABLE OF ty_datasource WITH DEFAULT KEY.

  METHODS get_datasources_by_semkey IMPORTING i_ds_semkey          TYPE /cadaxo/mds_ds_semkey
                                              i_read_depth         TYPE i
                                    RETURNING VALUE(r_datasources) TYPE ty_datasources.
  METHODS get_datasources_by_id IMPORTING i_ds_id              TYPE /cadaxo/mds_ds_id
                                          i_read_depth         TYPE i
                                RETURNING VALUE(r_datasources) TYPE ty_datasources.
  METHODS get_datasource_by_id IMPORTING i_ds_id             TYPE /cadaxo/mds_ds_id
                               RETURNING VALUE(r_datasource) TYPE ty_datasource.
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
  METHODS get_parameters_by_fieldid IMPORTING i_field_id          TYPE /cadaxo/mds_field_id
                                    RETURNING VALUE(r_parameters) TYPE ty_parameters.
  METHODS get_parameter_by_id IMPORTING i_parameter_id     TYPE /cadaxo/mds_parameter_id
                              RETURNING VALUE(r_parameter) TYPE ty_parameter.
ENDINTERFACE.
