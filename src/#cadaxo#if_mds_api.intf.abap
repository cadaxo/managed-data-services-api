INTERFACE /cadaxo/if_mds_api
  PUBLIC .
  TYPES: BEGIN OF ty_relation,
           to TYPE /cadaxo/mds_ds_id.
  INCLUDE TYPE /cadaxo/mds_ds_semkey AS semkey.
  TYPES:
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
    END OF ty_datasource,
    ty_datasources TYPE STANDARD TABLE OF ty_datasource WITH DEFAULT KEY.

  METHODS get_datasources_by_semkey IMPORTING i_ds_semkey          TYPE /cadaxo/mds_ds_semkey
                                    RETURNING VALUE(r_datasources) TYPE ty_datasources.
  METHODS get_datasources_by_id IMPORTING i_ds_id              TYPE /cadaxo/mds_ds_id
                                RETURNING VALUE(r_datasources) TYPE ty_datasources.
ENDINTERFACE.
