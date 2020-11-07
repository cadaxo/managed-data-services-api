INTERFACE /cadaxo/if_mds_api_field
  PUBLIC .

  TYPES: BEGIN OF ty_data,
           field_alias            TYPE fieldname,
           base_table             TYPE tabname,
           base_field_name        TYPE fieldname,
           origin_field_name      TYPE ddfldorigin,
           origin_appendstru_name TYPE appname,
           position               TYPE ddfdpos,
           description            TYPE string,
           object_state           TYPE int4,
           datatype               TYPE datatype_d,
           length                 TYPE ddleng,
           decimals               TYPE decimals,
           data_element           TYPE rollname,
         END OF ty_data.

  TYPES: BEGIN OF ty_field.
  INCLUDE TYPE /cadaxo/mds_fd_semkey AS semkey.
  INCLUDE TYPE /cadaxo/if_mds_api_field=>ty_data AS data.
  TYPES: field_id TYPE /cadaxo/mds_field_id,
         END OF ty_field.

  METHODS get_id RETURNING VALUE(e_field_id) TYPE /cadaxo/mds_field_id.
  METHODS get_semantic_key RETURNING VALUE(e_semantic_key) TYPE /cadaxo/mds_fd_semkey.
  METHODS get_as_structure RETURNING VALUE(e_field) TYPE ty_field.

ENDINTERFACE.
