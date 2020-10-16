INTERFACE /cadaxo/if_mds_api_datasource
  PUBLIC .
  CONSTANTS: BEGIN OF types,
               datadefinition TYPE string VALUE 'DDLS',
               table          TYPE string VALUE 'TABL',
             END OF types.
  METHODS build_related_entities.
  METHODS get_relations RETURNING VALUE(r_relations) TYPE /cadaxo/if_mds_api=>ty_relations.
  METHODS get_datasource RETURNING VALUE(r_datasource) TYPE /cadaxo/if_mds_api=>ty_datasource.
  METHODS get_fields RETURNING VALUE(r_fields) TYPE /cadaxo/if_mds_api=>ty_fields.
  METHODS get_annotations RETURNING VALUE(r_annotations) TYPE /cadaxo/if_mds_api=>ty_annotations.
  METHODS get_parameters RETURNING VALUE(r_parameters) TYPE /cadaxo/if_mds_api=>ty_parameters.
  METHODS get_action_links RETURNING VALUE(r_links_action) TYPE /cadaxo/if_mds_api=>ty_action_link.

  DATA header TYPE /cadaxo/if_mds_api=>ty_datasource READ-ONLY.
  DATA relations TYPE /cadaxo/if_mds_api=>ty_relations READ-ONLY.
  DATA fields TYPE /cadaxo/if_mds_api=>ty_fields READ-ONLY.
  DATA annotations TYPE /cadaxo/if_mds_api=>ty_annotations READ-ONLY.
  DATA parameters TYPE /cadaxo/if_mds_api=>ty_parameters READ-ONLY.

ENDINTERFACE.
