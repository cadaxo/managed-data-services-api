INTERFACE /cadaxo/if_mds_api_datasource
  PUBLIC .
  CONSTANTS: BEGIN OF types,
               datadefinition TYPE string VALUE 'DDLS',
               table          TYPE string VALUE 'TABL',
             END OF types.
  METHODS build_related_entities.
  METHODS get_relations RETURNING VALUE(r_relations) TYPE /cadaxo/if_mds_api=>ty_relations.
  METHODS get_datasource RETURNING VALUE(r_datasource) TYPE /cadaxo/if_mds_api=>ty_datasource.


  DATA relations TYPE /cadaxo/if_mds_api=>ty_relations.
  DATA header TYPE /cadaxo/if_mds_api=>ty_datasource.
ENDINTERFACE.
