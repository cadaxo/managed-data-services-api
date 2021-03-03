CLASS /cadaxo/cl_mds_search_api DEFINITION INHERITING FROM /cadaxo/cl_mds_api
  PUBLIC
  FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.
    CLASS-METHODS get_search_instance RETURNING VALUE(e_search_api) TYPE REF TO /cadaxo/if_mds_api.

  PROTECTED SECTION.
    METHODS get_ds_reader REDEFINITION.

ENDCLASS.


CLASS /cadaxo/cl_mds_search_api IMPLEMENTATION.

  METHOD get_search_instance.

    e_search_api = NEW /cadaxo/cl_mds_search_api( ).

  ENDMETHOD.

  METHOD get_ds_reader.

    r_ds_reader = /cadaxo/cl_mds_search_api_ds=>get_ds_search_instance( i_ds_id ).

  ENDMETHOD.

ENDCLASS.
