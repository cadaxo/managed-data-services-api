CLASS /CADAXO/CL_MDS_SEARCH_API_DS DEFINITION INHERITING FROM /cadaxo/cl_mds_api_ds
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.

    CLASS-METHODS get_ds_search_instance IMPORTING i_ds_id              TYPE /cadaxo/mds_ds_id
                                         RETURNING VALUE(e_ds_instance) TYPE REF TO /cadaxo/if_mds_api_datasource
                                         RAISING   /cadaxo/cx_mds_id.

  PROTECTED SECTION.

ENDCLASS.



CLASS /CADAXO/CL_MDS_SEARCH_API_DS IMPLEMENTATION.


  METHOD get_ds_search_instance.

    DATA(semkey) = id_handler->get_ds_semkey( i_ds_id ).

    DATA(ds_class_name) = '/CADAXO/CL_MDS_API_DS_' && semkey-type.


    CREATE OBJECT e_ds_instance TYPE (ds_class_name) EXPORTING i_sematic_key = semkey.

    IF e_ds_instance IS INITIAL.
      e_ds_instance->set_role( /cadaxo/if_mds_api=>ds_role-main ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
