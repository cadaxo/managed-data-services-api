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
    CLASS-DATA id_handler TYPE REF TO /cadaxo/cl_mds_id.

    TYPES: BEGIN OF ty_instance,
             ds_id    TYPE /cadaxo/mds_ds_id,
             instance TYPE REF TO /cadaxo/if_mds_api_datasource,
           END OF ty_instance,
           ty_instances TYPE SORTED TABLE OF ty_instance WITH UNIQUE KEY ds_id.
    CLASS-DATA instances TYPE ty_instances.

    CONSTANTS: BEGIN OF version,
                 active TYPE as4local VALUE 'A',
               END OF version.

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

      DATA(ds_class_name) = '/CADAXO/CL_MDS_API_' && semkey-type.
      CREATE OBJECT ds_instance TYPE (ds_class_name) EXPORTING i_sematic_key = semkey.

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

  ENDMETHOD.

ENDCLASS.
