CLASS /cadaxo/cl_mds_api_ds DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES /cadaxo/if_mds_api_datasource.

    CLASS-METHODS get_instance IMPORTING i_object_name     TYPE /cadaxo/mds_object_name
                                         i_ds_classname    TYPE string
                               RETURNING VALUE(e_instance) TYPE REF TO /cadaxo/if_mds_api_datasource.
    METHODS constructor IMPORTING i_object_name TYPE /cadaxo/mds_object_name.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_instance,
             name     TYPE /cadaxo/mds_object_name,
             instance TYPE REF TO /cadaxo/if_mds_api_datasource,
           END OF ty_instance,
           ty_instances TYPE SORTED TABLE OF ty_instance WITH UNIQUE KEY name.
    CLASS-DATA instances TYPE ty_instances.

    CONSTANTS: BEGIN OF version,
                 active TYPE as4local VALUE 'A',
               END OF version.

ENDCLASS.


CLASS /cadaxo/cl_mds_api_ds IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD get_instance.

    DATA: ds_instance TYPE REF TO /cadaxo/if_mds_api_datasource.

    IF NOT line_exists( instances[ name = i_object_name ] ).

      CREATE OBJECT ds_instance TYPE (i_ds_classname) EXPORTING i_object_name = i_object_name.

      INSERT VALUE #( name     = i_object_name
                      instance = ds_instance ) INTO TABLE instances ASSIGNING FIELD-SYMBOL(<instance>).
    ELSE.
      ASSIGN instances[ name = i_object_name ] TO <instance>.
    ENDIF.

    e_instance = <instance>-instance.

  ENDMETHOD.


  METHOD /cadaxo/if_mds_api_datasource~get_relations.
    r_relations = me->/cadaxo/if_mds_api_datasource~relations.
  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~get_datasource.
    r_datasource = me->/cadaxo/if_mds_api_datasource~header.
  ENDMETHOD.

  METHOD /cadaxo/if_mds_api_datasource~build_related_entities.

  ENDMETHOD.

ENDCLASS.
