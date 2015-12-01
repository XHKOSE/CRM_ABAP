
METHOD if_ex_exec_methodcall_ppf~execute .
*---------------------------------------------------------------------*
*       METHOD if_ex_exec_methodcall_ppf~execute                      *
*---------------------------------------------------------------------*
*  Triggering alerts for specific recipients                          *
*---------------------------------------------------------------------*

  INCLUDE crm_object_names_con. " Constants for object names
  INCLUDE crm_object_kinds_con. " Constants for kinds

  DATA  lc_action_execute     TYPE REF TO cl_action_execute.
  DATA  lv_message            TYPE bapi_msg.
  DATA  lv_header_guid        TYPE crmt_object_guid.
  DATA  lv_kind_ref           TYPE crmt_object_kind.
  DATA  ext_container         TYPE swconttab.
  DATA  exs_container         TYPE swcont.
  DATA  lt_container          TYPE STANDARD TABLE OF swcont.
  DATA  exs_but000            TYPE but000.
  DATA  exv_username          TYPE syuname.
  DATA  exv_url               TYPE agr_url.

  DATA: imt_recipients TYPE STANDARD TABLE OF salrtsrcp,
        ims_recipients TYPE salrtsrcp,
        imt_activities TYPE STANDARD TABLE OF salrtsact,
        ims_activities TYPE salrtsact,
        exp_alert_id   TYPE salrtextid,
        imv_category   TYPE salrtdcat.

  DATA: imt_header_guid TYPE crmt_object_guid_tab,
        imt_req_objects TYPE crmt_object_name_tab,
        ext_orderadm_h  TYPE crmt_orderadm_h_wrkt,
        ext_partner     TYPE crmt_partner_external_wrkt,
        exs_partner     TYPE crmt_partner_external_wrk,
        exs_orderadm_h  TYPE crmt_orderadm_h_wrk,
        cv_log_handle   TYPE balloghndl.

*--------------------------------------------------------------------*
* Macro: Seting elements in alert content
*--------------------------------------------------------------------*

  DEFINE set_element.

    call function 'SWC_ELEMENT_SET' " Note 821188
    exporting
      element             = &1
      field               = &2
    tables
      container           = lt_container
   exceptions
     type_conflict        = 1
     others               = 2.

    if sy-subrc = 0.

*   Update application log

      message i046(zcrm) with &1 into lv_message.

      cl_log_ppf=>add_message( ip_handle       = ip_application_log
                               ip_problemclass = sppf_pclass_3 ).

    else.

*   Update application log

      message e047(zcrm) with &1 into lv_message.

      cl_log_ppf=>add_message( ip_handle       = ip_application_log
                               ip_problemclass = sppf_pclass_3 ).

    endif.

  END-OF-DEFINITION.

*--------------------------------------------------------------------*
* Get header GUID of the related business transactioh
*--------------------------------------------------------------------*

  CREATE OBJECT lc_action_execute.

  IF lc_action_execute IS BOUND.

    CLEAR: lv_header_guid, lv_kind_ref, ext_container.

    CALL METHOD lc_action_execute->get_ref_object
      EXPORTING
        io_appl_object = io_appl_object
        ip_action      = ip_action
        ip_preview     = ip_preview
        ii_container   = ii_container
      IMPORTING
        ev_guid_ref    = lv_header_guid
        ev_kind_ref    = lv_kind_ref
        et_container   = ext_container.

*--------------------------------------------------------------------*
* Read transaction header details and partners
*--------------------------------------------------------------------*

*   Update application log

    MESSAGE i049(zcrm) INTO lv_message.

    cl_log_ppf=>add_message( ip_handle       = ip_application_log
                             ip_problemclass = sppf_pclass_4 ).


    CLEAR: imt_header_guid, imt_req_objects, ext_orderadm_h,
           cv_log_handle, ext_partner.

    INSERT lv_header_guid INTO TABLE imt_header_guid.

    INSERT gc_object_name-partner    INTO TABLE imt_req_objects.
    INSERT gc_object_name-orderadm_h INTO TABLE imt_req_objects.

    CALL FUNCTION 'CRM_ORDER_READ'
      EXPORTING
        it_header_guid       = imt_header_guid
        it_requested_objects = imt_req_objects
      IMPORTING
        et_orderadm_h        = ext_orderadm_h
        et_partner           = ext_partner
      CHANGING
        cv_log_handle        = cv_log_handle
      EXCEPTIONS
        document_not_found   = 1
        error_occurred       = 2
        document_locked      = 3
        no_change_authority  = 4
        no_display_authority = 5
        no_change_allowed    = 6
        OTHERS               = 7.

    IF sy-subrc <> 0.

*   Update application log

      MESSAGE e048(zcrm) INTO lv_message.

      cl_log_ppf=>add_message( ip_handle       = ip_application_log
                               ip_problemclass = sppf_pclass_1 ).

*   Set rp_status (result)

      rp_status = sppf_status_error.

      RETURN.

    ELSE.

*--------------------------------------------------------------------*
* Filling containers in alert content (a.k.a container elements)
*--------------------------------------------------------------------*

*--> Business transaction ID (OBJECT_ID)

      CLEAR exs_orderadm_h.

      READ TABLE ext_orderadm_h INTO exs_orderadm_h INDEX 1
                                TRANSPORTING guid object_id.

      IF sy-subrc = 0.

*   Fill container element (OBJECT_ID)

        set_element: 'OBJECT_ID' exs_orderadm_h-object_id. " Note 821188

      ELSE.

*   Update application log

        MESSAGE e050(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_3 ).

      ENDIF.

*--> Specific partner's first name and last name (MC_NAME1, MC_NAME2)

      CLEAR exs_partner.

      READ TABLE ext_partner INTO exs_partner
                             WITH KEY partner_fct = 'AGENT_PARTNER_FCT' " Specific partner function
                                      ref_kind    = gc_object_kind-orderadm_h
                             TRANSPORTING bp_partner_guid.

      IF sy-subrc = 0.

*   Obtain business partner's first and last name

        CLEAR exs_but000.

        CALL FUNCTION 'BUPA_NUMBERS_GET'
          EXPORTING
            iv_partner_guid = exs_partner-bp_partner_guid
          IMPORTING
            es_but000       = exs_but000.

        IF exs_but000 IS NOT INITIAL.

*   Fill container elements (MC_NAME1, MC_NAME2)

          set_element: 'MC_NAME1' exs_but000-mc_name1. " Note 821188
          set_element: 'MC_NAME2' exs_but000-mc_name2. " Note 821188

        ELSE.

*   Update application log

          MESSAGE e052(zcrm) INTO lv_message.

          cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                   ip_problemclass = sppf_pclass_3 ).

        ENDIF.

*--------------------------------------------------------------------*
* Recipient determination
*--------------------------------------------------------------------*

*   Update application log

        MESSAGE i053(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_4 ).

        CLEAR exv_username.

        CALL FUNCTION 'BP_CENTRALPERSON_GET'
          EXPORTING
            iv_bu_partner_guid  = exs_partner-bp_partner_guid
          IMPORTING
            ev_username         = exv_username
          EXCEPTIONS
            no_central_person   = 1
            no_business_partner = 2
            no_id               = 3
            OTHERS              = 4.

        IF sy-subrc <> 0 OR exv_username IS INITIAL.

*   Update application log

          MESSAGE e054(zcrm) INTO lv_message.

          cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                   ip_problemclass = sppf_pclass_1 ).

*   Set rp_status (result)

          rp_status = sppf_status_error.

          RETURN.

        ELSE.

*   Set recipients

          CLEAR: ims_recipients,
                 imt_recipients.

          ims_recipients-uname = exv_username.

          APPEND ims_recipients TO imt_recipients.

*   Update application log

          MESSAGE i055(zcrm) WITH exv_username INTO lv_message.

          cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                   ip_problemclass = sppf_pclass_4 ).

        ENDIF.

      ELSE.

*   Update application log

        MESSAGE e051(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_1 ).

*   Set rp_status (result)

        rp_status = sppf_status_error.

        RETURN.

      ENDIF.

*--------------------------------------------------------------------*
* Acquiring Web UI navigation link for "activities" in alert
*--------------------------------------------------------------------*

      CLEAR exv_url.

      CALL FUNCTION 'ZCRM_WEBUI_URL_BUILD'
        EXPORTING
          iv_header_guid    = exs_orderadm_h-guid
        IMPORTING
          ev_url            = exv_url
        EXCEPTIONS
          read_order_failed = 1
          OTHERS            = 2.

      IF sy-subrc = 0 AND exv_url IS NOT INITIAL.

        CLEAR: ims_activities, imt_activities.

        ims_activities-acttext = exs_orderadm_h-object_id.
        ims_activities-acturl  = exv_url.
        ims_activities-urltype = '1'.

        APPEND ims_activities TO imt_activities.

*   Update application log

        MESSAGE i056(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_4 ).

      ELSE.

*   Update application log

        MESSAGE e058(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_3 ).


      ENDIF.

*--------------------------------------------------------------------*
* Generating alert
*--------------------------------------------------------------------*

*--> Alert category

      CLEAR exs_container.

      READ TABLE ext_container INTO exs_container
                               WITH KEY element = 'CATEGORY'
                               TRANSPORTING value.

      IF sy-subrc = 0.

        CLEAR: exp_alert_id, imv_category.

        imv_category = exs_container-value.

        CALL FUNCTION 'SALRT_CREATE_API' " Note 821188
          EXPORTING
            ip_category            = imv_category
            ip_wait_on_commit      = sppf_true
          IMPORTING
            ep_alert_id            = exp_alert_id
          TABLES
            it_recipients          = imt_recipients
            it_activities          = imt_activities
            it_container           = lt_container
          EXCEPTIONS
            alert_category_unknown = 1
            alert_no_recipients    = 2
            alert_error_unknown    = 3
            destination_undefined  = 4
            communication_failure  = 5
            system_failure         = 6
            OTHERS                 = 7.

        IF sy-subrc <> 0.

*   Update application log

          cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                   ip_problemclass = sppf_pclass_1 ).
*   Set rp_status (result)

          rp_status = sppf_status_error.

          RETURN.

        ENDIF.

      ELSE.

*   Update application log

        MESSAGE e057(zcrm) INTO lv_message.

        cl_log_ppf=>add_message( ip_handle       = ip_application_log
                                 ip_problemclass = sppf_pclass_4 ).
*   Set rp_status (result)

        rp_status = sppf_status_error.

        RETURN.

      ENDIF.

*--------------------------------------------------------------------*
* Save document
*--------------------------------------------------------------------*

      CALL METHOD lc_action_execute->register_for_save
        EXPORTING
          iv_source_header_guid = lv_header_guid
          ip_application_log    = ip_application_log
        IMPORTING
          rp_status             = rp_status.

    ENDIF.

  ELSE.

*   Update application log

    MESSAGE e045(zcrm) INTO lv_message.

    cl_log_ppf=>add_message( ip_handle       = ip_application_log
                             ip_problemclass = sppf_pclass_1 ).

*   Set rp_status (result)

    rp_status = sppf_status_error.

  ENDIF.

ENDMETHOD.
ENDCLASS.
