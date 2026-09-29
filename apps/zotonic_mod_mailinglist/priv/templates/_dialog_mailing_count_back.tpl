<div class="modal-footer">
    {% button class="btn btn-default" text=_"Back"
        postback={mailing_back count_pid=count_pid id=id list_id=list_id options=options mail_when=mail_when mailing_date=mailing_date mailing_time=mailing_time on_success=on_success}
        delegate="action_mailinglist_dialog_mailing_page"
    %}
</div>
