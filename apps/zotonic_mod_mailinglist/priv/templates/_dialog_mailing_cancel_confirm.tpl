<p>{% trans "Are you sure you want to cancel the mailing to “{list}”?" list=m.rsc[list_id].title|escape %}</p>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Keep mailing" action={dialog_close} %}
    {% button class="btn btn-primary" text=_"Cancel mailing" postback={mailing_cancel list_id=list_id page_id=page_id} action={dialog_close} %}
</div>
