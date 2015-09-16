{% include "_email_status_view.tpl" id=id email=email %}

<div class="modal-footer">
	<a href="#cancel" id="{{ #cancel }}" class="btn btn-default">{_ Close _}</a>
</div>
{% wire id=#cancel action={dialog_close} %}
