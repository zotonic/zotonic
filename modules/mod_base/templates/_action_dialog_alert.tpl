<div class="alert">
    <p>{{ text }}</p>
</div>

<div class="modal-footer">
	{% button class="btn-primary" text=button|default:_"OK" action={dialog_close} action=action %}
</div>
