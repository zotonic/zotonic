<p class="text-warning">{{ text }}</p>

<div class="modal-footer">
	{% button class="btn btn-primary" text=button|default:_"OK" action={dialog_close} action=action %}
</div>
