<p class="text-warning">{{ text }}</p>

{% if not only_text %}
<div class="modal-footer">
	{% button class="btn btn-primary" text=button|default:_"OK" action={dialog_close} action=action postback=postback delegate=delegate %}
</div>
{% endif %}
