<div class="confirm">
    {% if is_template %}{{ text }}{% else %}<p>{{ text }}</p>{% endif %}
</div>

<div class="modal-footer">
	{% button class="btn" text=cancel|default:_"Cancel" action={dialog_close} action=on_cancel delegate=delegate tag="a" %}
	{% button class="btn btn-primary" text=ok|default:_"OK" action={dialog_close} delegate=delegate postback=postback action=action %}
</div>
