<div class="confirm">
    {% if is_template %}{{ text }}{% else %}<p>{{ text }}</p>{% endif %}
</div>

<div class="modal-footer">
    <button class="btn" id="{{ #cancel }}">{{ cancel|default:_"Cancel" }}</button>
    <button class="btn btn-primary" id="{{ #ok }}">{{ ok|default:_"OK" }}</button>
    {% wire id=#ok 
        action={dialog_close}
	action=action
	postback=postback
	delegate=delegate
    %}
    {% wire id=#cancel 
        action={dialog_close}
	action=on_cancel
    %}
</div>
