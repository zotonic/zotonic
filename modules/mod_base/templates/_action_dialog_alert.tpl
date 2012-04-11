<div class="alert">
    <p>{{ text }}</p>
</div>

<div class="modal-footer">
    <button class="btn-primary" id="{{ #cancel }}">{{ button|default:_"OK" }}</button>
    {% wire id=#cancel 
        action={dialog_close}
	action=action
    %}
</div>
