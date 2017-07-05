<div class="confirm">
    {% if is_template %}{{ text }}{% else %}<p>{{ text }}</p>{% endif %}
</div>

<div class="modal-footer">
    {% if is_danger %}
        {% button class="btn btn-default" text=cancel|default:_"Cancel" action={dialog_close} action=on_cancel delegate=delegate %}
        {% button class="btn btn-danger" text=ok|default:_"OK" action={dialog_close} delegate=delegate postback=postback action=action tag="a" %}
    {% else %}
        {% button class="btn btn-default" text=cancel|default:_"Cancel" action={dialog_close} action=on_cancel delegate=delegate tag="a" %}
        {% button class="btn btn-primary" text=ok|default:_"OK" action={dialog_close} delegate=delegate postback=postback action=action %}
    {% endif %}
</div>
