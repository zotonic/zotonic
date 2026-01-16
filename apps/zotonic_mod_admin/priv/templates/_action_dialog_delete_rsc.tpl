{# Used by the action dialog_delete_rsc #}
<p>{_ Are you sure you want to delete the page _}: <b>{{ m.rsc[id].title|default:m.rsc[id].short_title|default:_"Untitled" }}</b></p>

{% if not m.modules.active.mod_backup %}
    <p>{_ This can't be undone. Your page will be lost forever. _}</p>
{% endif %}

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
    {% button class="btn btn-danger" text=_"Delete" type="submit" action={delete_rsc id=id on_success=on_success} action={dialog_close} %}
</div>
