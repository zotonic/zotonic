{# Used by the action dialog_delete_media #}
{% with m.media[id] as media %}
	<p>{_ Are you sure you want to delete the _} {{ media.mime }} {_ media from _} “{{ m.rsc[id].title }}”?</p>

	<p>{_ This can't be undone. Your media will be lost forever. _}</p>
{% endwith %}

{% button text=_"Delete" action={delete_media id=id on_success=on_success} action={dialog_close} %}
{% button text=_"Cancel" action={dialog_close} %}
