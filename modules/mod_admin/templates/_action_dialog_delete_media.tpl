{# Used by the action dialog_delete_media #}
{% with m.media[id] as media %}
	<p>Are you sure you want to delete the {{ media.mime }} media from “{{ m.rsc[id].title }}”?</p>

	<p>This can't be undone. Your media will be lost forever.</p>
{% endwith %}

{% button text="Delete" action={delete_media id=id on_success=on_success} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
