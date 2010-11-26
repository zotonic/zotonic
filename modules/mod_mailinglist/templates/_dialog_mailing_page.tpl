
<p>{_ Select a mailing list to send the page _} {{ m.rsc[id].title }} {_ to: _}</p>

{% wire type="submit" id=#form postback={mailing_page id=id on_success=on_success} action={dialog_close} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">
	<select name="mailinglist_id">
		<option value="" disabled="disabled">{_ Select mailing list _}</option>
		{% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
			{% ifnotequal m.rsc[id].name "mailinglist_test" %}
				<option value="{{ id }}" {% if not m.rsc[id].is_editable %}disabled="disabled"{% endif %}>{{ title }}</option>
			{% endifnotequal %}
		{% endfor %}
	</select>
	
	<div class="clear">&nbsp;</div>

	{% if not m.rsc[id].is_published %}
		<p>{_ The mailing will be send after this page has been published. _}</p>
	{% else %}
		{% if m.rsc[id].publication_start|in_future %}
			<p>{_ The mailing will be send after the publication start date of _} {{ m.rsc[id].publication_start|date:"d M Y, H:i" }}.</p>
		{% endif %}
	{% endif %}
	
	{% button text=_"Send mailing" %}
	{% button text=_"Cancel" action={dialog_close} %}
</form>
