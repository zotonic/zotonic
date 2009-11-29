
<p>Select a mailing list to send the page {{ m.rsc[id].title }} to:</p>

{% wire type="submit" id=#form postback={mailing_page id=id on_success=on_success} action={dialog_close} delegate=delegate %}
<form id="{{ #form }}" method="post" action="postback">
	<select name="mailinglist_id">
		<option value="" disabled="disabled">Select mailing list</option>
		{% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
			{% ifnotequal m.rsc[id].name "mailinglist_test" %}
				<option value="{{ id }}" {% if not m.rsc[id].is_editable %}disabled="disabled"{% endif %}>{{ title }}</option>
			{% endifnotequal %}
		{% endfor %}
	</select>
	
	<div class="clear">&nbsp;</div>

	{% if not m.rsc[id].is_published %}
		<p>The mailing will be send after this page has been published.</p>
	{% else %}
		{% if m.rsc[id].publication_start|in_future %}
			<p>The mailing will be send after the publication start date of {{ m.rsc[id].publication_start|date:"d M Y, H:i" }}.</p>
		{% endif %}
	{% endif %}
	
	{% button text="Send mailing" %}
	{% button text="Cancel" action={dialog_close} %}
</form>
