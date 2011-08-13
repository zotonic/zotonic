
{% if page_count %}
	<p>
		{_ There are _} {{ page_count }} {_ pages with the category _} “{{ m.rsc[id].title }}”, {_ which you want to delete. _}<br/>{_ Please specify to which category you want to transfer those pages. _}
	</p>
{% else %}
	<p>
		{_ Are you sure you want to delete the category _} “{{ m.rsc[id].title }}”?
	</p>
{% endif %}

<p>{_ This action can't be undone, the category will be lost forever. _}</p>

{% wire id=#form type="submit" postback={delete_category on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />

	{% if page_count %}
		<div class="form-item clearfix">
			<label for="{{ #transfer_id }}">{_ Transfer to _}</label>
			<select id="{{ #transfer_id }}" name="transfer_id">
				<option value=""></option>
				{% for cat_id, level, indent, name in m.category.all_flat %}
					<option value="{{cat_id}}" {% if cat_id == id %}disabled="disabled" {% endif %}>
						{{ indent }}{{ m.rsc[cat_id].title|default:name }}
					</option>
				{% endfor %}
			</select>
			{% validate id=#transfer_id  type={presence} %}
		</div>
	{% endif %}

	<div class="form-item clearfix">
		{% button text=_"Delete" %}
		{% button text=_"Cancel" action={dialog_close} %}
	</div>

</form>

