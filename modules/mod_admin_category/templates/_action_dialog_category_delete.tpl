
{% if page_count %}
	<p>
		There are {{ page_count }} pages with the category “{{ m.rsc[id].title }}”, which you want to delete.<br/>
		Please specify to which category you want to transfer those pages.
	</p>
{% else %}
	<p>
		Are you sure you want to delete the category “{{ m.rsc[id].title }}”?
	</p>
{% endif %}

<p>This action can't be undone, the category will be lost forever.</p>

{% wire id=#form type="submit" postback={delete_category on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="id" value="{{ id }}" />

	{% if page_count %}
		<div class="form-item clearfix">
			<label for="{{ #transfer_id }}">Transfer to</label>
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
		{% button text="Delete" %}
		{% button text="Cancel" action={dialog_close} %}
	</div>

</form>

