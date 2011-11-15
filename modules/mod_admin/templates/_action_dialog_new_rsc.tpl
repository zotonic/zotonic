<script type="text/javascript">
	$(function()
	{
		$('.do_fieldreplace').fieldreplace();
	});
</script>
<p>{_ Please fill in the title _} {% if not nocatselect %}{_ and the category of the new page._}{% else %}{_ of the new _} {{ catname }}.{% endif %} </p>

{% wire id=#form type="submit" 
	postback={new_page subject_id=subject_id predicate=predicate redirect=redirect edge_template=edge_template} 
	delegate=delegate 
%}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-rsc-wrapper">
		<div class="form-item clearfix">
			<label for="new_rsc_title">{_ Page title _}</label>
			<input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" />
			{% validate id="new_rsc_title" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<label for="{{ #category }}">{_ Category _}</label>
			{% if cat and nocatselect %}
				<strong>{{ m.rsc[cat].title }}</strong>
				<input type="hidden" name="category_id" value="{{ cat }}"/>
			{% else %}
				<select id="{{ #category }}" name="category_id">
				{% for cat_id, level, indent, name in m.category.all_flat %}
					{% if m.acl.insert[name|as_atom] %}
						<option value="{{cat_id}}" {% ifequal cat_id cat %}selected="selected" {% endifequal %}>
							{{ indent }}{{ m.rsc[cat_id].title|default:name }}
						</option>
					{% endif %}
				{% endfor %}
				</select>
			{% endif %}
		</div>

		<div class="form-item clearfix">
			<input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if subject_id %}checked="checked"{% endif %} />
			<label for="{{ #published }}" class="left">{_ Published _}</label>
		</div>
		
		<div class="form-item clearfix">
			<button type="submit">{_ Make _} {{ catname }}</button>
			{% button action={dialog_close} text=_"Cancel" %}
		</div>
	</div>
</form>

