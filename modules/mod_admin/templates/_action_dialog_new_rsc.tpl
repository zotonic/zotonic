<script type="text/javascript">
	$(function()
	{
		$('.do_fieldreplace').fieldreplace();
	});
</script>
<p>Please fill in the title {% if not nocatselect %}and the category of the new page.{% else %}of the new {{ catname }}.{% endif %} </p>

{% wire id=#form type="submit" postback="new_page" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-rsc-wrapper">
		<div class="form-item clearfix">
			<label for="new_rsc_title">Page title</label>
			<input type="text" id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" />
			{% validate id="new_rsc_title" type={presence} %}
		</div>

		<div class="form-item clearfix">
			<label for="{{ #category }}">Category</label>
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
			<label for="{{ #published }}" class="left">Published</label>
		</div>
		
		<div class="form-item clearfix">
			<button type="submit">Make {{catname}}</button>
			{% button action={dialog_close} text="Cancel" %}
		</div>
		
		<input type="hidden" name="subject_id" value="{{ subject_id }}" />
		<input type="hidden" name="predicate" value="{{ predicate }}" />
		<input type="hidden" name="redirect" value="{{ redirect }}" />
	</div>
</form>

