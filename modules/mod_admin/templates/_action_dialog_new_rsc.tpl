<script type="text/javascript">
	$(function()
	{
		$('.do_fieldreplace').fieldreplace();
	});
</script>
<p>Please fill in the title and the category of the new page.  You also have to select the group you will share the page with.</p>

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
			<select id="{{ #category }}" name="category_id">
			{% for cat_id, level, indent, name in m.category.all_flat %}
				<option value="{{cat_id}}" {% ifequal cat_id cat %}selected="selected" {% endifequal %}>
					{{ indent }}{{ m.rsc[cat_id].title|default:name }}
				</option>
			{% endfor %}
			</select>
		</div>

		<div class="form-item clearfix">
			<input type="checkbox" id="{{ #published }}" name="is_published" value="1" {% if subject_id %}checked="checked"{% endif %} />
			<label for="{{ #published }}" class="left">Published</label>
		</div>
		
		<div class="form-item clearfix">
			<label for="{{ #group_id }}">Group</label>
			<select id="{{ #group_id }}" name="group_id">
			{% for group_id in m.acl.member %}
				<option value="{{ group_id }}">{{ m.rsc[group_id].title }}</option>
			{% endfor %}
			</select>
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

