
<p>Please specify the title, unique name and optional parent of the new category.</p>

{% wire id=#form type="submit" postback={category_add on_success=on_success} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<div class="new-category-wrapper">

		<div class="form-item clearfix">
			<label for="{{ #title }}">Title</label>
			<input id="{{ #title }}" type="text" name="title" value="" />
		</div>
		{% validate id=#title name="title" type={presence} %}

		<div class="form-item clearfix">
			<label for="{{ #name }}">Name</label>
			<input id="{{ #name }}" type="text" name="name" value="" />
		</div>
		{% validate id=#name name="name" type={presence} %}

		<div class="form-item clearfix">
			<label for="{{ #category }}">Below category</label>
			<select id="{{ #category }}" name="category_id">
				<option value=""></option>
			{% for cat_id, level, indent, name in m.category.all_flat %}
				<option value="{{cat_id}}">
					{{ indent }}{{ m.rsc[cat_id].title|default:name }}
				</option>
			{% endfor %}
			</select>
		</div>

		<div class="form-item clearfix">
			<button type="submit">Make category</button>
			{% button text="Cancel" action={dialog_close} %}
		</div>

	</div>
</form>

