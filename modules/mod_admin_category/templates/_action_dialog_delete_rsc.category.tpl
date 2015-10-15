<p>{_ Are you sure you want to delete the following categories: _}</p>

<ul>
{% for cg in m.category[id].tree_flat %}
	{% if cg.id.exists %}
		<li>{{ cg.id.title }}</li>
	{% endif %}
{% endfor %}
</ul>

{% if m.category.is_used[id] %}
	{% wire id=#menu_delete type="submit"
			postback={delete_move id=id}
			delegate=`mod_admin_category`
	%}
	<form id="{{ #menu_delete }}" action="postback" type="post">
		<p>{_ Move all pages in these categories to: _}</p>

		<div class="form-group">
			<select class="form-control" name="category_id" id="{{ #cgid }}">
				<option></option>
				{% for cg in m.category.tree_flat_meta %}
					{% if cg.id /= id and not id|member:cg.path and cg.id.exists %}
						<option value="{{ cg.id }}">
							{{ cg.indent }} {{ cg.id.title }}
						</option>
					{% endif %}
				{% endfor %}
			</select>
			{% validate id=#cgid name="category_id" type={presence} %}
		</div>

		<p>{_ Or click <strong>Delete All Pages</strong> to delete all pages in the categories. _}</p>

		<p>{_ This can not be undone. _}</p>

		<div class="modal-footer">
			{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
			{% button type="submit" class="btn btn-primary" text=_"Delete &amp; Move Pages" %}
			{% button tag="a" 
					  class="btn btn-danger pull-left" 
					  text=_"Delete All Pages"
					  postback={delete_all id=id}
					  delegate=`mod_admin_category`
			%}
		</div>
	</form>
{% else %}
	<p>{_ There are no pages in these categories. _}</p>

	<div class="modal-footer">
		{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
		{% button class="btn btn-primary" text=_"Delete" 
				postback={delete_all id=id if_empty}
				delegate=`mod_admin_category`
		%}
	</div>
{% endif %}
