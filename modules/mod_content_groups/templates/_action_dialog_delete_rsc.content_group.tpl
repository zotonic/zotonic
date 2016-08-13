<p>{_ Are you sure you want to delete the following content groups: _}</p>

<ul>
	{% for cg in m.hierarchy.content_group[id].tree_flat %}
		<li>{{ cg.id.title }}</li>
	{% endfor %}
</ul>

{% if m.content_group.is_used[id] %}
	{% wire id=#menu_delete type="submit"
			postback={delete_move id=id}
			delegate=`mod_content_groups`
	%}
	<form id="{{ #menu_delete }}" action="postback" type="post">
		<p>{_ Move all pages in these content groups to: _}</p>

		<div class="form-group">
			<select class="form-control" name="content_group_id" id="{{ #cgid }}">
				<option></option>
				{% for cg in m.hierarchy.content_group.tree_flat %}
					{% if cg.id /= id and not id|member:cg.path %}
						<option value="{{ cg.id }}">
							{{ cg.indent }} {{ cg.id.title }}
						</option>
					{% endif %}
				{% endfor %}
			</select>
			{% validate id=#cgid name="content_group_id" type={presence} %}
		</div>

		<p>{_ Or click <strong>Delete All Pages</strong> to delete all pages in the content groups. _}</p>

		<p>{_ This can not be undone. _}</p>

		<div class="modal-footer">
			{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
			{% button type="submit" class="btn btn-primary" text=_"Delete &amp; Move Pages" %}
			{% button tag="a"
					  class="btn btn-danger pull-left"
					  text=_"Delete All Pages"
					  postback={delete_all id=id}
					  delegate=`mod_content_groups`
			%}
		</div>
	</form>
{% else %}
	<p>{_ There are no pages in these content groups. _}</p>

	<div class="modal-footer">
		{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
		{% button class="btn btn-primary" text=_"Delete"
				postback={delete_all id=id if_empty}
				delegate=`mod_content_groups`
		%}
	</div>
{% endif %}
