{% if m.predicate.is_used[id] %}
	{% wire id=#delete type="submit"
			postback={delete_move id=id}
			delegate=`mod_admin_predicate`
	%}
	<form id="{{ #delete }}" action="postback" type="post">
		<p>{_ Are you sure you want to delete _} “{{ id.title }}”?</p>

		<p>{_ Change the predicate on all existing page connections to: _}</p>

		<div class="form-group">
			<select class="form-control" name="predicate_id" id="{{ #predid }}">
				<option></option>
				{% for name,ps in m.predicate.all %}
					{% if ps.id /= id %}
						<option value="{{ ps.id }}">
							{{ ps.id.title }}
						</option>
					{% endif %}
				{% endfor %}
			</select>
			{% validate id=#predid name="predicate_id" type={presence} %}
		</div>

		<p>{_ Or click <strong>Delete All Connections</strong> to delete all connections with this predicate. _}</p>

		<p>{_ This can not be undone. _}</p>

		<div class="modal-footer">
			{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
			{% button type="submit" class="btn btn-primary" text=_"Delete &amp; Change Connections" %}
			{% button tag="a"
					  class="btn btn-danger pull-left"
					  text=_"Delete All Connections"
					  postback={delete_all id=id}
					  delegate=`mod_admin_predicate`
			%}
		</div>
	</form>
{% else %}
	<p>{_ Are you sure you want to delete _} “{{ id.title }}”?</p>

	<p>{_ There are no page connections with this predicate. _}</p>

	<div class="modal-footer">
		{% button tag="a" class="btn btn-default" text=_"Cancel" action={dialog_close} %}
		{% button class="btn btn-primary" text=_"Delete"
				postback={delete_all id=id if_empty}
				delegate=`mod_admin_predicate`
		%}
	</div>
{% endif %}
