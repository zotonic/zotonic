<table class="table table-striped">
	<tr>
		<th>{_ Module _}</th>
		<th>{_ Template path _}</th>
		<th></th>
	</tr>
	<tr>
		{% if tpl %}
			<td>{{ tpl.module|escape }}</td>
			<td><small><tt>{{ tpl.path|development_filename|escape }}</tt></small></td>
			<td>
				{% button text=_"View"
						  class="btn btn-primary btn-xs"
						  postback={template_view template=tpl.path}
			              delegate=`mod_development`
				%}
                {% include "_development_editor_button.tpl" editor_file=tpl.path editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
			</td>
		{% else %}
			<td colspan="3">
				<em>{_ Not found _}</em>
                {% include "_development_editor_button.tpl" editor_file=tpl.path editor_label=_"Edit" editor_compact editor_title=_"Edit template" %}
			</td>
		{% endif %}
	</tr>
</table>
