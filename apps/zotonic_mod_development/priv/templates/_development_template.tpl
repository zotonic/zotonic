<table class="table table-striped">
	<tr>
		<th>{_ Module _}</th>
		<th>{_ Template path _}</th>
		<th></th>
	</tr>
	<tr>
		{% if tpl %}
			<td>{{ tpl.module|escape }}</td>
			<td><small><tt>{{ tpl.path|escape }}</tt></small></td>
			<td>
				{% button text=_"View template"
						  class="btn btn-primary btn-xs"
						  postback={template_view template=tpl.path}
			              delegate=`mod_development`
				%}
			</td>
		{% else %}
			<td colspan="3">
				<em>{_ Not found _}</em>
			</td>
		{% endif %}
	</tr>
</table>
