<table class="table table-striped">
	<tr>
		<th>{_ Module _}</th>
		<th>{_ Template path _}</th>
	</tr>
	<tr>
		{% if tpl %}
			<td>{{ tpl.module|escape }}</td>
			<td><tt>{{ tpl.path|escape }}</tt></td>
		{% else %}
			<td colspan="2">
				<em>{_ Not found _}</em>
			</td>
		{% endif %}
	</tr>
</table>
