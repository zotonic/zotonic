<table class="table table-striped">
	<tr>
		<th>{_ User-Agent _}</th>
		<th>{_ Module _}</th>
		<th>{_ Template path _}</th>
	</tr>
	{% for ua_class, tpl in tpls %}
	<tr>
		<th>{{ ua_class }}</th>
		{% if tpl %}
			<td>{{ tpl.module|escape }}</td>
			<td><tt>{{ tpl.path|escape }}</tt></td>
		{% else %}
			<td colspan="2">
				<em>{_ Not found _}</em>
			</td>
		{% endif %}
	</tr>
	{% endfor %}
</table>
