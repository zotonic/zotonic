<table class="navbar navbar-top" width="100%">
	<tr>
		<td>
			<a class="brand" href="/" title="{_ Home _}">{{ m.config.site.title.value }}</a>
		</td>
		<td align="right">
			{% include "_language_switch.tpl" %}
		</td>
		<td align="right">
			<a class="search" href="{% url search %}">{_ Search _}</a>
		</td>
	</tr>
	<tr>
		<td colspan="3">
			{% menu id=id %}
		<td>
	</tr>
</table>
