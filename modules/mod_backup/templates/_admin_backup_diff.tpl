{% with a.id|is_defined and b.id|is_defined as has_diff %}

{% if a.id and (a.id == b.id or b.id|is_undefined) %}
<p>
	{_ This is the version saved on _} <strong>{{ a.created|date:"Y-m-d H:i" }}</strong>
	{_ by _} <strong>{{ a.user_id.title|default:a.user_name }}</strong>
</p>
{% elseif a and b %}
<p>
	{_ Changes since _} <strong>{{ a.created|date:"Y-m-d H:i" }}</strong>
	{_ till _} <strong>{{ b.created|date:"Y-m-d H:i" }}</strong>
</p>
{% else %}
<p>
	{_ This is the version saved on _} <strong>{{ id.modified|date:"Y-m-d H:i" }}</strong>
	{_ by _} <strong>{{ id.modifier_id.title }}</strong>
</p>
{% endif %}

<table class="table">
{% if a.id and (a.id == b.id or b.id|is_undefined) %}
	<tr>
		<td></td>
		<td colspan="2">
			{% button
                text=[_"Revert to this version…"] 
                class="btn btn-danger" 
				action={confirm text=_"Are you sure you want to revert to this version?"
								ok=_"Revert"
								action={postback postback={revert rsc_id=a.rsc_id rev_id=a.id}
												delegate=`controller_admin_backup_revision`}
						}
			%}
		</td>
	</tr>
{% endif %}

{% for k,a,b in diff %}
{% if not k|member:[`modifier_id`, `modified`, `version`] %}
	<tr {% if has_diff %}class="do_make_diff"{% endif %}>
		<th>{{ k }}</th>
 		<td>{{ a }}</td>
		<td>{{ b }}</td>
	</tr>
{% endif %}
{% endfor %}
{% endwith %}

</table>

