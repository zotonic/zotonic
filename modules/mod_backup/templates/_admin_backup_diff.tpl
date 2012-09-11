<table class="table">
	<tr>
		<td></td>
		<td>
			{% if a.id %}
				{% button text=_"Revert to this version" class="btn btn-success" 
					action={confirm text=_"Are you sure you want to revert to this version?"
									ok=_"Revert"
									action={postback postback={revert rsc_id=a.rsc_id rev_id=a.id}
													delegate=`resource_admin_backup_revision`}
							}
				%}
			{% else %}
				<b>{_ current version _}</b>
			{% endif %}
		</td>
		<td>
			{% if b.id %}
				{% button text=_"Revert to this version" class="btn btn-danger"
					action={confirm text=_"Are you sure you want to revert to this version?"
									ok=_"Revert"
									action={postback postback={revert rsc_id=b.rsc_id rev_id=b.id}
													delegate=`resource_admin_backup_revision`}
							}
				%}
			{% endif %}
		</td>
	</tr>
{% for k,a,b in diff %}
	<tr>
		<th>{{ k }}</th>
		<td>{{ a }}</td>
		<td>{{ b }}</td>
	</tr>
{% endfor %}
</table>
