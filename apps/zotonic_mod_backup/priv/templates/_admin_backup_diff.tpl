{% with a.id|is_defined and b.id|is_defined as has_diff %}

{% if a.id and (a.id == b.id or b.id|is_undefined) %}
<p>
	{% trans "This version was saved on <strong>{date}</strong> by <strong>{author}</strong>."
        date=a.created|date:"Y-m-d H:i"|escape
        author=a.user_id.title|default:a.user_name|escape
    %}
</p>
{% elseif a and b %}
<p>
	{% trans "Changes from <strong>{from}</strong> to <strong>{to}</strong>."
        from=a.created|date:"Y-m-d H:i"|escape
        to=b.created|date:"Y-m-d H:i"|escape
    %}
</p>
{% else %}
<p>
	{% trans "This version was saved on <strong>{date}</strong> by <strong>{author}</strong>."
        date=id.modified|date:"Y-m-d H:i"|escape
        author=id.modifier_id.title|escape
    %}
</p>
{% endif %}

<table class="table">
{% if a.id and (a.id == b.id or b.id|is_undefined) %}
	<tr>
		<td></td>
		<td colspan="2">
			{% if a.rsc_id.exists %}
			{% button
                text=[_"Revert to this version..."]
                class="btn btn-danger"
				action={dialog_open
					title=_"Revert page to earlier version"
					template="_dialog_backup_revert_confirm.tpl"
					rsc_id=a.rsc_id
					rev_id=a.id
				}
			%}
			{% else %}
			{% button
                text=[_"Restore this page..."]
                class="btn btn-primary"
				action={dialog_open
					title=_"Restore deleted page"
					template="_dialog_backup_revert_confirm.tpl"
					rsc_id=a.rsc_id
					rev_id=a.id
				}
			%}
			{% endif %}
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

