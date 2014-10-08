{% if identities %}
	<table id="{{ #listemail }}" class="table">
	{% for idn in identities %}
	{% with idn.id as idn_id %}
		<tr id="{{ #row.idn_id }}">
			<td>
			<div class="radio"><label>
				<input type="radio" name="{{ #verified}}" {% if id.email == idn.key %}checked{% endif %} class="radio nosubmit" value="{{ idn.key }}" />
				{{ idn.key }}
			</label></div>
			</td>
			<td>
				{% if idn.is_verified %}
					<span class="glyphicon glyphicon-ok" title="{_ Verified _}"></span>
				{% else %}
					<a id="{{ #verify.idn_id }}"  href="#" class="btn btn-default btn-sm" title="{_ Send verification e-mail _}">{_ Verify _}</a>		
					{% wire id=#verify.idn_id 
							postback={identity_verify_confirm id=id idn_id=idn_id element=#row.idn_id}
							delegate=`mod_admin_identity`
					%}
				{% endif %}
			</td>
			<td>
				<a id="{{ #del.idn_id }}" href="#" class="btn btn-default btn-sm" title="{_ Delete this e-mail address _}">{_ Delete _}</a>
				{% wire id=#del.idn_id 
						postback={identity_delete_confirm id=id idn_id=idn_id element=#row.idn_id list_element=#listemail}
						delegate=`mod_admin_identity`
				%}
			</td>
		</tr>
	{% endwith %}
	{% endfor %}
	</table>
{% else %}
<p class="help-block">{_ No verified e-mail addresses. Please add one below. _}</p>
{% endif %}

{% wire name="verify-preferred-email"
		postback={identity_verify_preferred type='email' id=id} 
		delegate=`mod_admin_identity` 
%}
{% javascript %}
	$('#{{ #listemail }}').on('click', 'input.radio', function() {
		z_event('verify-preferred-email', {key: $(this).val()});
	});

	if (!$('#{{ #listemail }} input.radio:checked').length) {
		$('#{{ #listemail }} input.radio:first').click();
	}
{% endjavascript %}
