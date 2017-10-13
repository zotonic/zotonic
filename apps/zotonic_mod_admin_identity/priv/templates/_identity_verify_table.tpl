{% if identities %}
	<table id="{{ #listemail }}" class="table">
	{% with id.email_raw as email_raw %}
	{% for idn in identities %}
	{% with idn.id as idn_id %}
		<tr>
			<td>
                <div class="radio">
                    <label>
                        <input type="radio" name="{{ #verified}}" {% if email_raw == idn.key %}checked{% endif %} class="radio nosubmit" value="{{ idn.key|escape }}" />
                    {{ idn.key|escape }}
                    </label>
                </div>
			</td>
			{% if m.modules.active.mod_email_status %}
				{% with m.email_status[idn.key] as status %}
				<td class="text-right">
					{% if status.is_blocked %}
						<span class="text-error"><span class="glyphicon glyphicon-ban-circle"></span> {_ Blocked _}</span>
					{% elseif idn.is_verified %}
						<span class="glyphicon glyphicon-ok" title="{_ Verified _}"></span> {_ Verfied _}
					{% else %}
						<a id="{{ #verify.idn_id }}"  href="#" class="btn btn-default btn-sm" title="{_ Send verification e-mail _}">{_ Verify _}</a>
						{% wire id=#verify.idn_id
								postback={identity_verify_confirm id=id idn_id=idn_id list_element=#listemail}
								delegate=`mod_admin_identity`
						%}
					{% endif %}

					<a id="{{ #del.idn_id }}" href="#" class="btn btn-default btn-sm" title="{_ Delete this e-mail address _}">{_ Delete _}</a>
					{% wire id=#del.idn_id
							postback={identity_delete_confirm id=id idn_id=idn_id list_element=#listemail}
							delegate=`mod_admin_identity`
					%}
				</td>
				<td>
					<a id="{{ #status.idn_id }}" href="#" class="btn btn-default btn-sm" title="{_ View email status _}">{_ Status _}</a>
					{% wire id=#status.idn_id action={dialog_open title=_"Email Status" template="_dialog_email_status.tpl" email=idn.key} %}
				</td>
				{% endwith %}
			{% else %}
				<td class="text-right">
					{% if idn.is_verified %}
						<span class="glyphicon glyphicon-ok" title="{_ Verified _}"></span> {_ Verified _}
					{% else %}
						<a id="{{ #verify.idn_id }}"  href="#" class="btn btn-default btn-sm" title="{_ Send verification e-mail _}">{_ Verify _}</a>
						{% wire id=#verify.idn_id
								postback={identity_verify_confirm id=id idn_id=idn_id list_element=#listemail}
								delegate=`mod_admin_identity`
						%}
					{% endif %}

					<a id="{{ #del.idn_id }}" href="#" class="btn btn-default btn-sm" title="{_ Delete this e-mail address _}">{_ Delete _}</a>
					{% wire id=#del.idn_id
							postback={identity_delete_confirm id=id idn_id=idn_id list_element=#listemail}
							delegate=`mod_admin_identity`
					%}
				</td>
			{% endif %}
		</tr>
	{% endwith %}
	{% endfor %}
	{% endwith%}
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
