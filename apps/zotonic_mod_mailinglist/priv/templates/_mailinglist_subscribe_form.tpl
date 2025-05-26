{% with m.mailinglist.recipient[recipient_id] as rcpt %}
{% with form_id|default:#form as form_id %}
    {% if recipient_id %}
        {% wire id=form_id
        		type="submit"
        		postback={recipient_edit
        			id=id
        			in_admin=in_admin
        			recipient_id=recipient_id
        			send_confirm=send_confirm
        			send_welcome=send_welcome
        		}
        		delegate=delegate
        %}
    {% else %}
        {% wire id=form_id
        		type="submit"
        		postback={recipient_add
        			id=id
        			in_admin=in_admin
        			send_confirm=send_confirm
        			send_welcome=send_welcome
        		}
        		delegate=delegate
        %}
    {% endif %}

    {% if is_email_only %}
        <form id="{{ form_id }}" method="post" action="postback" class="form-inline">
	        <div class="form-group">
		        <input class="form-control" type="text" id="{{ #email }}" name="email" value="{{ rcpt.email|default:id.email|default:q.email|escape }}" placeholder="you@example.com" />
		        {% validate id=#email name="email" type={presence} type={email} %}
		        {% button class="btn btn-primary" type="submit" text=_"Subscribe" %}
	        </div>
        </form>
    {% else %}
        <form id="{{ form_id }}" method="post" action="postback" class="form">
	        <div class="form-group">
		        <label class="control-label" for="{{ #email }}">{_ E-mail _}</label>
				<input class="form-control" type="text" id="{{ #email }}" name="email" value="{{ rcpt.email|default:id.email|default:q.email|escape }}" />
				{% validate id=#email name="email" type={presence} type={email} %}
			</div>

			<div class="row">
				<div class="form-group col-lg-4 col-md-4">
					<label class="control-label" for="{{ #name_first }}">{_ First name _}</label>
					<div>
						<input class="form-control" id="{{ #name_first }}" type="text" name="name_first" value="{{ rcpt.props.name_first|default:id.name_first|default:q.name_first|escape }}" />
					</div>
				</div>

				<div class="form-group col-lg-2 col-md-2">
					<label class="control-label" for="{{ #name_surname_prefix }}">{_ Prefix _}</label>
					<div>
						<input class="form-control" id="{{ #name_surname_prefix }}" type="text" name="name_surname_prefix" value="{{ rcpt.props.name_surname_prefix|default:id.name_surname_prefix|default:q.name_surname_prefix }}" />
					</div>
				</div>

			    <div class="form-group col-lg-6 col-md-6">
				    <label class="control-label" for="{{ #name_surname }}">{_ Surname _}</label>
					<div>
						<input class="form-control" id="{{ #name_surname }}" type="text" name="name_surname" value="{{ rcpt.props.name_surname|default:id.name_surname|default:q.name_surname|escape }}" />
					</div>
				</div>
			</div>

			{% if not in_admin %}
				{% validate id=#name_first name="name_first" type={presence} %}
				{% validate id=#name_surname name="name_surname" type={presence} %}
			{% endif %}

			{% if in_admin and m.modules.active.mod_translation %}
				<div class="form-group">
				    <div class="form-group">
				        <label class="control-label" for="pref_language">{_ Language _}</label>
				        <div>
				            <select class="form-control" name="pref_language">
				                <option></option>
				                {% for code,lang in m.translation.language_list_enabled %}
				                    <option {% if rcpt.props.pref_language == code %}selected{% endif %} value="{{ code }}">{{ lang.name }}</a>
				                {% endfor %}
				            </select>
				        </div>
				    </div>
				</div>
			{% endif %}

			{% if in_admin and not recipient_id and not m.rsc[id].mailinglist_private %}
			<div class="form-group">
				<label class="checkbox-inline"><input type="checkbox" id="{{ #welcome }}" name="send_welcome" value="1" />{_ Send welcome _}</label>
			</div>
			{% endif %}

			<div {% if in_admin %}class="modal-footer"{% endif %}>
				{% if in_admin %}
					{% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
				{% endif %}

				{% if recipient_id %}
					{% button class="btn btn-primary" type="submit" text=_"Save" %}
				{% else %}
					{% button class="btn btn-primary" type="submit" text=_"Subscribe" %}
				{% endif %}
			</div>
		</form>
	{% endif %}
{% endwith %}
{% endwith %}
