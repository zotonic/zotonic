{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}

{% block widget_title %}{_ Address _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}content-address{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset>

	<h5>{_ Electronic address _}</h5>
	
	<div class="row">
	<div class="span3 control-group">
		<label class="control-label" for="phone">{_ Telephone _}</label>
			<div class="controls">
		<input id="phone" type="text" name="phone" value="{{ r.phone }}" class="span3" />
		</div>
	</div>

	<div class="span3 control-group">
		<label class="control-label" for="phone_alt">{_ Alternative telephone _}</label>
			<div class="controls">
		<input id="phone_alt" type="text" name="phone_alt" value="{{ r.phone_alt }}" class="span3" />
		</div>
		</div>

	<div class="span3 control-group">
		<label class="control-label" for="phone_emergency">{_ Emergency telephone _}</label>
			<div class="controls">
		<input id="phone_emergency" type="text" name="phone_emergency" value="{{ r.phone_emergency }}" class="span3" />
		</div>
		</div>
	</div>

	<div class="row">
	<div class="span4 control-group">
		<label class="control-label" for="email">{_ E-mail address _}</label>
			<div class="controls">
		<input id="email" type="text" name="email" value="{{ r.email }}" class="span4" />
		{% validate id="email" type={email} %}
		</div>
		</div>
	<div class="span4 control-group">
		<label class="control-label" for="website">{_ Website _}</label>
			<div class="controls">
		<input id="website" type="text" name="website" value="{{ r.website }}" class="span4" />
			</div>
		</div>
	</div>

	<hr />

	<h5>{_ Visiting address _}</h5>
	
	<div class="row">
		<div class="control-group span4">
			<label class="control-label" for="address_country">{_ Country _}</label>
				<div class="controls">
			{% if m.modules.info.mod_l10n.enabled %}
			<select id="address_country" name="address_country" class="span4">
				<option value=""></option>
				{% include "_l10n_country_options.tpl" country=r.address_country %}
			</select>
			{% else %}
			<input id="address_country" type="text" name="address_country" value="{{ r.address_country }}" class="span4" />
			{% endif %}
				</div>
		</div>
		{% wire id="address_country" 
				type="change" 
				action={script script="
					if ($(this).val() != '') $('#visit_address').slideDown();
					else $('#visit_address').slideUp();
				"}
		%}
	</div>
	
	<div id="visit_address" {% if not r.address_country %}style="display:none"{% endif %}>
		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="address_street_1">{_ Street Line 1 _}</label>
					<div class="controls">
				<input id="address_street_1" type="text" name="address_street_1" value="{{ r.address_street_1 }}" class="span4" />
					</div>
			</div>

			<div class="control-group span4">
				<label class="control-label" for="address_street_2">{_ Street Line 2 _}</label>
					<div class="controls">
				<input id="address_street_2" type="text" name="address_street_2" value="{{ r.address_street_2 }}" class="span4" />
					</div>
			</div>
		</div>

		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="address_city">{_ City _}</label>
					<div class="controls">
				<input id="address_city" type="text" name="address_city" value="{{ r.address_city }}" class="span4" />
					</div>
			</div>

			<div class="control-group span4">
				<label class="control-label" for="address_postcode">{_ Postcode _}</label>
					<div class="controls">
				<input id="address_postcode" type="text" name="address_postcode" value="{{ r.address_postcode }}" class="span4" />
					</div>
			</div>
		</div>

		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="address_state">{_ State _}</label>
					<div class="controls">
				<input id="address_state" type="text" name="address_state" value="{{ r.address_state }}" class="span4" />
					</div>
			</div>
		</div>
	</div>

	<h5>{_ Mailing address _}</h5>

	<div class="row">
		<div class="control-group span4">
			<label class="control-label" for="mail_country">{_ Country _}</label>
			<div class="controls">
			{% if m.modules.info.mod_l10n.enabled %}
				<select id="mail_country" name="mail_country" class="span4">
					<option value=""></option>
					{% include "_l10n_country_options.tpl" country=r.mail_country %}
				</select>
			{% else %}
				<input id="mail_country" type="text" name="mail_country" value="{{ r.mail_country }}" class="span4" />
			{% endif %}
			</div>
		</div>
		{% wire id="mail_country" 
				type="change" 
				action={script script="
					if ($(this).val() != '') $('#mail_address').slideDown();
					else $('#mail_address').slideUp();
				"}
		%}
	</div>
	
	<div id="mail_address" {% if not r.mail_country %}style="display:none"{% endif %}>
		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="mail_street_1">{_ Street Line 1 _}</label>
				<div class="controls">
					<input id="mail_street_1" type="text" name="mail_street_1" value="{{ r.mail_street_1 }}" class="span4" />
				</div>
			</div>

			<div class="control-group span4">
				<label class="control-label" for="mail_street_2">{_ Street Line 2 _}</label>
				<div class="controls">
					<input id="mail_street_2" type="text" name="mail_street_2" value="{{ r.mail_street_2 }}" class="span4" />
				</div>
			</div>
		</div>

		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="mail_city">{_ City _}</label>
				<div class="controls">
					<input id="mail_city" type="text" name="mail_city" value="{{ r.mail_city }}" class="span4" />
				</div>
			</div>

			<div class="control-group span4">
				<label class="control-label" for="mail_postcode">{_ Postcode _}</label>
				<div class="controls">
					<input id="mail_postcode" type="text" name="mail_postcode" value="{{ r.mail_postcode }}" class="span4" />
				</div>
			</div>
		</div>

		<div class="row">
			<div class="control-group span4">
				<label class="control-label" for="mail_state">{_ State _}</label>
				<div class="controls">
					<input id="mail_state" type="text" name="mail_state" value="{{ r.mail_state }}" class="span4" />
				</div>
			</div>
		</div>
	</div>
</fieldset>
{% endwith %}
{% endblock %}
