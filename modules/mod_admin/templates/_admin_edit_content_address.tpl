{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}

{% block widget_title %}{_ Address _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}content-address{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<fieldset>

    <div class="row">
        <div class="span8">
	    <h5>{_ Electronic address _}</h5>
        </div>
    </div>
        <div class="row">
	    <div class="span3 control-group">
		<label class="control-label" for="phone">{_ Telephone _}</label>
                <div class="controls">
		    <input id="phone" type="text" name="phone" value="{{ r.phone }}" style="width: 185px" />
		</div>
	    </div>

	    <div class="span3 control-group">
		<label class="control-label" for="phone_alt">{_ Alternative telephone _}</label>
                <div class="controls">
		    <input id="phone_alt" type="text" name="phone_alt" value="{{ r.phone_alt }}" style="width: 185px" />
		</div>
            </div>

	    <div class="span3 control-group">
		<label class="control-label" for="phone_emergency">{_ Emergency telephone _}</label>
                <div class="controls">
		    <input id="phone_emergency" type="text" name="phone_emergency" value="{{ r.phone_emergency }}" style="width: 195px" />
		</div>
            </div>
	</div>

        <div class="row">
	    <div class="span4 control-group">
		<label class="control-label" for="email">{_ E-mail address _}</label>
                <div class="controls">
		    <input id="email" type="text" name="email" value="{{ r.email }}" style="width: 285px" />
		    {% validate id="email" type={email} %}
		</div>
            </div>
	    <div class="span4 control-group">
		<label class="control-label" for="website">{_ Website _}</label>
                <div class="controls">
		    <input id="website" type="text" name="website" value="{{ r.website }}" style="width: 295px" />
                </div>
            </div>
	</div>

	<hr />

	<div class="zp-50">
		<h4>{_ Visiting address _}</h4>

		<div class="form-item clearfix">
			<label for="address_street_1">{_ Street Line 1 _}</label>
			<input id="address_street_1" type="text" name="address_street_1" value="{{ r.address_street_1 }}" style="width: 285px" />
		</div>

		<div class="form-item clearfix">
			<label for="address_street_2">{_ Street Line 2 _}</label>
			<input id="address_street_2" type="text" name="address_street_2" value="{{ r.address_street_2 }}" style="width: 285px" />
		</div>

		<div class="form-item clearfix">
			<label for="address_city">{_ City _}</label>
			<input id="address_city" type="text" name="address_city" value="{{ r.address_city }}" style="width: 285px" />
		</div>

		<div class="form-item clearfix">
			<label for="address_postcode">{_ Postcode _}</label>
			<input id="address_postcode" type="text" name="address_postcode" value="{{ r.address_postcode }}" style="width: 100px"/>
		</div>

		<div class="form-item clearfix">
			<label for="address_state">{_ State _}</label>
			<input id="address_state" type="text" name="address_state" value="{{ r.address_state }}" style="width: 285px" />
		</div>

		<div class="form-item clearfix">
			<label for="address_country">{_ Country _}</label>
			{% if m.modules.info.mod_l10n.enabled %}
				<select id="address_country" name="address_country">
					<option value=""></option>
					{% include "_l10n_country_options.tpl" country=r.address_country %}
				</select>
			{% else %}
				<input id="address_country" type="text" name="address_country" value="{{ r.address_country }}" style="width: 285px" />
			{% endif %}
		</div>
	</div>

	<div class="zp-50">
		<h4>{_ Mailing address _}</h4>

		<div class="form-item clearfix">
			<label for="mail_street_1">{_ Street Line 1 _}</label>
			<input id="mail_street_1" type="text" name="mail_street_1" value="{{ r.mail_street_1 }}" style="width: 295px" />
		</div>

		<div class="form-item clearfix">
			<label for="mail_street_2">{_ Street Line 2 _}</label>
			<input id="mail_street_2" type="text" name="mail_street_2" value="{{ r.mail_street_2 }}" style="width: 295px" />
		</div>

		<div class="form-item clearfix">
			<label for="mail_city">{_ City _}</label>
			<input id="mail_city" type="text" name="mail_city" value="{{ r.mail_city }}" style="width: 295px" />
		</div>

		<div class="form-item clearfix">
			<label for="mail_postcode">{_ Postcode _}</label>
			<input id="mail_postcode" type="text" name="mail_postcode" value="{{ r.mail_postcode }}" style="width: 100px"/>
		</div>

		<div class="form-item clearfix">
			<label for="mail_state">{_ State _}</label>
			<input id="mail_state" type="text" name="mail_state" value="{{ r.mail_state }}" style="width: 295px" />
		</div>

		<div class="form-item clearfix">
			<label for="mail_country">{_ Country _}</label>
			{% if m.modules.info.mod_l10n.enabled %}
				<select id="mail_country" name="mail_country">
					<option value=""></option>
					{% include "_l10n_country_options.tpl" country=r.mail_country %}
				</select>
			{% else %}
				<input id="mail_country" type="text" name="mail_country" value="{{ r.mail_country }}" style="width: 285px" />
			{% endif %}
		</div>
	</div>

	<div class="clearfix">&nbsp;</div>

	{% include "_admin_save_buttons.tpl" %}
</fieldset>
{% endwith %}
{% endblock %}
