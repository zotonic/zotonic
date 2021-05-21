{% extends "admin_edit_widget_std.tpl" %}

{# Show the edit fields to edit the name of a person #}

{% block widget_title %}
{_ Address _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}content-address{% endblock %}

{% block widget_content %}
	<div class="row">
		<div class="col-lg-6 col-md-6">
			<div class="form-group label-floating">
					<input class="form-control" id="phone" type="text" name="phone" inputmode="tel" value="{{ id.phone }}" placeholder="{_ Telephone _}">
				<label class="control-label" for="phone">{_ Telephone _}</label>
			</div>
			<div class="form-group label-floating">
				<input class="form-control" id="phone_mobile" type="text" name="phone_mobile" inputmode="tel" value="{{ id.phone_mobile }}" placeholder="{_ Mobile _}">
				<label class="control-label" for="phone">{_ Mobile _}</label>
			</div>
		</div>

		<div class="col-lg-6 col-md-6">
			<div class="form-group label-floating">
				<input class="form-control" id="phone_alt" type="text" name="phone_alt" inputmode="tel" value="{{ id.phone_alt }}" placeholder="{_ Alternative telephone _}">
				<label class="control-label" for="phone_alt">{_ Alternative telephone _}</label>
			</div>

			<div class="form-group label-floating">
				<input class="form-control" id="phone_emergency" type="text" name="phone_emergency" inputmode="tel" value="{{ id.phone_emergency }}" placeholder="{_ Emergency telephone _} {_ (public) _}">
				<label class="control-label" for="phone_emergency">{_ Emergency telephone _} {_ (public) _}</label>
			</div>
		</div>
	</div>

	<div class="row">
		<div class="col-lg-12 col-md-12">
			<div class="form-group label-floating">
				<input class="form-control" id="website" type="text" name="website" inputmode="url" value="{{ id.website }}" placeholder="{_ Website _} {_ (public) _}">
				<label class="control-label" for="website">{_ Website _} {_ (public) _}</label>
			</div>

	        <div class="form-group">
                <label class="checkbox">
                    <input type="checkbox" id="field-is-website=redirect" name="is_website_redirect" value="1"
                        {% if id.is_website_redirect %}checked{% endif %}
                        {% if not id.is_editable %}disabled="disabled"{% endif %}
                    />
                    {_ Redirect to website on page view _}
                </label>
	        </div>

			{% catinclude "_admin_edit_content_address_email.tpl" id %}
		</div>
	</div>

    <div class="form-group">
    	<div class="widget-section-header">{_ Visiting address _} {_ (private) _}</div>
    </div>

	<div class="form-group label-floating">
		{% if m.modules.active.mod_l10n %}
			<select class="form-control" id="address_country" name="address_country">
				<option value=""></option>
				{% optional include "_l10n_country_options.tpl" country=id.address_country %}
			</select>
		{% else %}
			<input class="form-control" id="address_country" type="text" name="address_country" value="{{ id.address_country }}" placeholder="{_ Country _}">
		{% endif %}
		<label class="control-label" for="address_country">{_ Country _}</label>
	</div>
	{% wire id="address_country"
			type="change"
			action={script script="
				if ($(this).val() != '') $('#visit_address').slideDown();
				else $('#visit_address').slideUp();
			"}
	%}
	<div id="visit_address" {% if not id.address_country %}style="display:none"{% endif %}>
		<div class="form-group label-floating">
			<input class="form-control" id="address_street_1" type="text" name="address_street_1" value="{{ id.address_street_1 }}" placeholder="{_ Street Line 1 _}">
			<label class="control-label" for="address_street_1">{_ Street Line 1 _}</label>
		</div>

		<div class="form-group label-floating">
			<input class="form-control" id="address_street_2" type="text" name="address_street_2" value="{{ id.address_street_2 }}" placeholder="{_ Street Line 2 _}">
			<label class="control-label" for="address_street_2">{_ Street Line 2 _}</label>
		</div>

		<div class="row">
			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="address_city" type="text" name="address_city" value="{{ id.address_city }}" placeholder="{_ City _}">
				<label class="control-label" for="address_city">{_ City _}</label>
			</div>

			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="address_postcode" type="text" name="address_postcode" value="{{ id.address_postcode }}" placeholder="{_ Postcode _}">
				<label class="control-label" for="address_postcode">{_ Postcode _}</label>
			</div>
		</div>

		<div class="row">
			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="address_state" type="text" name="address_state" value="{{ id.address_state }}" placeholder="{_ State _}">
				<label class="control-label" for="address_state">{_ State _}</label>
			</div>
		</div>
	</div>

    <div class="form-group">
    	<div class="widget-section-header">{_ Mailing address _} {_ (public) _}</div>
    </div>

	<div class="form-group label-floating">
		<input class="form-control" id="mail_email" type="text" name="mail_email" value="{{ id.mail_email }}" placeholder="{_ Email address _}" placeholder="{_ Email address for public display _}">
		{% validate id="mail_email" type={email} %}
		<label class="control-label" for="mail_email">{_ Email address for public display _}</label>
	</div>

	<div class="form-group">
		<label class="control-label" for="mail_country">{_ Country _}</label>
		<div>
		{% if m.modules.active.mod_l10n %}
			<select class="form-control" id="mail_country" name="mail_country">
				<option value=""></option>
				{% optional include "_l10n_country_options.tpl" country=id.mail_country %}
			</select>
		{% else %}
			<input class="form-control" id="mail_country" type="text" name="mail_country" value="{{ id.mail_country }}" />
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

	<div id="mail_address" {% if not id.mail_country %}style="display:none"{% endif %}>
		<div class="form-group label-floating">
			<input class="form-control" id="mail_street_1" type="text" name="mail_street_1" value="{{ id.mail_street_1 }}" placeholder="{_ Street Line 1 _}">
			<label class="control-label" for="mail_street_1">{_ Street Line 1 _}</label>
		</div>

		<div class="form-group label-floating">
			<input class="form-control" id="mail_street_2" type="text" name="mail_street_2" value="{{ id.mail_street_2 }}" placeholder="{_ Street Line 2 _}">
			<label class="control-label" for="mail_street_2">{_ Street Line 2 _}</label>
		</div>

		<div class="row">
			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="mail_city" type="text" name="mail_city" value="{{ id.mail_city }}" placeholder="{_ City _}">
				<label class="control-label" for="mail_city">{_ City _}</label>
			</div>

			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="mail_postcode" type="text" name="mail_postcode" value="{{ id.mail_postcode }}" placeholder="{_ Postcode _}">
				<label class="control-label" for="mail_postcode">{_ Postcode _}</label>
			</div>
		</div>

		<div class="row">
			<div class="form-group col-lg-6 col-md-6 label-floating">
				<input class="form-control" id="mail_state" type="text" name="mail_state" value="{{ id.mail_state }}" placeholder="{_ State _}">
				<label class="control-label" for="mail_state">{_ State _}</label>
			</div>
		</div>
	</div>
{% endblock %}
