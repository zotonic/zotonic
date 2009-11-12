{# Show the edit fields to edit the name of a person #}
{% if r.is_a.person or r.is_a.location %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
		<span class="title">Address</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item">
		<fieldset class="admin-form">

			<h4>Electronic address</h4>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone">Telephone</label>
					<input id="phone" type="text" name="phone" value="{{ r.phone }}" style="width: 185px" />
				</div>
			</div>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone_alt">Alternative telephone</label>
					<input id="phone_alt" type="text" name="phone_alt" value="{{ r.phone_alt }}" style="width: 185px" />
				</div>
			</div>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone_emergency">Emergency telephone</label>
					<input id="phone_emergency" type="text" name="phone_emergency" value="{{ r.phone_emergency }}" style="width: 195px" />
				</div>
			</div>
			
			<div class="zp-50">
				<div class="clear form-item clearfix">
					<label for="email">E-mail address</label>
					<input id="email" type="text" name="email" value="{{ r.email }}" style="width: 285px" />
					{% validate id="email" type={email} %}
				</div>
			</div>

			<div class="zp-50">
				<div class="form-item clearfix">
					<label for="website">Website</label>
					<input id="website" type="text" name="website" value="{{ r.website }}" style="width: 295px" />
				</div>
			</div>
{#
	-- Inherited:   street1 character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   street2 character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   city character varying(50) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   state character varying(50) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   postcode character varying(30) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   country character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   mail_is_special boolean NOT NULL DEFAULT false,
	-- Inherited:   m_street1 character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_street2 character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_city character varying(50) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_state character varying(50) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_postcode character varying(30) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_country character varying(80) NOT NULL DEFAULT ''::character varying,
	-- Inherited:   m_addressee character varying(80) NOT NULL DEFAULT ''::character varying,
#}

			<hr class="clear" />

			<div class="zp-50">
				<h4>Visiting address</h4>
			
				<div class="form-item clearfix">
					<label for="address_street_1">Street Line 1</label>
					<input id="address_street_1" type="text" name="address_street_1" value="{{ r.address_street_1 }}" style="width: 285px" />
				</div>

				<div class="form-item clearfix">
					<label for="address_street_2">Street Line 2</label>
					<input id="address_street_2" type="text" name="address_street_2" value="{{ r.address_street_2 }}" style="width: 285px" />
				</div>

				<div class="form-item clearfix">
					<label for="address_city">City</label>
					<input id="address_city" type="text" name="address_city" value="{{ r.address_city }}" style="width: 285px" />
				</div>

				<div class="form-item clearfix">
					<label for="address_postcode">Postcode</label>
					<input id="address_postcode" type="text" name="address_postcode" value="{{ r.address_postcode }}" style="width: 100px"/>
				</div>

				<div class="form-item clearfix">
					<label for="address_state">State</label>
					<input id="address_state" type="text" name="address_state" value="{{ r.address_state }}" style="width: 285px" />
				</div>

				<div class="form-item clearfix">
					<label for="address_country">Country</label>
					<input id="address_country" type="text" name="address_country" value="{{ r.address_country }}" style="width: 285px" />
				</div>
			</div>

			<div class="zp-50">
				<h4>Mailing address</h4>
			
				<div class="form-item clearfix">
					<label for="mail_street_1">Street Line 1</label>
					<input id="mail_street_1" type="text" name="mail_street_1" value="{{ r.mail_street_1 }}" style="width: 295px" />
				</div>

				<div class="form-item clearfix">
					<label for="mail_street_2">Street Line 2</label>
					<input id="mail_street_2" type="text" name="mail_street_2" value="{{ r.mail_street_2 }}" style="width: 295px" />
				</div>

				<div class="form-item clearfix">
					<label for="mail_city">City</label>
					<input id="mail_city" type="text" name="mail_city" value="{{ r.mail_city }}" style="width: 295px" />
				</div>

				<div class="form-item clearfix">
					<label for="mail_postcode">Postcode</label>
					<input id="mail_postcode" type="text" name="mail_postcode" value="{{ r.mail_postcode }}" style="width: 100px"/>
				</div>

				<div class="form-item clearfix">
					<label for="mail_state">State</label>
					<input id="mail_state" type="text" name="mail_state" value="{{ r.mail_state }}" style="width: 295px" />
				</div>

				<div class="form-item clearfix">
					<label for="mail_country">Country</label>
					<input id="mail_country" type="text" name="mail_country" value="{{ r.mail_country }}" style="width: 295px" />
				</div>
			</div>
			
			<div class="clearfix">&nbsp;</div>

			{% include "_admin_save_buttons.tpl" %}
		</fieldset>
	</div>
</div>
{% endif %}
