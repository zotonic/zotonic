{# Show the edit fields to edit the name of a person #}
{% with m.rsc[id] as r %}
<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier" data-blockminifier="minifiedOnInit: true">
		<span class="title">{_ Address _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	<div class="item">
		<fieldset class="admin-form">

			<h4>{_ Electronic address _}</h4>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone">{_ Telephone _}</label>
					<input id="phone" type="text" name="phone" value="{{ r.phone }}" style="width: 185px" />
				</div>
			</div>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone_alt">{_ Alternative telephone _}</label>
					<input id="phone_alt" type="text" name="phone_alt" value="{{ r.phone_alt }}" style="width: 185px" />
				</div>
			</div>

			<div class="zp-33">
				<div class="form-item clearfix">
					<label for="phone_emergency">{_ Emergency telephone _}</label>
					<input id="phone_emergency" type="text" name="phone_emergency" value="{{ r.phone_emergency }}" style="width: 195px" />
				</div>
			</div>
			
			<div class="zp-50">
				<div class="clear form-item clearfix">
					<label for="email">{_ E-mail address _}</label>
					<input id="email" type="text" name="email" value="{{ r.email }}" style="width: 285px" />
					{% validate id="email" type={email} %}
				</div>
			</div>

			<div class="zp-50">
				<div class="form-item clearfix">
					<label for="website">{_ Website _}</label>
					<input id="website" type="text" name="website" value="{{ r.website }}" style="width: 295px" />
				</div>
			</div>

			<hr class="clear" />

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
					<input id="address_country" type="text" name="address_country" value="{{ r.address_country }}" style="width: 285px" />
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
					<input id="mail_country" type="text" name="mail_country" value="{{ r.mail_country }}" style="width: 295px" />
				</div>
			</div>
			
			<div class="clearfix">&nbsp;</div>

			{% include "_admin_save_buttons.tpl" %}
		</fieldset>
	</div>
</div>
{% endwith %}
