{% with m.l10n.countries as countries %}
	{% if country and not countries[country] %}
		<option value="{{ country }}" selected="selected">{{ country }}</option>
	{% endif %}
	{% for iso_code,name in countries %}
		<option value="{{ iso_code }}" {% if country == iso_code %}selected="selected"{% endif %}>{{ name }}</option>
	{% endfor %}
{% endwith %}
