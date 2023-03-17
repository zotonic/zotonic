{% for tz in m.l10n.timezones %}
	<option {% if timezone == tz %}selected{% endif %}>{{ tz }}</option>
{% endfor %}
