{% with id.pref_tz as pref_tz %}
{% for tz in m.l10n.timezones %}
	<option {% if timezone == tz %}selected{% endif %}>{{ tz }}</option>
{% endfor %}
{% endwith %}
