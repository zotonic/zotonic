{% with m.site.security as security %}
# Our security address
{% for k, c in security.contact %}
Contact: {% if k == 'email' %}mailto:{% endif %}{{ c }}
{% empty %}
# Please contact the site administrator as no contact details are configured for security issues specifically.
{% endfor %}

{% if security.policy as url %}
# Our security policy
Policy: {{ url }}
{% endif %}

{% if security.hiring as url %}
# We are hiring!
Hiring: {{ url }}
{% endif %}

# The official location of this file
Canonical: {% url security_txt absolute_url %}

Expires: {{ security.expires }}

{% endwith %}
