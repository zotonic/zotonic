{% for cat,_true in id.is_a %}
    {{ cat }}
    {% for z_language in id.language|default:[z_language] %}
        {{ m.rsc[cat].title }}
    {% endfor %}
{% endfor %}

{% for oid in id.o.author %}
    {% catinclude "pivot/_title.tpl" oid %}
{% endfor %}

{% for z_language in id.language|default:[z_language] %}
    {{ id.chapeau }} {{ id.summary }} {{ id.body }} {{ id.body_extra }}
    {{ id.date_remarks }}
{% endfor %}

{{ id.address_street_1 }}
{{ id.address_street_2 }}
{{ id.address_city }}
{{ id.address_state }}
{{ id.address_postcode }}
{{ id.address_country }}
{% if id.address_country and m.modules.active.mod_l10n %}
    {% for z_language in id.language|default:[z_language] %}
        {{ m.l10n.country_name[id.address_country] }}
    {% endfor %}
{% endif %}

{{ id.mail_street_1 }}
{{ id.mail_street_2 }}
{{ id.mail_city }}
{{ id.mail_state }}
{{ id.mail_postcode }}
{{ id.mail_country }}
{% if id.mail_country and m.modules.active.mod_l10n %}
    {% for z_language in id.language|default:[z_language] %}
        {{ m.l10n.country_name[id.mail_country] }}
    {% endfor %}
{% endif %}

{{ id.website }}

{{ id.seo_desc }}
