{% for z_language in id.language|default:[z_language] %}
    {{ id.title }} {{ id.short_title }} {{ id.subtitle }}
{% endfor %}

{{ id.name_prefix }} {{ id.name_first }} {{ id.name_surname_prefix }} {{ id.name_surname }}

{{ id.slug|replace:"-":" " }}
{{ id.page_path|replace:"/":" " }}
