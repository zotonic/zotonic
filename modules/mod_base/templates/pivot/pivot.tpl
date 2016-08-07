{# Template used by z_pivot_rsc to fill the index tables for resources #}
{# Extend this template if a category needs special pivoting (pivot uses a catinclude) #}

{# Arguments:
 # - id (integer) the resource id to be pivoted
 # - props (property list) the prepivoted properties of the resource
 # - z_language (atom) the preferred stemmer language
 #}

{# Blocks a..d contain the texts to be indexed, you need to render all languages #}
{% block a %}
    {% catinclude "pivot/_title_text.tpl" id %}
{% endblock %}

{% block b %}
    {% catinclude "pivot/_main_text.tpl" id %}
{% endblock %}

{% block c %}
    {% catinclude "pivot/_block_text.tpl" id %}
{% endblock %}

{% block d %}
    {% catinclude "pivot/_related_text.tpl" id %}
{% endblock %}

{# This title is used for sorting #}
{% block title %}
    {{ id.title|lower }}
{% endblock %}

{# All related resource ids, used for finding similar resources #}
{# Prefix object ids with 'zpo' and categories with 'zpc' #}
{% block related_ids %}
    {% catinclude "pivot/_related_ids.tpl" id %}
{% endblock %}

{# Pivot fields #}
{% block address_street %}{{ id.address_street_1|default:mail_address_street_1 }}{% endblock %}
{% block address_city %}{{ id.address_city|default:mail_city }}{% endblock %}
{% block address_postcode %}{{ id.address_postcode|default:mail_postcode }}{% endblock %}
{% block address_state %}{{ id.address_state|default:mail_state }}{% endblock %}
{% block address_country %}{{ id.address_country|default:mail_country }}{% endblock %}

{% block name_first %}{{ id.name_first }}{% endblock %}
{% block name_surname %}{{ id.surname }}{% endblock %}
{% block gender %}{{ id.gender }}{% endblock %}

{% block date_start %}{{ id.date_start }}{% endblock %}
{% block date_end %}{{ id.date_end }}{% endblock %}
{% block date_start_month_day %}{{ id.date_start|date:"md" }}{% endblock %}
{% block date_end_month_day %}{{ id.date_end|date:"md" }}{% endblock %}

{% block location_lat %}{{ id.computed_location_lat|default:id.location_lat }}{% endblock %}
{% block location_lng %}{{ id.computed_location_lng|default:id.location_lng }}{% endblock %}

{# Optional date when to repivot #}
{# Useful to repivot events when they are past their date_end, example: {{ id.date_end }} #}
{% block date_repivot %}{% endblock %}
