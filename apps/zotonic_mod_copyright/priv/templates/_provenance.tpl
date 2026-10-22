{# Include explicitly where source provenance should be displayed. #}
{% with id|trans_filter_filled:"provenance_html" as provenance_html %}
{% if provenance_html or id.provenance_url %}
<div class="provenance">
    {{ provenance_html }}
    {% if id.provenance_url %}
        <p><a href="{{ id.provenance_url|sanitize_url }}" target="_blank" rel="noopener noreferrer">{_ Original source _}</a></p>
    {% endif %}
</div>
{% endif %}
{% endwith %}
