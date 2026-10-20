{# Include explicitly where source provenance should be displayed. #}
{% if id.provenance_html or id.provenance_url %}
<div class="provenance">
    {{ id.provenance_html }}
    {% if id.provenance_url %}
        <p><a href="{{ id.provenance_url|sanitize_url }}" target="_blank" rel="noopener noreferrer">{_ Original source _}</a></p>
    {% endif %}
</div>
{% endif %}
