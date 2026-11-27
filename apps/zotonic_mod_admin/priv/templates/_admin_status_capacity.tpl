{# Native meters clamp their visual fill and expose their range without JavaScript. #}
{% if total > 0 %}
    <div class="admin-capacity{% if alert %} admin-capacity-alert{% endif %}">
        <meter min="0" max="{{ total }}" value="{{ used }}" aria-label="{{ label|escape }}"></meter>
        <span class="admin-capacity-value">{{ (used / total * 100)|round }}%</span>
        {% if alert %}<strong class="text-danger"><span class="fa fa-warning" aria-hidden="true"></span> {_ High usage _}</strong>{% endif %}
    </div>
{% else %}
    <p class="text-muted">{_ Usage information unavailable. _}</p>
{% endif %}
