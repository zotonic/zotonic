{% with timezone|default:m.req.timezone as timezone %}
{% if is_editable %}
    {% with date_is_all_day|if:true:timezone as disp_tz %}
    <input
        type="date"
        name="dt:ymd:{{ is_end|if:1:0 }}:{{ name }}"
        value="{{ date|date:'Y-m-d':disp_tz }}"
        class="{{ class }} {{ date_class }} form-control"
        {% if placeholder %}
            placeholder="{{ placeholder }}"
        {% endif %}
    />
    <input
        type="time"
        name="dt:hi:{{ is_end|if:1:0 }}:{{ name }}"
        value="{% if not date_is_all_day %}{{ date|date:'H:i':disp_tz }}{% endif %}"
        {% if date_is_all_day %}
            style="display: none;"
        {% endif %}
        data-timepicker='{
            "timeFormat": "H:i",
            "step": 15,
            "scrollDefaultTime": {% if is_end %} "18:00" {% else %} "08:30" {% endif %}
        }'
        class="input-mini {{ class }} {{ time_class }} form-control"
    />
    {% endwith %}
{% else %}
    <span class="date">{{ date|date:'Y-m-d':date_is_all_day }} {% if not date_is_all_day %}{{ date|date:'H:i':timezone }}{% endif %}</span>
{% endif %}
{% endwith %}
