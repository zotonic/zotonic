{% if is_editable %}
    <input
        type="text"
        name="dt:ymd:{{ is_end }}:{{ name }}"
        value="{{ date|date:'Y-m-d':date_is_all_day }}"
        class="input-medium do_datepicker {{ class }} {{ date_class }}"
    />
    <input  
        type="text" 
        name="dt:hi:{{ is_end }}:{{ name }}"
        value="{% if not date_is_all_day %}{{ date|date:'H:i' }}{% endif %}" 
        {% if date_is_all_day %}
            style="display: none;"
        {% endif %}
        data-timepicker="timeFormat:'H:i',step:15,scrollDefaultTime:{% if is_end %}'18:00'{%else%}'08:30'{% endif %}"
        class="input-mini do_timepicker {{ class }} {{ time_class }}"
    />
{% else %}
    <span class="date">{{ date|date:'Y-m-d':date_is_all_day }} {% if not date_is_all_day %}{{ date|date:'H:i' }}{% endif %}</span>
{% endif %}
