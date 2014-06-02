<input
    type="text"
    name="dt:ymd:{{ is_end }}:{{ name }}"
    value="{{ date|date:'Y-m-d':date_is_all_day }}"
    {% if is_editable %}
        class="do_datepicker {{ class }} {{ date_class }}"
    {% else %}
        class="{{ class }} {{ date_class }}"
        disabled="disabled"
    {% endif %}
/>
<input 	
    type="text" 
	name="dt:hi:{{ is_end }}:{{ name }}"
	value="{% if not date_is_all_day %}{{ date|date:'H:i' }}{% endif %}" 
	{% if date_is_all_day %}
        style="display: none;"
    {% endif %}
    {% if is_editable %}
		data-timepicker="timeFormat:'H:i',step:15,scrollDefaultTime:{% if is_end %}'18:00'{%else%}'08:30'{% endif %}"
		class="input-mini do_timepicker {{ class }} {{ time_class }}"
    {% else %}
        class="input-mini {{ class }} {{ time_class }}"
        disabled="disabled"
    {% endif %}
/>
