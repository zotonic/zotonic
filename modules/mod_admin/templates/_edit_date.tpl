<input type="text" style="width:80px" name="dt:ymd:{{ is_end }}:{{ name }}" value="{{ date|date:'Y-m-d':date_is_all_day }}" class="do_datepicker" />
<input 	type="text" 
		name="dt:hi:{{ is_end }}:{{ name }}"
		class="input-mini do_timepicker"
		style="{% if date_is_all_day %}display: none;{% endif %}"
		value="{% if not date_is_all_day %}{{ date|date:'H:i' }}{% endif %}" 
		data-timepicker="timeFormat:'H:i',step:15,scrollDefaultTime:{% if is_end %}'18:00'{%else%}'08:30'{% endif %}"
	/>