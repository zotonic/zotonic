<input type="text" style="width:80px" name="dt:ymd:{{ is_end }}:{{ name }}" value="{{ date|date:'Y-m-d' }}" class="do_datepicker" />
<input 	type="text" 
		name="dt:hi:{{ is_end }}:{{ name }}"
		class="input-mini do_timepicker"
		style="{% if is_whole_day %}display: none;{% endif %}"
		value="{{ date|date:'H:i' }}" 
		data-timepicker="timeFormat:'H:i',step:15,scrollDefaultTime:{% if is_end %}'18:00'{%else%}'08:30'{% endif %}"
	/>