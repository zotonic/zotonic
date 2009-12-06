<input type="text" style="width:80px" name="dt:ymd:{{ is_end }}:{{ name }}" value="{{ date|date:'Y-m-d' }}" class="do_datepicker" />
<input type="text" style="width:40px" name="dt:hi:{{ is_end }}:{{ name }}" value="{{ date|date:'H:i' }}" />

{# 
<input type="text" style="width:32px" name="dt:y:{{ is_end }}:{{ name }}" value="{{ date|date:'Y' }}" />
<input type="text" style="width:20px" name="dt:m:{{ is_end }}:{{ name }}" value="{{ date|date:'m' }}" />
<input type="text" style="width:20px" name="dt:d:{{ is_end }}:{{ name }}" value="{{ date|date:'d' }}" /> &ndash;
<input type="text" style="width:20px" name="dt:h:{{ is_end }}:{{ name }}" value="{{ date|date:'H' }}" /> :
<input type="text" style="width:20px" name="dt:i:{{ is_end }}:{{ name }}" value="{{ date|date:'i' }}" />
#}