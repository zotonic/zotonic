{% if m.acl.is_admin and not m.acl.is_read_only %}
{% wire id=#entryform type="submit" postback="save" delegate="controller_admin_traffic_guard" %}
<form id="{{ #entryform }}" method="post" action="postback">
{% if entry %}
<p><strong>{% if entry.kind == "allow" %}{_ Allow _}{% else %}{_ Deny _}{% endif %} — {{ entry.ip|escape }}</strong></p>
<input type="hidden" name="kind" value="{{ entry.kind|escape }}">
<input type="hidden" name="ips" value="{{ entry.ip|escape }}">
{% else %}
<p>{_ Separate IPv4 or IPv6 addresses with spaces, commas or newlines. Re-adding an address to the same list updates its note and expiry. _}</p>
<div class="form-group"><label for="{{ #kind }}">{_ List _}</label>
<select id="{{ #kind }}" name="kind" class="form-control"><option value="allow">{_ Allow _}</option><option value="block">{_ Deny _}</option></select></div>
<div class="form-group"><label for="{{ #ips }}">{_ IP addresses _}</label>
<textarea id="{{ #ips }}" name="ips" rows="3" class="form-control" required autofocus>{{ ips|escape }}</textarea></div>
{% endif %}
<div class="form-group"><label for="{{ #note }}">{_ Note _}</label>
<input id="{{ #note }}" name="note" value="{{ entry.note|escape }}" class="form-control" maxlength="1000" required {% if entry %}autofocus{% endif %}></div>
{% if entry %}
<input type="hidden" name="validity" value="forever">
<div class="form-group"><label for="{{ #until }}">{_ Until (UTC; empty means forever) _}</label>
<input id="{{ #until }}" name="until" type="datetime-local" value="{{ entry.until|escape }}" class="form-control"></div>
{% else %}
<div class="form-group"><label for="{{ #validity }}">{_ Valid for _}</label><select id="{{ #validity }}" name="validity" class="form-control">{% include "_traffic_guard_duration.tpl" %}</select></div>
<div class="form-group"><label for="{{ #until }}">{_ Or until (UTC) _}</label><input id="{{ #until }}" name="until" type="datetime-local" class="form-control"></div>
{% endif %}
<div class="modal-footer">
{% button text=_"Cancel" class="btn btn-default" action={dialog_close} %}
<button type="submit" class="btn btn-primary">{% if entry %}{_ Save _}{% else %}{_ Save IP entries _}{% endif %}</button>
</div>
</form>
{% endif %}
