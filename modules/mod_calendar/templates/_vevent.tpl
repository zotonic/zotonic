{% with m.rsc[id] as r %}{% with r.date_start as dtstart %}{% with r.date_end as dtend %}
BEGIN:VEVENT
CREATED:{{ r.created|utc|date:"Ymd\\THis" }}Z
LAST-MODIFIED:{{ r.modified|utc|date:"Ymd\\THis" }}Z
UID:{{ r.resource_uri|unescape|escape_ical }}
{% ifequal dtstart|date:"His" "000000" %}
{% if dtend|date:"His"|eq:"235959" or dtend|is_undefined %}
{% with dtstart|date:"Ymd" as date_start %}
DTSTART;VALUE=DATE:{{ date_start }}
DTEND;VALUE=DATE:{{ dtend|date:"Ymd"|default:date_start }}
{% endwith %}
{% else %}
DTSTART:{{ dtstart|utc|date:"Ymd\\THis" }}Z
DTEND:{{ dtend|utc|date:"Ymd\\THis" }}Z
{% endif %}
{% else %}
{% with dtstart|utc|date:"Ymd\\THis" as date_start %}
DTSTART:{{ date_start }}Z
DTEND:{{ dtend|utc|date:"Ymd\\THis"|default:date_start }}Z
{% endwith %}
{% endifequal %}
TRANSP:OPAQUE
SUMMARY:{{ r.title|unescape|escape_ical }}
DESCRIPTION:{{ r.summary|unescape|escape_ical }}
LOCATION:
URL;VALUE=URI:{{ site_url }}{{ r.page_url|unescape }}
SEQUENCE:0
END:VEVENT
{% endwith %}{% endwith %}{% endwith %}