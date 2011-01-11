{% with m.rsc[id] as r %}{% with r.date_start as dtstart %}{% with r.date_end as dtend %}
BEGIN:VEVENT
CREATED:{{ r.created|utc|date:"Ymd\\THis" }}Z
LAST-MODIFIED:{{ r.modified|utc|date:"Ymd\\THis" }}Z
UID:{{ r.resource_uri|unescape|escape_ical }}
{% ifequal dtstart|date:"His" "000000" %}
{% if dtend|is_undefined or dtend|date:"His" == "235959" %}
DTSTART;VALUE=DATE:{{ dtstart|date:"Ymd" }}
DTEND;VALUE=DATE:{{ dtend|default:dtstart|date:"Ymd" }}
{% else %}
DTSTART:{{ dtstart|utc|date:"Ymd\\THis" }}Z
DTEND:{{ dtend|utc|date:"Ymd\\THis" }}Z
{% endif %}
{% else %}
DTSTART:{{ dtstart|utc|date:"Ymd\\THis" }}Z
DTEND:{{ dtend|default:dtstart|utc|date:"Ymd\\THis" }}Z
{% endifequal %}
TRANSP:OPAQUE
SUMMARY:{{ r.title|unescape|escape_ical }}
DESCRIPTION:{{ r.summary|unescape|escape_ical }}
LOCATION:
URL;VALUE=URI:{{ site_url }}{{ r.page_url|unescape }}
SEQUENCE:0
END:VEVENT
{% endwith %}{% endwith %}{% endwith %}