{% with m.rsc[id] as r %}BEGIN:VEVENT
CREATED:{{ r.created|utc|date:"Ymd\\THis" }}Z
LAST-MODIFIED:{{ r.modified|utc|date:"Ymd\\THis" }}Z
UID:{{ r.resource_uri|unescape|escape_ical }}
{%with r.date_start|utc|date:"Ymd\\THis" as date_start %}DTSTART:{{ date_start }}Z
DTEND:{{ r.date_end|utc|date:"Ymd\\THis"|default:date_start }}Z{%endwith %}
TRANSP:OPAQUE
SUMMARY:{{ r.title|unescape|escape_ical }}
DESCRIPTION:{{ r.summary|unescape|escape_ical }}
LOCATION:
URL;VALUE=URI:{{ site_url }}{{ r.page_url|unescape }}
SEQUENCE:0
END:VEVENT{% endwith %}