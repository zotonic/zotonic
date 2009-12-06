{% with m.rsc[id] as r %}BEGIN:VEVENT
CREATED:{{ r.created|utc|date:"Ymd\\THis" }}Z
LAST-MODIFIED:{{ r.modified|utc|date:"Ymd\\THis" }}Z
UID:{{ site_url|escape_ical }}{{ r.resource_uri|unescape|escape_ical }}
DTSTART:{{ r.date_start|utc|date:"Ymd\\THis" }}Z
DTEND:{{ r.date_end|utc|date:"Ymd\\THis" }}Z
TRANSP:OPAQUE
SUMMARY:{{ r.title|unescape|escape_ical }}
DESCRIPTION:{{ r.summary|unescape|escape_ical }}
LOCATION:
URL;VALUE=URI:{{ site_url }}{{ r.page_url|unescape }}
SEQUENCE:0
END:VEVENT{% endwith %}