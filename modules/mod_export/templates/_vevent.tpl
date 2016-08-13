{% with id.date_start, id.date_end as dtstart, dtend %}{% if dtstart %}BEGIN:VEVENT
CREATED:{{ id.created|date:"Ymd\\THis":"UTC" }}Z
LAST-MODIFIED:{{ id.modified|date:"Ymd\\THis":"UTC" }}Z
UID:{{ id.uri|escape_ical }}{% if id.date_is_all_day %}
DTSTART;VALUE=DATE:{{ dtstart|date:"Ymd" }}
DTEND;VALUE=DATE:{{ dtend|default:dtstart|add_day|date:"Ymd":"UTC" }}{% else %}
DTSTART:{{ dtstart|date:"Ymd\\THis":"UTC" }}Z
DTEND:{{ dtend|default:dtstart|date:"Ymd\\THis":"UTC" }}Z{% endif %}
TRANSP:OPAQUE
SUMMARY:{{ id.title|unescape|escape_ical }}
DESCRIPTION:{{ id|summary|striptags|unescape|escape_ical }}
LOCATION:{% if id.pivot_location_lat and id.pivot_location_lng %}
GEO:{{ id.pivot_location_lat }};{{ id.pivot_location_lng }}{% endif %}
URL;VALUE=URI:{{ id.page_url_abs }}
SEQUENCE:0
END:VEVENT{% endif %}
{% endwith %}
