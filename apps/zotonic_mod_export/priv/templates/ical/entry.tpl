{% if dtstart %}
BEGIN:VEVENT
DTSTAMP:{{ dtstamp|date:"Ymd\\THis":"UTC" }}Z
{% if created %}CREATED:{{ created|date:"Ymd\\THis":"UTC" }}Z
{% endif %}
LAST-MODIFIED:{{ id.modified|date:"Ymd\\THis":"UTC" }}Z
UID:{{ id.uri|escape_ical_text }}{% if id.date_is_all_day %}
DTSTART;VALUE=DATE:{{ dtstart|date:"Ymd":"UTC" }}
DTEND;VALUE=DATE:{{ dtend|default:dtstart|add_day|date:"Ymd":"UTC" }}{% else %}
DTSTART:{{ dtstart|date:"Ymd\\THis":"UTC" }}Z
{% if dtend > dtstart %}DTEND:{{ dtend|date:"Ymd\\THis":"UTC" }}Z
{% endif %}{% endif %}
TRANSP:OPAQUE
SUMMARY:{{ id.title|unescape|escape_ical_text }}
DESCRIPTION:{{ id.summary|default:id.body|striptags|unescape|escape_ical_text }}
LOCATION:{{ id.location|unescape|escape_ical_text }}{% if id.pivot_location_lat and id.pivot_location_lng %}
GEO:{{ id.pivot_location_lat }};{{ id.pivot_location_lng }}{% endif %}
URL;VALUE=URI:{{ id.page_url_abs }}
{% if resource_url %}X-ZOTONIC-RESOURCE:{{ resource_url }}
{% endif %}SEQUENCE:{{ id.version|default:0 }}
END:VEVENT{% endif %}
