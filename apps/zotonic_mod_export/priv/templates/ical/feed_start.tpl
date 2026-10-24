BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Zotonic//Zotonic {{ m.site.title|escape_ical_text }} iCalendar//EN
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:{{ title|default:id.title|unescape|escape_ical_text }}
X-WR-CALDESC:{_ Calendar _}
X-PUBLISHED-TTL:PT360M

{% if next_url %}X-ZOTONIC-NEXT:{{ next_url }}
{% endif %}