BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Zotonic//Zotonic {{ m.config.site.title.value|escape_ical }} iCalendar//EN
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:{{ title|default:id.title|unescape|escape_ical }}
X-WR-CALDESC:{_ Calendar _}
X-PUBLISHED-TTL:PT360M
