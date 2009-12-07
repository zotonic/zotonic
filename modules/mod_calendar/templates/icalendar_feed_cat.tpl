BEGIN:VCALENDAR
PRODID:-//Zotonic//Zotonic {{ m.config.zotonic.version.value }} Module iCalendar//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:{{ m.config.site.title.value|default:m.config.site.hostname.value|escape_ical }} - {{ m.rsc[cat].title|escape_ical }}
X-WR-CALDESC:All upcoming “{{ m.rsc[cat].title|escape_ical }}” events.
{% for id in m.search[{upcoming cat=cat}] %}{% catinclude "_vevent.tpl" id %}
{% endfor %}END:VCALENDAR
