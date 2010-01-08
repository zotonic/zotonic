BEGIN:VCALENDAR
PRODID:-//Zotonic//Zotonic {{ m.config.zotonic.version.value }} Module iCalendar//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:{{ m.config.site.title.value|default:m.config.site.hostname.value|escape_ical }} - {{ m.rsc[cat].ical_feed_title|default:m.rsc[cat].title|escape_ical }}
X-WR-CALDESC:{% if m.rsc[cat].ical_feed_desc %}{{ m.rsc[cat].ical_feed_desc|escape_ical }}{% else %}All upcoming “{{ m.rsc[cat].title|escape_ical }}” events.{% endif %}
{% for id in m.search[{upcoming cat=cat}] %}{% catinclude "_vevent.tpl" id %}
{% endfor %}END:VCALENDAR
