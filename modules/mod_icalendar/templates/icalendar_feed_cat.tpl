BEGIN:VCALENDAR
PRODID:-//Zotonic//Zotonic {{ m.config.zotonic.version.value }} Module iCalendar//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:{{ m.rsc[cat].title }}
X-WR-CALDESC:All upcoming “{{ m.rsc[cat].title }}” events.
{% for id in m.search[{upcoming cat=cat}] %}{% catinclude "_vevent.tpl" id %}
{% endfor %}END:VCALENDAR
