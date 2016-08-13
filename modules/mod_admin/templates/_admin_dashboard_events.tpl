{# Latest modified events #}
{% if m.rsc['event'].id and m.acl.view['event'] %}
    {% include "admin_widget_dashboard_latest.tpl" cat="event" headline=_"Latest modified events" last=1 %}
{% endif %}
