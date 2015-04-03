{# Latest modified locations #}
{% if m.rsc.location.id and m.acl.view['location'] %}
    {% include "admin_widget_dashboard_latest.tpl" cat="location" headline=_"Latest modified locations" last=1 %}
{% endif %}