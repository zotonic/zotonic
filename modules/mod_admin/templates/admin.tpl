{% extends "admin_base.tpl" %}

{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
        <div>
            <h2>{_ Dashboard _}</h2>

            <div class="well">
                {% button   class="btn btn-primary" 
                        text=_"Make a new page or media" 
                        action={dialog_new_rsc title=""} %}

                {% all include "_admin_make_page_buttons.tpl" %}

                <a class="btn" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn" href="{% url admin_media %}">{_ All media _}</a>
            </div>
        </div>

        <div class="row">
            <div class="span6">
                {# Latest modified texts #}
                {% include "admin_widget_dashboard_latest.tpl" cat="text" headline=_"Latest modified texts" %}

                {# Latest modified persons #}
                {% include "admin_widget_dashboard_latest.tpl" cat="person" headline=_"Latest modified persons" %}
            </div>

            <div class="span6">

                {# Latest modified locations #}
                {% if m.rsc['location'].id and m.acl.view['location'] %}
                {% include "admin_widget_dashboard_latest.tpl" cat="location" headline=_"Latest modified locations" last=1 %}
                {% endif %}

                {# Latest modified events #}
                {% if m.rsc['event'].id and m.acl.view['event'] %}
                {% include "admin_widget_dashboard_latest.tpl" cat="event" headline=_"Latest modified events" last=1 %}
                {% endif %}
                
                {# Latest modified media items #}
                {% include "admin_widget_dashboard_latest.tpl" cat="media" headline=_"Latest modified media items" media=1 last=1 %}
            </div>
            
        </div>
{% endblock %}
