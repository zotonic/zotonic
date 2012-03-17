{% extends "admin_base.tpl" %}

{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
        <div>
            <h2>{_ Dashboard _}</h2>

            <div class="well">
                {% all include "_admin_make_page_buttons.tpl" %}

                {% button   class="btn btn-primary" 
                        text=_"Make a new page" 
                        action={dialog_new_rsc title=""} %}

                {% button   class="btn"
                        text=_"Make a new media item"
                        action={dialog_media_upload title=""}
                        disabled=(not m.acl.insert.media and not m.acl.insert.image) %}
            </div>
        </div>

        <hr />
                        
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
