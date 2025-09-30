<div class="row">
    <div class="col-lg-6 col-md-6">
        {% include "_admin_dashboard_texts.tpl" %}
        {% include "_admin_dashboard_persons.tpl" %}
    </div>
    <div class="col-lg-6 col-md-6">
        {% include "_admin_dashboard_locations.tpl" %}
        {% include "_admin_dashboard_events.tpl" %}
        {% live template="_admin_dashboard_media.tpl" topic="bridge/origin/model/media/event/+/update" %}
    </div>
</div>
