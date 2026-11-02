{% if m.acl.use.mod_oembed %}
{% compose "_admin_maintenance_panel.tpl" title=_"Media maintenance" %}
{% block tools %}

<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Fix embedded videos" postback="fix_missing" delegate=`mod_oembed` %}
        <span class="help-block">{_ Attempt to fix embedded videos for which OEmbed embedding has failed previously. _}</span>
    </div>
</div>
{% endblock %}
{% endcompose %}
{% endif %}
