{% if m.acl.use.mod_oembed %}
{% compose "_admin_maintenance_action.tpl" title=_"Embedded videos" description=_"Attempt to fix embedded videos for which OEmbed embedding has failed previously." %}
    {% block action %}
        {% button class="btn btn-default" text=_"Fix embedded videos" postback="fix_missing" delegate=`mod_oembed` %}
    {% endblock %}
{% endcompose %}
{% endif %}
