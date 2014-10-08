{% if m.acl.use.mod_oembed %}
<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Fix embedded videos" postback="fix_missing" delegate=`mod_oembed` %} 
        <span class="help-block">{_ Attempt to fix embedded videos for which OEmbed embedding has failed previously. _}</span>
    </div>
</div>
{% endif %}
