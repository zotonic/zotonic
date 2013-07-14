{% if m.acl.use.mod_oembed %}
<div class="control-group">
    <div class="controls">
        {% button class="btn" text=_"Fix embedded videos" postback="fix_missing" delegate=`mod_oembed` %} 
        <span class="help-inline">{_ Attempt to fix embedded videos for which OEmbed embedding has failed previously. _}</span>
    </div>
</div>
{% endif %}
