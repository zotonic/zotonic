{% if m.acl.use.mod_oembed %}
<div class="form-group">
    <div>
        {% button class="btn btn-outline-secondary" text=_"Fix embedded videos" postback="fix_missing" delegate=`mod_oembed` %}
        <p class="help-block">{_ Attempt to fix embedded videos for which OEmbed embedding has failed previously. _}</p>
    </div>
</div>
{% endif %}
