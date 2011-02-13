{% if m.acl.use.mod_import_wordpress %}
<h2>{_ Wordpress import _}</h2>
<div class="clearfix">
    {% button class="" text=_"Wordpress import" action={dialog_open title=_"Import WXR file" template="_dialog_import_wordpress.tpl"} %} 
    <span class="expl">{_ Import a Wordpress WXR export file into Zotonic. _}</span>
</div>
{% endif %}
