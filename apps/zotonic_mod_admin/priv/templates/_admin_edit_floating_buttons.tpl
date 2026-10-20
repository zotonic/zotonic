<div class="navbar admin-navbar-fixed-save">
    {% button type="submit" id="save_stay_float" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
    {% if id.page_url and id.is_editable %}
        {% button type="submit" id="save_view_float" class="btn btn-default" text=_"Save &amp; view" title=_"Save and view the page." %}
    {% endif %}
    {% include "_admin_edit_preview_button.tpl" %}
</div>
