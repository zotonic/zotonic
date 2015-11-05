<div class="well z-button-row">
    {% button
        class="btn btn-primary" 
        text=_"Make a new page or media" 
        action={
            dialog_new_rsc
            title=""
            cat=q.qcat
        }
    %}
    {% all include "_admin_make_page_buttons.tpl" %}

    <a class="btn btn-default" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
    <a class="btn btn-default" href="{% url admin_media %}">{_ All media _}</a>
    {% all include "_admin_extra_buttons.tpl" %}
</div>
