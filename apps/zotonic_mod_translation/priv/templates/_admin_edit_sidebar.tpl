{% extends "admin_edit_widget_std.tpl" %}

{# Sidebar widget for editing rsc transalations #}

{% block widget_title %}
{_ Translations _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{_ Help about translations _}', text: '{_ The title, body and other texts can be translated in different languages. Here you can select which languages will be used. _}'" title="{_ Translate this page in other languages. _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-translations{% endblock %}

{% block widget_content %}
{% include "_translation_edit_languages.tpl" %}

<div class="form-group">
    <button class="btn btn-default" id="{{ #copy }}">{_ Copy translation _}…</button>
    {% wire id=#copy
        action={dialog_open
            title=_"Copy translation"
            template="_dialog_language_copy.tpl"
            center=0
        }
    %}
</div>
{% endblock %}
