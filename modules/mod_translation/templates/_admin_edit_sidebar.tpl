{% extends "admin_edit_widget_std.tpl" %}

{# Sidebar widget for editing rsc transalations #}

{% block widget_title %}{_ Translations _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-translations{% endblock %}

{% block widget_content %}
<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{_ Help about translations. _}', text: '{_ The title, body and other texts can be translated in different languages. Here you can select which languages will be used. _}'" title="{_ Translate this page in other languages. _}"><i class="icon-question-sign icon-white"></i></a>
</div>

{% include "_translation_edit_languages.tpl" %}

<div class="control-group">
    <button class="btn" id="{{ #copy }}">{_ Copy translation _}…</button>
    {% wire id=#copy action={dialog_open title=_"Copy translation" template="_dialog_language_copy.tpl"} %}
</div>
{% endblock %}
