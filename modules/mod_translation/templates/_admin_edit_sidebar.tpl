{% extends "admin_edit_widget_std.tpl" %}

{# Sidebar widget for editing rsc transalations #}

{% block widget_title %}{_ Translations _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
{% with m.rsc[id].language as r_lang %}
<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{_ Help about translations. _}', text: '{_ The title, body and other texts can be translated in different languages. Here you can select which languages will be used. _}'" title="{_ Translate this page in other languages. _}"><i class="icon-question-sign icon-white"></i></a>
</div>

<div class="control-group">
    <div class="controls">
        {% for code, lang in languages %}
        {% if lang.is_enabled %}
        <label class="inline checkbox">
	    <input type="checkbox" id="{{ #language.code }}" name="language" value="{{ code }}"
	           {% if code|member:r_lang or (not r_lang and z_language == code) %}checked="checked"{% endif %} /> 
	    <span {% include "_language_attrs.tpl" language=code %}>{{ lang.language }}</span>
        </label>
        {% wire id=#language.code 
        action={toggle selector=[".tab-",code|make_list]}
        %}
        {% endif %}

        {% empty %}
        <label><input type="checkbox" checked="checked" disabled="disabled"> {{ z_language }}</label>
        {% endfor %}
    </div>
</div>
{% endwith %}
{% endblock %}
