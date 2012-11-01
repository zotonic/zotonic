{% extends "admin_base.tpl" %}

{% block title %} {_ Translation _} {% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ Languages overview _}</h2>

    <p>{_ All languages known to the system. You can add or remove languages. _}
    <br/>{_ Enabled languages show up in the language selection menu. The default language is used for new visitors without a selected language. _}</p>

    <div class="well">
        {% button class="btn btn-primary" text=_"Add language" 
            action={dialog_open title=_"Add language" template="_dialog_language_edit.tpl" new}
            %}
    </div>

    <table class="table table-striped">
        <thead>
            <tr>
                <th width="10%">{_ Enabled _}</th>
                <th width="10%">{_ Default _}</th>
                <th width="15%">{_ ISO Code _}</th>
                <th width="65%">{_ Language _}</th>
            </tr>
        </thead>

        <tbody>
        {% with m.config.i18n.language.value as default_code %}
        {% for code, lang in m.config.i18n.language_list.list %}
        <tr id="{{ #li.code }}">
            <td>
                <input type="checkbox" id="{{ #enabled.code }}" name="is_enabled" value="1"
                       {% if lang.is_enabled %}checked="checked"{% endif %} />
                {% wire id=#enabled.code postback={language_enable code=code} delegate="mod_translation" %}
            </td>
            <td>
                <input type="radio" id="{{ #default.code }}" name="is_default" value="{{ code }}"
                       {% if code == default_code %}checked="checked"{% endif %} />
                {% wire id=#default.code postback={language_default code=code} delegate="mod_translation" %}
            </td>

            <td class="clickable" id="{{ #a.code }}">{{ code|default:"-" }}</td>
            <td class="clickable" id="{{ #b.code }}">
                <div class="pull-right">
                        {% button class="btn btn-mini" text=_"Delete" 
                                action={dialog_open
                                title=_"Delete language"
                                template="_dialog_language_delete.tpl"
                                code=code lang=lang
                                }
                                %}
                        {% button class="btn btn-mini"text=_"Edit" 
                                action={dialog_open 
                                title=_"Edit language"|append:": "|append:lang.language template="_dialog_language_edit.tpl"
                                code=code lang=lang}
                                %}
                </div>
                {{ lang.language|default:"-" }}
            </td>
            {% wire id=#a.code action={dialog_open title=_"Edit language" template="_dialog_language_edit.tpl" code=code lang=lang} %}
            {% wire id=#b.code action={dialog_open title=_"Edit language" template="_dialog_language_edit.tpl" code=code lang=lang} %}
        </tr>
        {% empty %}
        <tr>
            <td colspan="4">
                {_ No languages configured. _}
            </td>
        </tr>
        {% endfor %}
        {% endwith %}
        </tbody>
    </table>
    
</div>

<div>
    <h3>{_ Translation configuration and tools _}</h3>

    <div class="well">

        <div class="control-group">
            <div class="controls">
                {% button class="btn" text=_"Generate .pot files" 
                    action={postback postback="translation_generate" delegate="mod_translation"} %}
                <span class="help-inline">{_ Scan all templates for translation tags and generate .pot files that can be used for translating the templates. _}</span>
            </div>
        </div>
        <div class="control-group">
            <div class="controls">
                {% button class="btn" text=_"Reload Translations" 
                    action={postback postback="translation_reload" delegate="mod_translation"} %}
                <span class="help-inline">{_ Reload all translations from the modules and site. All templates will be recompiled. _}</span>
            </div>
        </div>
        <div class="control-group">
            <div class="controls">
                <a class="btn" href="{% url admin_translation_status %}" class="button">{_ Translation status _}</a>
                <span class="help-inline">{_ Show per module how much of the templates are translated. _}</span>
            </div>
        </div>
    </div>
    
</div>

{% endblock %}
