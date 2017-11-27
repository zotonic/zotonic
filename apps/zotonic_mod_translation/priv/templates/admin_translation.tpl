{% extends "admin_base.tpl" %}

{% block title %} {_ Translation _} {% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Languages overview _}</h2>
    <p>
        {_ Part of _} <a href="http://zotonic.com/docs/latest/ref/modules/mod_translation.html">mod_translation</a>. {_ See also: _} <a href="http://zotonic.com/docs/latest/developer-guide/translation.html">{_ Developer Documentation _}</a></li>
    </p>
</div>

<div class="well">
    {% button class="btn btn-primary" text=_"Add language"
        action={dialog_open title=_"Add language" template="_dialog_language_edit.tpl" new}
    %}
</div>

<div>
    <table class="table table-striped">
        <thead>
            <tr>
                <th width="10%">{_ Enabled _}</th>
                <th width="10%">{_ Default _}</th>
                <th width="20%">{_ Language _}</th>
                <th width="10%">{_ Code _}</th>
                <th width="10%">{_ Region _}</th>
                <th width="10%">{_ Script _}</th>
                <th></th>
            </tr>
        </thead>

        <tbody>
            {% with m.translation.default_language as default_code %}
                {% for code, lang in m.translation.language_list_configured %}
                    <tr id="{{ #li.code }}" class="{% if not lang.is_enabled %}unpublished{% endif %}">
                        <td>
                            <input type="checkbox" id="{{ #enabled.code }}" name="is_enabled" value="1"{% if lang.is_enabled %} checked="checked"{% endif %}{% if m.translation.default_language == code %} disabled="disabled"{% endif %} />
                            {% wire id=#enabled.code postback={language_enable code=code} delegate="mod_translation" %}
                        </td>
                        <td>
                            <input type="radio" id="{{ #default.code }}" name="is_default" value="{{ code }}"
                            {% if code == default_code %}checked="checked"{% endif %} />
                            {% wire id=#default.code postback={language_default code=code} delegate="mod_translation" %}
                        </td>
                        <td>
                            {{ lang.name_en|default:"-" }}
                        </td>
                        <td>
                            {{ code|default:"-" }}
                        </td>
                        <td>
                            {{ lang.region|default:"<span class='text-muted'>-</span>" }}
                        </td>
                        <td>
                            {{ lang.script|default:"<span class='text-muted'>-</span>" }}
                        </td>
                        <td>
                            <div class="pull-right">
                                {% button class="btn btn-default btn-xs" text=_"Remove"
                                    action={
                                        dialog_open
                                        title=_"Remove language"|append:": "|append:lang.name_en
                                        template="_dialog_language_delete.tpl"
                                        code=code
                                        lang=lang
                                    }
                                %}
                                {% button class="btn btn-default btn-xs"text=_"Details"
                                    action={
                                        dialog_open
                                        title=_"Language"|append:": "|append:lang.name_en
                                        template="_dialog_language_edit.tpl"
                                        code=code
                                    }
                                %}
                            </div>
                        </td>
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
    <h3>{_ Configuration _}</h3>

    <div class="well">
        <div class="row">
            <div class="col-md-6">
                <div class="form-group">
                    {% wire id=#redir
                        postback={toggle_url_rewrite}
                        delegate="mod_translation"
                    %}
                    <label class="checkbox-inline">
                        <input type="checkbox" id="{{ #redir }}" value="1"
                        {% if m.translation.rewrite_url %}checked="checked"{% endif %}
                        />
                        <span>{_ Show the language in the URL _} (<tt>/en/page/...</tt>).</span>
                    </label>
                </div>

                <div class="form-group">
                    {% wire id=#force
                        action={config_toggle module="mod_translation" key="force_default"}
                    %}
                    <label class="checkbox-inline">
                        <input type="checkbox" id="{{ #force }}" value="1"
                        {% if m.translation.force_default %}checked="checked"{% endif %}
                        />
                        <span>
                            {_ For new visitors, set the language to the default language _}
                            ({{ m.translation.language_list_configured[m.translation.default_language].name }})
                        </span>
                    </label>
                </div>
            </div>
            <div class="col-md-6">
                <div class="form-group">
                    {# See z_pivot_rsc for the list of languages #}
                    {# TODO: make a model call to fetch this list #}

                    {_ Language used for stemming of the search indices _} &nbsp;

                    {% wire id=#stemmer type="change"
                        action={config_toggle module="i18n" key="language_stemmer"}
                    %}
                    {% with m.translation.language_stemmer as stm %}
                    <select id="{{ #stemmer }}">
                        <option></option>
                        {% for lang, text in [
                                ["dk", _"Danish" ],
                                ["en", _"English" ],
                                ["nl", _"Dutch" ],
                                ["fi", _"Finnish" ],
                                ["fr", _"French" ],
                                ["de", _"German" ],
                                ["hu", _"Hungarian" ],
                                ["it", _"Italian" ],
                                ["no", _"Norwegian" ],
                                ["ro", _"Romanian" ],
                                ["ru", _"Russian" ],
                                ["es", _"Spanish" ],
                                ["se", _"Swedish" ],
                                ["tr", _"Turkish" ]
                            ]
                        %}
                            <option value="{{ lang }}" {% if stm == lang %}selected{% endif %}>{{ text }}</option>
                        {% endfor %}
                    </select>
                    {% endwith %}
                    <p class="help-block">
                        {_ If you change the stemmer language then you will need to reindex all data. _}
                        {% button
                            class="btn btn-default"
                            id="btn-rebuild-indices"
                            text=_"Rebuild search indices"
                            action={
                                admin_tasks
                                task='pivot_all'
                            }
                        %}
                    </p>
                </div>
            </div>
        </div>
    </div>

    <h3>{_ For Developers _}</h3>

    <div class="well">

        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Generate .pot files"
                    postback={translation_generate} delegate="mod_translation" %}
                <span class="help-block">{_ Scan all templates for translation tags and generate .pot files that can be used for translating the templates. The <a href="http://docs.zotonic.com/en/latest/developer-guide/translation.html">gettext package must be installed</a>._}</span>
            </div>
        </div>
        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Reload Translations"
                    postback={translation_reload} delegate="mod_translation" %}
                <span class="help-block">{_ Reload all translations from the modules and site. All templates will be recompiled. _}</span>
            </div>
        </div>
        <div>
            <div>
                <a class="btn btn-default" href="{% url admin_translation_status %}" class="button">{_ Translation status _}</a>
                <span class="help-block">{_ Show per module how much of the templates are translated. _}</span>
            </div>
        </div>

    </div>

</div>

{% endblock %}
