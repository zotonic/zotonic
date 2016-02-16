{% extends "admin_base.tpl" %}

{% block title %} {_ Translation _} {% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Languages overview _}</h2>

    <p>{_ All languages known to the system. You can add or remove languages. _}
        <br/>{_ Enabled languages show up in the language selection menu. The default language is used for new visitors without a selected language. _}</p>
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
                    <th width="15%">{_ Language _}</th>
                    <th width="15%">{_ ISO Code _}</th>
                    <th width="15%">{_ Fallback language _}</th>
                    <th></th>
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
                            <td class="clickable" id="{{ #b.code }}">
                                {{ lang.language|default:"-" }}
                            </td>
                            <td class="clickable" id="{{ #a.code }}">{{ code|default:"-" }}</td>
                            <td class="clickable" id="{{ #a.fallback }}">{{ lang.fallback|default:"-" }}</td>
                            <td class="clickable">
                                <div class="pull-right">
                                    {% button class="btn btn-default btn-xs" text=_"Delete" 
                                        action={dialog_open
                                            title=_"Delete language"
                                            template="_dialog_language_delete.tpl"
                                            code=code lang=lang
                                        }
                                    %}
                                    {% button class="btn btn-default btn-xs"text=_"Edit" 
                                        action={dialog_open 
                                            title=_"Edit language"|append:": "|append:lang.language template="_dialog_language_edit.tpl"
                                            code=code lang=lang fallback=lang.fallback}
                                    %}
                                </div>
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
        <div class="row">
            <div class="col-md-6">
                <div class="form-group">
                    {% wire id=#redir 
                        action={config_toggle module="mod_translation" key="rewrite_url"}
                    %}
                    <label class="checkbox-inline">
                        <input type="checkbox" id="{{ #redir }}" value="1"
                        {% if m.config.mod_translation.rewrite_url.value
                            or m.config.mod_translation.rewrite_url.value|is_undefined %}checked="checked"{% endif %}
                        />
                        <span>{_ Put the current language in the URL _} (<tt>/en/page/...</tt>).</span>
                    </label>
                </div>

                <div class="form-group">
                    {% wire id=#force 
                        action={config_toggle module="mod_translation" key="force_default"}
                    %}
                    <label class="checkbox-inline">
                        <input type="checkbox" id="{{ #force }}" value="1"
                        {% if m.config.mod_translation.force_default.value %}checked="checked"{% endif %}
                        />
                        <span>{_ Set initial language to the default language. _}</span>
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
                    {% with m.config.i18n.language_stemmer.value|default:m.config.i18n.language.value as stm %}
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

    <div class="well">

        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Generate .pot files" 
                    action={postback postback="translation_generate" delegate="mod_translation"} %}
                <span class="help-block">{_ Scan all templates for translation tags and generate .pot files that can be used for translating the templates. _}</span>
            </div>
        </div>
        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Reload Translations" 
                    action={postback postback="translation_reload" delegate="mod_translation"} %}
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
