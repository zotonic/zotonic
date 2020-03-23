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
                <th width="5%">{_ Default _}</th>
                <th width="5%">{_ Enabled _}</th>
                <th width="5%">{_ Editable _}</th>
                <th width="5%">{_ Disabled _}</th>
                <th width="20%">{_ Language _}</th>
                <th width="10%">{_ Code _}</th>
                <th width="10%">{_ Region _}</th>
                <th width="10%">{_ Script _}</th>
                <th></th>
            </tr>
        </thead>

        <tbody id="translation-language-status">
            {% include "_translation_language_status.tpl" %}
        </tbody>
    </table>

    <p class="help-block">
        {_ If a language is enabled or editable then texts for that language are editable in the admin. _}<br>
    </p>

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
                    {# TODO: make a model call to fetch this list from the database #}

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

        {% if m.acl.is_admin or m.acl.use.mod_development %}
            <div class="form-group">
                <div>
                    {% button class="btn btn-default" text=_"Generate .pot files"
                        postback={translation_generate} delegate="mod_translation" %}
                    <span class="help-block">{_ Scan all templates for translation tags and generate .pot files that can be used for translating the templates. The <a href="http://docs.zotonic.com/en/latest/developer-guide/translation.html">gettext package must be installed</a>._}</span>
                </div>
            </div>
        {% endif %}

        <div class="form-group">
            <div>
                {% button class="btn btn-default" text=_"Reload Translations"
                    postback={translation_reload} delegate="mod_translation" %}
                <span class="help-block">{_ Reload all translations from the modules and site. All templates will be recompiled. _}</span>
            </div>
        </div>

    </div>

</div>

{% endblock %}
