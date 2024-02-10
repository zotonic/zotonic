{% extends "admin_base.tpl" %}

{% block title %} {_ Translation _} {% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Languages overview _}</h2>
    <p>
        {_ Part of _} <a href="http://zotonic.com/docs/latest/ref/modules/mod_translation.html">mod_translation</a>. {_ See also: _} <a href="http://docs.zotonic.com/en/latest/developer-guide/translation.html">{_ Developer Documentation _}</a></li>
    </p>
</div>

<div class="well z-button-row">
    {% button class="btn btn-primary" text=_"Add language"
        action={dialog_open title=_"Add language" template="_dialog_language_edit.tpl" new}
    %}
</div>

<div class="widget">
    <div class="widget-content">

        <p class="help-block">
            {_ Drag languages to define the preferred order, the first 'view' language will be the default language. _}<br>
            {_ If a language is set to 'view' or 'editable' then texts for that language are editable in the admin. _}<br>
            {_ If a language is set to 'off' then edited texts will loose their translation in that language. _}
        </p>

        {% wire id="translation-language-form"
                type="submit"
                postback={language_list}
                delegate=`mod_translation`
        %}
        <form id="translation-language-form" action="postback">
            <table class="table table-striped">
                <thead>
                    <tr>
                        <th width="40px"></th>
                        <th width="5%">{_ View _}</th>
                        <th width="5%">{_ Editable _}</th>
                        <th width="5%">{_ Off _}</th>
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
        </form>

        {% javascript %}
            $('#translation-language-status').sortable({
                handle: '.drag-handle',
                items: 'tr',
                revert: 'invalid',
                axis: 'y',
                start: function(event, ui) {
                    $(this).find(".btn").hide();
                },
                stop: function(event, ui) {
                    $(this).find(".btn").show();
                    $('#translation-language-form').trigger('submit');
                }
            });

            $('#translation-language-form').on('click', 'input[type="radio"]', function(e) {
                $('#translation-language-form').trigger('submit');
            });
        {% endjavascript %}
    </div>
</div>

<div class="widget">
    <div class="widget-header">
        {_ Configuration _}
    </div>
    <div class="widget-content">
        <div class="row">
            <div class="col-md-6">
                <div class="form-group">
                    {% wire id=#redir
                        postback={toggle_url_rewrite}
                        delegate="mod_translation"
                    %}
                    <div class="form-check">
                        <input type="checkbox" id="{{ #redir }}" class="form-check-input" value="1"
                        {% if m.translation.rewrite_url %}checked="checked"{% endif %}
                        />
                        <label for="{{ #redir }}" class="form-check-label"><span>{_ Show the language in the URL _} (<tt>/en/page/...</tt>).</span></label>
                    </div>
                </div>

                <div class="form-group">
                    {% wire id=#force
                        action={config_toggle module="mod_translation" key="force_default"}
                    %}
                    <div class="form-check">
                        <input type="checkbox" id="{{ #force }}" class="form-check-input" value="1"
                        {% if m.translation.force_default %}checked="checked"{% endif %}
                        />
                        <label for="{{ #force }}" class="form-check-label"><span>
                            {_ For new visitors, set the language to the default language _}
                            ({{ m.translation.language_list_configured[m.translation.default_language].name }})
                        </span></label>
                    </div>
                </div>
            </div>
            <div class="col-md-6">
                <div class="form-group">
                    {# See z_pivot_rsc for the list of languages #}
                    {# TODO: make a model call to fetch this list from the database #}
                    <div class="row">
                        <label for="{{ #stemmer }}" class="control-label col-md-5">{_ Language used for stemming of the search indices _}</label>

                        {% wire id=#stemmer type="change"
                            action={config_toggle module="i18n" key="language_stemmer"}
                        %}
                        {% with m.translation.language_stemmer as stm %}
                            <div class="col-md-7">
                            <select id="{{ #stemmer }}" class="form-select ">
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
                        </div>
                    </div>
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
</div>

<div class="widget">
    <div class="widget-header">
        {_ For Developers _}
    </div>
    <div class="widget-content">
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
