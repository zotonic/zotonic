{% extends "admin_base.tpl" %}

{% block title %}{_ Development _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Site Development _}</h2>

    <p>{_ Tools and settings that are useful for site development. _}</p>
</div>

<div class="widget">
    <div class="widget-header">
        {_ Settings _}
    </div>
    <div class="widget-content">

        {% wire id="tpldbg"
            action={config_toggle module="mod_development" key="debug_includes"}
            action={admin_tasks task='flush'}
        %}
        <div class="form-check">
            <input type="checkbox" id="tpldbg" class="form-check-input" value="1" {% if m.development.debug_includes %}checked="checked"{% endif %} />
            <label for="tpldbg" class="form-check-label">{_ Show paths to included template files in generated templates _}</label>
        </div>

        {% wire id="blkdbg"
            action={config_toggle module="mod_development" key="debug_blocks"}
            action={admin_tasks task='flush'}
        %}
        <div class="form-check">
            <input type="checkbox" id="blkdbg" class="form-check-input" value="1" {% if m.development.debug_blocks %}checked="checked"{% endif %} />
            <label for="blkdbg" class="form-check-label">{_ Show defined blocks in generated templates _}</label>
        </div>

        {% wire id="libsep"
            action={config_toggle module="mod_development" key="libsep"}
        %}
        <div class="form-check">
            <input type="checkbox" id="libsep" class="form-check-input" value="1" {% if m.development.libsep %}checked="checked"{% endif %} />
            <label for="libsep" class="form-check-label">{_ Download CSS and JavaScript files as separate files. Don’t combine them in one URL. _}</label>
        </div>

        {% wire id="livereload"
            action={config_toggle module="mod_development" key="livereload"}
            action={script script="
                if ($('#livereload').is(':checked') && !$('#libsep').is(':checked')) {
                    $('#libsep').click();
                };" }
        %}
        <div class="form-check">
            <input type="checkbox" id="livereload" class="form-check-input" value="1" {% if m.development.livereload %}checked="checked"{% endif %} />
            <label for="livereload" class="form-check-label">{_ Live reload of changed CSS files. Reload page on change of templates or JavaScript. _}</label>
        </div>

        {% wire id="devapi"
            action={config_toggle module="mod_development" key="enable_api"}
        %}
        <div class="form-check">
            <input type="checkbox" id="devapi" class="form-check-input" value="1" {% if m.development.enable_api %}checked="checked"{% endif %} />
            <label for="devapi" class="form-check-label">{_ Enable API to recompile and build Zotonic _}</label>
        </div>

        {% wire id="nocache"
            action={config_toggle module="mod_development" key="nocache"}
        %}
        <div class="form-check">
            <input type="checkbox" id="nocache" class="form-check-input" value="1" {% if m.development.nocache %}checked="checked"{% endif %} />
            <label for="nocache" class="form-check-label">{_ Disable caching by the <tt>{% cache %}</tt> tag. _}</label>
        </div>

        {% if m.modules.provided.server_storage %}
            {% wire id="dbtrace"
                    postback=`dbtrace_toggle`
                    delegate=`z_development_dbtrace`
            %}
            <div class="form-check">
                <input type="checkbox" id="dbtrace" class="form-check-input" value="1" {% if m.development.is_dbtrace %}checked="checked"{% endif %} />
                <label for="dbtrace" class="form-check-label">{_ Trace all database queries for the current session _}</label>
            </div>
        {% else %}
            <p class="help-block">
                <br>
                {_ Enable <tt>mod_server_storage</tt> to use database query tracing. _}
            </p>
        {% endif %}
    </div>
</div>

<div class="widget">
    <div class="widget-header">
        {_ Template debugging _}
    </div>
    <div class="widget-content">
        <p>{_ Find a template, check which template will be selected _}</p>

        {% wire id="explain-tpl" type="submit"
                postback=`explain_tpl`
                delegate=`z_development_template`
        %}
        <form id="explain-tpl" class="row" method="GET" action="postback">
            <div class="form-group col-sm-3">
                <select class="form-control" name="tpl_cat">
                    <option value="">{_ Optional category for catinclude _}</option>
                    <option disabled></option>
                    {% for c in m.category.tree_flat %}
                        <option value="{{ c.id.name }}">{{ c.indent }}{{ c.id.name }}</option>
                    {% endfor%}
                </select>
            </div>
            <div class="form-group col-sm-2">
                <input class="form-control" type="text" name="tpl_name" placeholder="foo.tpl" value="" />
            </div>
            <button class="btn btn-primary col-sm-1" type="submit">{_ Find _}</button>
        </form>

        <div id="explain-tpl-output" style="display:none"></div>

        <hr/>

        <p>
            <a href="{% url admin_development_templates_xref %}">{_ Cross-reference check of templates _} &gt;</a>
        </p>
        <p class="help-block">
            {% trans "All templates are checked for missing includes or missing <tt>{ext}</tt> template references."
                ext="extends/overrules"
            %}
            {_ All templates will be compiled. _}
        </p>

        <hr/>

        <p>
            <a href="{% url admin_development_templates_trace %}">{_ Live dependency graph of templates _} &gt;</a>
        </p>
        <p class="help-block">{_ Traces all templates for the current session-id, or all sessions, and renders them as a graph. _}</p>

        <hr/>

        <p>
            <a href="{% url admin_development_templates_graph %}">{_ Dependency graph of all available templates _} &gt;</a>
        </p>
        <p class="help-block">{_ Calculate and visualize a dependency graph of all templates. _}
        {_ All templates will be compiled. _}</p>

        <hr/>

        <p>
            {% button class="btn btn-primary" text=_"Recompile templates"
                      action={admin_tasks task="templates_reset"}
            %}
            {% button class="btn btn-default" text=_"Rescan modules"
                      action={module_rescan}
            %}
        </p>
        <p class="help-block">{_ Force a recompilation of all templates. This fixes any issues where the compiled templates might be out of sync with the template index. _}</p>
        </p>
    </div>
</div>

<div class="widget">


    <div class="widget-header">
        {_ Dispatch rule debugging _}
    </div>
    <div class="widget-content">
        <p>
            <a href="{% url admin_development_dispatch_details %}">{_ View all dispatch rules  _} &gt;</a>
        </p>
        <p class="help-block">{_ View all dispatch rules and hostnames usable in the site. _}</p>

        <hr />

        <p>{_ Match a request URL, display matched dispatch rule. _}</p>

        {% wire id="explain-dispatch" type="submit"
                postback=`explain_dispatch`
                delegate=`z_development_dispatch`
        %}
        <form id="explain-dispatch" class="row" method="GET" action="postback">
            <div class="form-group col-sm-3">
                <select id="explain_protocol" name="explain_protocol" class="col-md-4 form-control">
                    <option value="https">https://{{ m.site.hostname }}</option>
                    <option value="http">http://{{ m.site.hostname }}</option>
                </select>
            </div>
            <div class="form-group col-sm-2">
                <input class="form-control" type="text" id="explain_req" name="explain_req" placeholder="/foo/bar" value="" />
            </div>
            <button class="btn btn-primary col-sm-1" type="submit">{_ Explain _}</button>
        </form>
        <br>

        <div id="explain-dispatch-output" style="display:none"></div>
    </div>
</div>


<div class="widget">
    <div class="widget-header">
        {_ Introspection _}
    </div>
    <div class="widget-content">
        <p>{_ Show internals of Zotonic and the modules _}</p>

        <p>
            <a href="{% url admin_development_observers %}">{_ Show an overview of all observers _} &gt;</a>
        </p>
    </div>
</div>

{% endblock %}
