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
        <label class="checkbox">
            <input type="checkbox" id="tpldbg" value="1" {% if m.development.debug_includes %}checked="checked"{% endif %} />
            {_ Show paths to included template files in generated templates _}
        </label>

        {% wire id="blkdbg"
            action={config_toggle module="mod_development" key="debug_blocks"}
            action={admin_tasks task='flush'}
        %}
        <label class="checkbox">
            <input type="checkbox" id="blkdbg" value="1" {% if m.development.debug_blocks %}checked="checked"{% endif %} />
            {_ Show defined blocks in generated templates _}
        </label>

        {% wire id="libsep"
            action={config_toggle module="mod_development" key="libsep"}
        %}
        <label class="checkbox">
            <input type="checkbox" id="libsep" value="1" {% if m.development.libsep %}checked="checked"{% endif %} />
            {_ Download CSS and JavaScript files as separate files. Don’t combine them in one URL. _}
        </label>

        {% wire id="livereload"
            action={config_toggle module="mod_development" key="livereload"}
            action={script script="
                if ($('#livereload').is(':checked') && !$('#libsep').is(':checked')) {
                    $('#libsep').click();
                };" }
        %}
        <label class="checkbox">
            <input type="checkbox" id="livereload" value="1" {% if m.development.livereload %}checked="checked"{% endif %} />
            {_ Live reload of changed CSS files. Reload page on change of templates or JavaScript. _}
        </label>

        {% wire id="devapi"
            action={config_toggle module="mod_development" key="enable_api"}
        %}
        <label class="checkbox">
            <input type="checkbox" id="devapi" value="1" {% if m.development.enable_api %}checked="checked"{% endif %} />
            {_ Enable API to recompile and build Zotonic _}
        </label>

        {% wire id="nocache"
            action={config_toggle module="mod_development" key="nocache"}
        %}
        <label class="checkbox">
            <input type="checkbox" id="nocache" value="1" {% if m.development.nocache %}checked="checked"{% endif %} />
            {_ Disable caching by the <tt>{% cache %}</tt> tag. _}
        </label>

        {% if m.modules.provided.server_storage %}
            {% wire id="dbtrace"
                    postback=`dbtrace_toggle`
                    delegate=`z_development_dbtrace`
            %}
            <label class="checkbox">
                <input type="checkbox" id="dbtrace" value="1" {% if m.development.is_dbtrace %}checked="checked"{% endif %} />
                {_ Trace all database queries for the current session _}
            </label>
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
        <form id="explain-tpl" class="form-inline" method="GET" action="postback">
            <div class="form-group">
                <select class="form-control" name="tpl_cat">
                    <option value="">{_ Optional category for catinclude _}</option>
                    <option disabled></option>
                    {% for c in m.category.tree_flat %}
                        <option value="{{ c.id.name }}">{{ c.indent }}{{ c.id.name }}</option>
                    {% endfor%}
                </select>
            </div>
            <div class="form-group">
                <input class="form-control" type="text" name="tpl_name" placeholder="foo.tpl" value="" />
            </div>
            <button class="btn btn-primary" type="submit">{_ Find _}</button>
        </form>

        <div id="explain-tpl-output" style="display:none"></div>

        <hr/>

        <p><a href="{% url admin_development_templates %}">Show which files are included in a template compilation</a></p>
        <p class="help-block">At times it can be confusing which templates are actually used during a template compilation.  Here you can see which files are included whilst compiling a template.</p>

    </div>
</div>

<div class="widget">
    <div class="widget-header">
        {_ Dispatch rule debugging _}
    </div>
    <div class="widget-content">
        <p>{_ Match a request URL, display matched dispatch rule. _}</p>

        {% wire id="explain-dispatch" type="submit"
                postback=`explain_dispatch`
                delegate=`z_development_dispatch`
        %}
        <form id="explain-dispatch" class="form-inline" method="GET" action="postback">
            <div class="form-group">
                <select id="explain_protocol" name="explain_protocol" class="col-md-4 form-control">
                    <option value="https">https://{{ m.site.hostname }}</option>
                    <option value="http">http://{{ m.site.hostname }}</option>
                </select>
            </div>
            <div class="form-group">
                <input class="form-control" type="text" id="explain_req" name="explain_req" placeholder="/foo/bar" value="" />
            </div>
            <button class="btn btn-primary" type="submit">{_ Explain _}</button>
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
            <a href="{% url admin_development_observers %}">{_ Show an overview of all observers. _}</a>
        </p>
    </div>
</div>

{% endblock %}
