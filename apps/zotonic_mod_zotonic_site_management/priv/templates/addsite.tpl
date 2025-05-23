{% extends "base.tpl" %}

{% block title %}Zotonic {_ Add Site _}{% endblock %}

{% block content %}
<div class="col-md-8">
    <div class="panel panel-default">
        <div class="panel-heading">
            <h3 class="panel-title">{_ Add or checkout a new site _}</h3>
        </div>
        <div class="panel-body">
            {% wire id=#addsite
                    type="submit"
                    postback=`addsite`
                    delegate=`mod_zotonic_site_management`
                    action={mask target=#addsite}
            %}
            <form id="{{ #addsite }}" method="POST" action="postback" class="form-horizontal">

                {% include "_password_autocomplete_off.tpl" %}

                <!-- Site and host name -->

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        {_ Site name _}
                    </label>
                    <div class="col-sm-7">
                        <input class="form-control" type="text" id="{{ #sitename }}" name="sitename" value="" placeholder="sitename">
                        {% validate id=#sitename name="sitename"
                            type={format pattern="^[a-z][a-z0-9_]*$" failure_message=_"Please use only lowercase a…z and 0…9 characters."}
                            type={presence}
                        %}
                    </div>
                </div>

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        {_ Host name _}
                    </label>
                    <div class="col-sm-7">
                        <input class="form-control" type="text" id="{{ #hostname }}" name="hostname" value="" placeholder="www.example.com">
                        {% validate id=#hostname name="hostname"
                            type={format pattern="^[a-z0-9-]+(\\.[a-z0-9-]+)+$" failure_message=_"Enter a valid hostname like \"www.example.com\" or \"test.local\"."}
                            type={presence}
                        %}

                        <p class="help-block">
                            {_ Make sure that this hostname has a valid DNS entry or is added to your <tt>/etc/hosts</tt> file. _}
                        </p>

                    </div>
                </div>

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        {_ Skeleton _}
                    </label>
                    <div class="col-sm-7">
                        <select id="{{ #skel }}" name="skel" class="form-control">
                            <option value=""></option>
                            <option value="blog">{_ Basic blogging site _}</option>
                            <option value="empty">{_ Empty site, no templates etc. _}</option>
                            <option value="nodb">{_ Empty site without database connection _}</option>
                        </select>
                        {% validate id=#skel name="skel" type={presence} %}
                        <p class="help-block">{_ The skeleton provides templates and content for a site. _}</p>
                    </div>
                </div>

                <!-- Git -->

                <fieldset>
                    <legend>{_ Checkout existing site from Git _}</legend>

                    <div class="form-group">
                        <div class="col-sm-7 col-sm-offset-3">
                            <p class="help-block">
                                {_ If you are creating a new site, ignore this section. If you have an existing Zotonic site on a git repo, you can optionally specify it here. in which case Zotonic will check that out instead of creating a new site. _}
                            </p>
                        </div>
                    </div>

                    <div class="form-group">
                        <div class="col-sm-7 col-sm-offset-3">
                            <a href="#" id="{{ #gita }}">{_ Checkout an existing site from Git _}</a>
                            {% wire id=#gita action={toggle target=#git} %}
                        </div>
                    </div>

                    <div id="{{ #git }}" style="display: none">
                        <div class="form-group">
                            <div class="col-sm-7 col-sm-offset-3">
                                <p class="help-block">
                                    {_ <b>Note that Zotonic only handles initial check out</b>, after which it is expected that you will handle further git interactions yourself.) _}
                                </p>
                                <p class="help-block">
                                    {_ The config file in <tt>priv/zotonic_site.config</tt> will be overwritten with the database settings from above. _} {_ To add config files to your git repo, add them to <tt>priv/config.d/</tt> directory of your site. _}
                                </p>
                                </p>
                            </div>
                        </div>

                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Git URL _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #giturl }}" name="giturl" value="" placeholder="https://github.com/foo/bar.git">
                            </div>
                        </div>
                    </div>
                </fieldset>

                <!-- Database connection -->

                <fieldset>
                    <legend>{_ PostgreSQL database connection _}</legend>

                    <div class="form-group">
                        <div class="col-sm-7 col-sm-offset-3">
                            <p class="help-block">
                                {_ PostgreSQL database, username and password. The schema defaults to the site name filled in above. _}

                                {% if m.config.zotonic.dbdatabase.value as db %}
                                    {% trans "The default database for the schema is <b>{db}</b>."
                                             db=db|escape
                                    %}
                                {% endif %}

                            </p>
                        </div>
                    </div>

                    {% if m.config.zotonic.dbdatabase.value %}
                        <div class="form-group">
                            <div class="col-sm-7 col-sm-offset-3">
                                <a href="#" id="{{ #dba }}">{_ Configure PostgreSQL Database Connection _}</a>
                                {% wire id=#dba action={toggle target=#db} %}
                            </div>
                        </div>
                    {% endif %}

                    <div id="{{ #db }}" {% if m.config.zotonic.dbdatabase.value %}style="display:none"{% endif %}>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Database _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbdatabase }}" name="dbdatabase" value="" placeholder="{{ m.config.zotonic.dbdatabase.value|escape }}">
                                {% validate id=#dbdatabase name="dbdatabase"
                                    type={format pattern="^[A-Za-z0-9_]*$" failure_message=_"Please use only a…z and 0…9 characters."}
                                %}
                            </div>
                        </div>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Host _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbhost }}" name="dbhost" value="" placeholder="{{ m.config.zotonic.dbhost.value|escape }}">
                                {% validate id=#dbhost name="dbhost"
                                    type={format pattern="^[a-z0-9-]+(\\.[a-z0-9-]+)*$" failure_message=_"Please use a valid hostname."}
                                %}
                            </div>
                        </div>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Port _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbport }}" name="dbport" value="" placeholder="{{ m.config.zotonic.dbport.value|escape }}">
                                {% validate id=#dbport name="dbport"
                                    type={format pattern="^[0-9]+$" failure_message=_"The port must be numerical."}
                                %}
                            </div>
                        </div>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Schema _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbschema }}" name="dbschema" value="" placeholder="[sitename]">
                                {% validate id=#dbschema name="dbschema"
                                    type={format pattern="^[A-Za-z0-9_]*$" failure_message=_"Please use only a…z and 0…9 characters."}
                                %}
                            </div>
                        </div>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ User _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbuser }}" name="dbuser" value="" placeholder="{{ m.config.zotonic.dbuser.value|escape }}">
                                {% validate id=#dbuser name="dbuser"
                                    type={format pattern="^[A-Za-z0-9_.]*$" failure_message=_"Please only use: a…z, 0…9, and . (period)."}
                                %}
                            </div>
                        </div>
                        <div class="form-group">
                            <label class="col-sm-3 control-label">
                                {_ Password _}
                            </label>
                            <div class="col-sm-7">
                                <input class="form-control" type="text" id="{{ #dbpassword }}" name="dbpassword" value="" placeholder="&bull;&bull;&bull;&bull;&bull;&bull;&bull;&bull;">
                            </div>
                        </div>
                    </div>
                </fieldset>

                <!-- Submit -->

                <hr>

                <div class="form-group">
                    <div class="col-sm-7 col-sm-offset-3">
                        <button type="submit" class="btn btn-primary">{_ Add Site _}</button>
                        <a href="{% url zotonic_status %}" class="btn btn-default">{_ Cancel _}</a>
                    </div>
                </div>
            </form>
        </div>
    </div>
</div>
{% endblock %}

{% block sidebar %}
<div class="col-md-4">
    <div id="notices"></div>
</div>
{% endblock %}
