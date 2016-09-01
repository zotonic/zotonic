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
            %}
            <form id="{{ #addsite }}" method="POST" action="postback" class="form-horizontal">

                <!-- Site and host name -->

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        Site name
                    </label>
                    <div class="col-sm-7">
                        <input class="form-control" type="text" id="{{ #sitename }}" name="sitename" value="" placeholder="sitename" />
                        {% validate id=#sitename name="sitename"
                            type={format pattern="^[a-z][a-z0-9_]*$" failure_message=_"Please use only lowercase a…z and 0…9 characters."}
                            type={presence}
                        %}
                    </div>
                </div>

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        Title
                    </label>
                    <div class="col-sm-7">
                        <input class="form-control" type="text" id="{{ #title }}" name="title" value="" placeholder="Descriptive title of your site" />
                    </div>
                </div>

                <div class="form-group">
                    <label class="col-sm-3 control-label">
                        Host name
                    </label>
                    <div class="col-sm-7">
                        <input class="form-control" type="text" id="{{ #hostname }}" name="hostname" value="" placeholder="www.example.com" />
                        {% validate id=#hostname name="hostname"
                            type={format pattern="^[a-z0-9-]+(\\.[a-z0-9-]+)+($:[0-9]+)?$" failure_message=_"Enter a valid hostname like \"www.example.com\" or \"test.dev:8000\"."}
                            type={presence}
                        %}

                        <p class="help-block">
                            {_ Make sure that this hostname has a valid DNS entry or is added to your <tt>/etc/hosts</tt> file. _}
                        </p>

                    </div>
                </div>

                <!-- Git -->

                <fieldset>
                    <legend>{_ Git _}</legend>

                    <div class="form-group">
                        <div class="col-sm-7 col-sm-offset-3">
                            <p class="help-block">
                                {_ Optionally fill in a Git url to checkout the site. An existing <tt>config</tt> file will be overwritten. _}
                                {_ To add config files to your Git repo, place them in the <tt>config.d</tt> directory of your site. _}
                            </p>
                        </div>
                    </div>

                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Git URL
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #giturl }}" name="giturl" value="" placeholder="https://github.com/foo/bar.git" />
                        </div>
                    </div>
                </fieldset>

                <!-- Database connection -->

                <fieldset>
                    <legend>{_ PostgreSQL _}</legend>

                    <div class="form-group">
                        <div class="col-sm-7 col-sm-offset-3">
                            <p class="help-block">
                                {_ Optionally fill in the database connection. Leave empty to use the in Zotonic configured database credentials with the site name as the schema. _}
                            </p>
                        </div>
                    </div>

                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Database
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #dbdatabase }}" name="dbdatabase" value="" placeholder="" />
                            {% validate id=#dbdatabase name="dbdatabase"
                                type={format pattern="^[A-Za-z0-9_]*$" failure_message=_"Please use only a…z and 0…9 characters."}
                            %}
                        </div>
                    </div>
                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Host
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #dbhost }}" name="dbhost" value="" placeholder="" />
                            {% validate id=#dbhost name="dbhost"
                                type={format pattern="^[a-z0-9-]+(\\.[a-z0-9-]+)*$" failure_message=_"Please use a valid hostname."}
                            %}
                        </div>
                    </div>
                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Port
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #dbport }}" name="dbport" value="" placeholder="" />
                            {% validate id=#dbdatabase name="dbdatabase"
                                type={format pattern="^[0-9]+$" failure_message=_"The port must be numerical."}
                            %}
                        </div>
                    </div>
                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Schema
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #dbschema }}" name="dbschema" value="" placeholder="" />
                            {% validate id=#dbschema name="dbschema"
                                type={format pattern="^[A-Za-z0-9_]*$" failure_message=_"Please use only a…z and 0…9 characters."}
                            %}
                        </div>
                    </div>
                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            User
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="text" id="{{ #dbuser }}" name="dbuser" value="" placeholder="" />
                            {% validate id=#dbuser name="dbuser"
                                type={format pattern="^[A-Za-z0-9_]*$" failure_message=_"Please use only a…z and 0…9 characters."}
                            %}
                        </div>
                    </div>
                    <div class="form-group">
                        <label class="col-sm-3 control-label">
                            Password
                        </label>
                        <div class="col-sm-7">
                            <input class="form-control" type="password" id="{{ #dbpassword }}" name="dbpassword" value="" placeholder="" />
                        </div>
                    </div>

                </fieldset>

                <!-- Submit -->

                <div class="form-group">
                    <div class="col-sm-7 col-sm-offset-3">
                        <button type="submit" class="btn btn-primary">{_ Add Site _}</button>
                        <a href="{% url home %}" class="btn btn-default">{_ Cancel _}</a>
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
