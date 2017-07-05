{% extends "base.tpl" %}

{% block title %}Zotonic{% endblock %}
{% block navbar %}{% endblock %}

{% block content %}
<div class="col-md-6 col-md-offset-3">
    <div class="jumbotron">
        <h1>{_ Powered by _} <span class="zotonic-logo"><em>Zotonic</em></span></h1>
        <p>
            {_ Welcome, visitor! The page you are
            currently looking at is the default page for a
            Zotonic web server. The fact that you are seeing
            this page could mean that the website you are
            trying to visit has not been configured
            correctly. _}
        </p>
        <p>
            {_ If you feel that you are here by mistake,
            please hit the "back" button in your browser or
            try a different address. If you are here to manage
            this server, please enter the password below. _}
        </p>
    </div>
</div>
<div class="col-md-6 col-md-offset-3">
   <div class="panel panel-default">
        <div class="panel-heading">
            <h3 class="panel-title">{_ Enter your password to manage this server _}</h3>
        </div>
        <div class="panel-body">
            <form id="logon_form" method="post" action="postback" class="form">
                <div class="form-group row">
                    <div class="col-md-8 col-md-offset-2">
                        <div class="input-group">
                            <input class="form-control" type="password" id="password" name="password" value="" autofocus />
                            <span class="input-group-btn">
                                <button type="submit" class="btn btn-primary">{_ Log On _}</button>
                            </span>
                        </div>
                        <div id="error-pw" class="has-error">
                            <p class="help-block help-block-error">{_ This password is not correct. _}</p>
                            <p class="help-block">{_ You can find the password in ~/.zotonic/[version]/zotonic.config. _}</p>
                        </div>
                    </div>
                </div>
            </form>
        </div>
    </div>
</div>
{% endblock %}
