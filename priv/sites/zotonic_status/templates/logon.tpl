{% extends "base.tpl" %}

{% block title %}Zotonic{% endblock %}
{% block navbar %}{% endblock %}

{% block content %}
<div class="hero-unit">
    <h1>{_ Powered by _} <span class="zotonic-logo"></span></h1>
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
<div class="well">
    <h2>{_ Log on to manage this server _}</h2>
    <p class="help-block">{_ To manage the sites on this server, please enter the main admin password. _}</p>
    <form id="logon_form" action="postback" class="form-inline z_logon_form">
    	<div id="logon_password">
    	    <p>
        		<label>{_ Password _}:&nbsp;</label>
    	       	<input type="password" id="password" name="password" value="" autofocus />
    	        <button class="btn btn-primary">{_ Log On _}</button>
    	    </p>

            <div id="error-pw" class="has-error">
                <p class="help-block help-block-error">{_ This password is not correct. _}</p>
                <p class="help-block">{_ You can find the password in ~/.zotonic/[version]/zotonic.config. _}</p>
            </div>
            <div id="error-ratelimit" class="has-error">
                <p class="help-block help-block-error">{_ Too many tries, try again in an hour. _}</p>
                <p class="help-block">{_ You can find the password in ~/.zotonic/[version]/zotonic.config. _}</p>
            </div>
    	</div>

    	<div id="logon_button">
    	</div>
    </form>
</div>
{% endblock %}	
