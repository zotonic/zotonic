{% extends "base.tpl" %}

{% block navbar %}
{% endblock %}

{% block content_area %}
<div class="hero-unit">
    <h1>{_ Powered by Zotonic _}</h1>
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
    <form id="logon_form" action="postback" class="form-inline">
	<p id="error-pw" class="alert alert-error">The password does not match.  Please retry.</p>
	<div id="logon_password">
	    <div style="display:none">
		<input type="text" id="username" name="username" value="admin"  />
	    </div>
	    <p>
		<label>{_ Password _}</label>
		<input type="password" id="password" name="password" value="" autofocus />
	        <button class="btn btn-primary">{_ Log On _}</button>
	    </p>
	</div>

	<div id="logon_button">
	</div>
    </form>
</div>
{% endblock %}	
