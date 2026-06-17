{% extends "base.tpl" %}

{% block title %}{_ Sign Up _}{% endblock %}

{% block html_head_extra %}
	{% inherit %}
	{% lib
	    "css/logon.css"
	%}

	{# Redirect away if there is a user logged on #}
	{% if m.acl.user %}
    	{% wire action={redirect location=m.signup.confirm_redirect} %}
    {% endif %}
{% endblock %}

{% block content_area %}
	<div id="signup_logon_box" class="signup z-logon-box{% if style_boxed %} z-logon-box-boxed{% endif %}" {% if style_width %}style="width: {{ style_width }};"{% endif %}>
		<h1 class="z-logon-title">{_ Sign Up _}</h1>

   		<div id="signup-login">
			<p>{_ If you already have an account, _} <a href="{% url logon p=q.p %}" id="back_to_logon">{_ log in _}</a>.</p>
		</div>

		<!-- Show signup with email -->
		{% include "_signup_with_email.tpl" %}

		<!-- Show all signup SSO buttons -->
		<div id="signup-services">
		    <ul class="z-logon-extra">
				<li class="text-muted z-logon-extra-separator -first">{_ or _}</li>
				{% all include "_logon_extra.tpl" is_signup %}
			</ul>
		</div>
	</div>
{% endblock %}
