{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{% if error == `signup_confirm` %}{_ Confirm _}{% else %}{_ Error _}{% endif %}{% endblock %}

{% block content %}
{% if error == `signup_confirm` %}
	<div class="container">
		<h1>{_ You are about to create a new account. _}</h1>

		<p>{_ Close this window and log in if you are already a user of _} {{ m.config.site.title.value }}</p>

		{% wire id="signup_confirm"
				type="submit"
			  	postback={signup_confirm auth=what}
				delegate=`mod_authentication`
		%}
		<form id="signup_confirm" class="z_cookie_form" action="postback">
			<button class="btn btn-primary" type="submit">{_ I want to create a new account _}</button>

			{% button tag="a" class="btn btn-default" text=_"Close Window" action={script script="window.close();"} %}
		</form>

		<div class="padding alert alert-danger" style="display:none" id="signup_error">
			<p>
				<b>{_ Something went wrong _}</b>
				{_ Please try again later. _}
			</p>
		</div>
	</div>
{% else %}
	{% if error == `email_required` %}
		<div class="container">
			<h1>{_ Sorry _}</h1>

			<p>{_ You have to share your email address to be able to sign in. _}</p>

			{% if auth_link %}
				<p><a href="{{ auth_link }}">{_ Change your permissions _}</a></p>
			{% endif %}
		</div>
	{% elseif error == `cancel` %}
		{% javascript %}
			window.close();
		{% endjavascript %}
	{% elseif error == `duplicate` %}
		<div class="container">
			<h1>{_ Already Connected _}</h1>

			<p class="alert">{_ Somebody else is already connected with this account on _} {{ service }}</p>
		</div>
	{% elseif is_safari8problem %}
		<div class="container">
			<h1>{_ Sorry _}</h1>

			<p>{_ Safari 8 has a known problem with handling external authentications. _}</p>

			<p>{_ This can be resolved by changing the Cookies and website data settings in the Safari preferences. _}</p>

			<p>
				<img src="https://cloud.githubusercontent.com/assets/38268/5646695/451d9c32-967f-11e4-80fc-913d5e8b483e.jpg" width="100%" />
			</p>
		</div>
	{% else %}
		<div class="container">
			<h1>{_ Sorry _}</h1>

			<p class="alert alert-danger">{_ There was a problem authenticating with _} {{ service }}</p>

			<p>{_ Please try again later. _}</p>
		</div>
	{% endif %}

	<p>
		{% button class="btn" action={script script="window.close();"} text=_"Close Window" %}
	</p>
{% endif %}
{% endblock %}
