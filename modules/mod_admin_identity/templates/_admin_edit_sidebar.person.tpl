{% extends "admin_edit_widget_std.tpl" %}

{# editing rsc username and password #}

{% block widget_title %}{_ Username / password _}{% endblock %}
{% block widget_show_minimized %}{{ not m.identity[id].is_user }}{% endblock %}

{% block widget_content %}
<div class="notification notice">
	{_ Add or remove credentials. _} 
	<a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about user credentials. _}', text: '{_ When you add credentials to a person then the person becomes an user. A person or machine can log on with those credentials and perform actions on your Zotonic system.<br/><br/>What an user can do depends on the groups the user is member of. _}', width: '450px'">{_ Need more help? _}</a>

	<br />
	<strong>
		{% if m.identity[id].is_user %}
			{_ This person is also a user. _}
		{% else %}
			{_ This person is not yet a user. _}
		{% endif %}
	</strong>
</div>

{% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
	{% button action={dialog_set_username_password id=id} text=_"Set username / password" %}
{% endif %}

{% endblock %}
