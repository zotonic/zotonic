{% extends "admin_edit_widget_std.tpl" %}

{# editing rsc username and password #}

{% block widget_title %}{_ Username / password _}{% endblock %}
{% block widget_show_minimized %}{{ not m.identity[id].is_user }}{% endblock %}

{% block widget_content %}
<div class="pull-right">
    <a href="javascript:void(0)" class="btn btn-primary btn-mini do_dialog" data-dialog="title: '{_ Help about user credentials. _}', text: '{_ When you add credentials to a person then the person becomes an user. A person or machine can log on with those credentials and perform actions on your Zotonic system.<br/><br/>What an user can do depends on the groups the user is member of. _}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
</div>


{% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
<div class="control-group">
    <div class="controls">
	{% button class="btn" action={dialog_set_username_password id=id} text=_"Set username / password" %}
    </div>
</div>
{% endif %}

<div class="alert alert-info">
{% if m.identity[id].is_user %}
    {_ This person is also a user. _}
{% else %}
    {_ This person is not yet a user. _}
{% endif %}
</div>

{% endblock %}
