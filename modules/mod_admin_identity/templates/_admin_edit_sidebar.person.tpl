{% extends "admin_edit_widget_std.tpl" %}

{# editing rsc username and password #}

{% block widget_title %}{_ Username / password _}{% endblock %}
{% block widget_show_minimized %}{{ not m.identity[id].is_user }}{% endblock %}

{% block widget_content %}
    <div class="pull-right">
	    <a href="javascript:void(0)" class="btn btn-primary btn-xs do_dialog" data-dialog="title: '{_ Help about user credentials. _}', text: '{_ When you add credentials to a person then the person becomes an user. A person or machine can log on with those credentials and perform actions on your Zotonic system.<br/><br/>What an user can do depends on the groups the user is member of. _}'" title="{_ Need more help? _}"><i class="glyphicon glyphicon-question-sign"></i></a>
</div>


{% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
    <div class="form-group">
	    <div>
		    {% button class="btn btn-default" action={dialog_set_username_password id=id} text=_"Set username / password" %}
		    {% if m.acl.is_admin and m.identity[id].is_user and id != m.acl.user %}
			    {% button class="btn btn-default" action={confirm text=_"Click OK to log on as this user. You will be redirected to the home page if this user has no rights to access the admin system." postback={switch_user id=id} delegate=`mod_admin_identity`} text=_"Log on as this user" %}
		    {% endif %}
	    </div>
    </div>
{% endif %}

{% if m.modules.active.mod_translation %}
    <div class="form-group">
	    <label class="control-label" for="pref_language">{_ Language _}</label>
	    <div>
		    <select class="form-control" id="pref_language" name="pref_language">
		        {% with m.config.i18n.language_list.list as list %}
		            {% for code,lang in list %}
			            {% if lang.is_enabled %}
			                <option {% if id.pref_language == code %}selected{% endif %} value="{{ code }}">{{ lang.language }}</a>
			            {% endif %}
		            {% endfor %}
		        {% endwith %}
		    </select>
	    </div>
    </div>	
{% endif %}

{% if m.modules.active.mod_l10n %}
    <div class="form-group">
	    <label class="control-label" for="pref_tz">{_ Timezone _}</label>
	    <div>
		    <select class="form-control" id="pref_tz" name="pref_tz">
			    <option></option>
			    {% include "_l10n_timezone_options.tpl" timezone=id.pref_tz %}
		    </select>
	    </div>
    </div>
{% endif %}

<div class="form-group">
    <div class="alert alert-info">
        {% if m.identity[id].is_user %}
	        {_ This person is also a user. _}
        {% else %}
	        {_ This person is not yet a user. _}
        {% endif %}
    </div>
</div>
{% endblock %}
