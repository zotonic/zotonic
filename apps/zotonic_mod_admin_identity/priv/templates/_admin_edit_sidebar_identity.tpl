{% extends "admin_edit_widget_std.tpl" %}

{# editing rsc username and password #}

{% block widget_id %}username-password{% endblock %}

{% block widget_title %}
{_ Username / password _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Help about user credentials",
                text: _"When you add credentials to a person then the person becomes a user. A person or machine can log on with those credentials and perform actions on your Zotonic system.<br/><br/>What a user can do depends on the groups the user is member of."
            }|escape
        }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}{{ not m.identity[id].is_user }}{% endblock %}

{% block widget_content %}
    {% live template="_admin_edit_sidebar_identity_live.tpl" id=id topic=[ "bridge", "origin", "model", "identity", "event", id, "username_pw" ] %}
{% endblock %}
