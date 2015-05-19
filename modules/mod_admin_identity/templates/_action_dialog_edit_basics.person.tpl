{% extends "_action_dialog_edit_basics.tpl" %}

{% block tabbar %}
	<ul class="nav nav-pills">
	    <li class="active"><a data-toggle="tab" href="#{{ #main }}">{_ Main _}</a></li>
	    <li><a data-toggle="tab" href="#{{ #user }}">{_ User account _}</a></li>
	    <li><a data-toggle="tab" href="#{{ #acl }}">{_ Access control _}</a></li>
	</ul>
{% endblock %}

{% block tab_extra %}
	<div class="tab-pane" id="{{ #user }}">
	    {% include "_admin_edit_basics_user.tpl" id=id %}
    </div>
    
{% endblock %}
