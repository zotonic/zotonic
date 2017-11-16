{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ id.title }}”{% endblock %}

{% block bodyclass %}edit-page{% endblock %}

{% block content %}
    {% if not id.is_editable %}
        <div class="alert alert-warning" role="alert">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
            {_ You are not allowed to edit this page. _}
        </div>
    {% endif %}

    <div id="edit_admin_header">
        {% include "_admin_edit_header.tpl" %}
    </div>

    {% block admin_edit_form_pre %}{% endblock %}

    {% wire
        id="rscform"
        type="submit"
        postback="rscform"
        action={update
            target="edit_admin_header"
            template="_admin_edit_header.tpl"
            id=id
        }
    %}
    <form id="rscform" method="post" action="postback" class="form">
    	<input type="hidden" name="id" value="{{ id }}" />

    	<div class="row">
    		<div class="col-lg-8 col-md-8" id="poststuff">
                {% block admin_edit_form_top %}{% endblock %}
    			{% catinclude "_admin_edit_main_parts.tpl" id %}
    		</div>

    		<div class="col-lg-4 col-md-4" id="sidebar">
    			{% catinclude "_admin_edit_sidebar_parts.tpl" id %}
    		</div>
    	</div>
    </form>

    {% block admin_edit_form_post %}{% endblock %}

    </div>

    {% include "_admin_edit_js.tpl" %}
{% endblock %}

{% block editor %}
	{% include "_editor.tpl" %}
{% endblock %}

