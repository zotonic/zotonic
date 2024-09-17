{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ id.title|default:id.short_title }}”{% endblock %}

{% block bodyclass %}edit-page cg-{{ id.content_group_id.name }} {% for cat,_ in id.is_a %}cat-{{ cat }} {% endfor %} name-{{ id.name }}{% endblock %}

{% block bodyattr %} data-rsc-id="{{ id.id }}" data-fileuploader="{{ %{ subject_id: id.id, predicate: "depiction" }|escape }}"{% endblock %}

{% block content %}

    {% include "_admin_system_content_warning.tpl" category_id=id.category_id %}

    {% if not id.is_editable %}
        <div class="alert alert-warning" role="alert">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
            {_ You are not allowed to edit this page. _}
        </div>
    {% endif %}

    <div id="edit_admin_header">
        {% include "_admin_edit_header.tpl" %}
    </div>

    {% include "_admin_authoritative_info.tpl" %}

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

    	<div class="rscform-columns">
    		<div id="poststuff">
                <div id="translation-tabs" class="minimalTabs">
                    {% include "_admin_translation_tabs.tpl" prefix=#prefix r_language=r_language top %}
                </div>

                {% block admin_edit_form_top %}{% endblock %}
    			{% catinclude "_admin_edit_main_parts.tpl" id %}
    		</div>

    		<div id="sidebar">
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

