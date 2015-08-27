{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ m.rsc[id].title }}”{% endblock %}

{% block bodyclass %}edit-page{% endblock %}

{% block content %}
{% with m.rsc[id] as r %}
{% with r.is_editable as is_editable %}
{% with m.config.i18n.language_list.list as languages %}

{% if not is_editable %}
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
    action={
        update
        target="edit_admin_header"
        template="_admin_edit_header.tpl"
        r=r
        id=id
        is_editable=is_editable
        languages=languages
    }
%}
<form id="rscform" method="post" action="postback" class="form">
	<input type="hidden" name="id" value="{{ id }}" />

	<div class="row">
		<div class="col-lg-8 col-md-8" id="poststuff">
            {% block admin_edit_form_top %}{% endblock %}
			{% catinclude "_admin_edit_main_parts.tpl" id is_editable=is_editable languages=languages r=r %}
		</div>

		<div class="col-lg-4 col-md-4" id="sidebar">
			{% catinclude "_admin_edit_sidebar_parts.tpl" id is_editable=is_editable languages=languages r=r %}
		</div>
	</div>
</form>

{% block admin_edit_form_post %}{% endblock %}

</div>

{% endwith %}
{% endwith %}
{% endwith %}

{% include "_admin_edit_js.tpl" %}

{% endblock %}

{% block editor %}
	{% include "_editor.tpl" %}
{% endblock %}

