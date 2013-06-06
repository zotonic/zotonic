{% extends "base_frontend_edit.tpl" %}

{% block title %}{% if id %}{{ id.title|default:"-" }}{% elseif tree_id %}{{ tree_id.title|default:"-" }}{% endif %}{% endblock%}

{% block html_head_extra %}
	{% lib 
			"css/zp-menuedit.css" 
			"css/zotonic-admin.css" 
	%}
	<style type="text/css">
		#editcol {
			max-width: 800px;
		}
	</style>
{% endblock %}

{% block content_area %}
	{% with id|menu_rsc as tree_id %}
	{% with {postback postback=`admin-menu-edit` delegate=`mod_admin_frontend`} as admin_menu_edit_action %}
	<div class="row-fluid">
		{% with m.rsc[tree_id].id as tree_id %}
			{% if tree_id and tree_id.is_visible %}
				<div class="span4" id="menu-editor">
			        {% include "_admin_menu_menu_view.tpl" id=tree_id connect_tab="new" cat_id=m.rsc.text.id %}
				</div>
				<div class="span8" id="editcol">
					{% if id %}
						{% catinclude "_admin_frontend_edit.tpl" id tree_id=tree_id %}
					{% else %}
						{% include "_admin_frontend_nopage.tpl" tree_id=tree_id %}
					{% endif %}
				</div>
			{% else %}
				<div class="span12" id="editcol">
					{% if id %}
						{% catinclude "_admin_frontend_edit.tpl" id tree_id=tree_id %}
					{% else %}
						{% include "_admin_frontend_nopage.tpl" tree_id=tree_id %}
					{% endif %}
				</div>
			{% endif %}
		{% endwith %}
	</div>
	{% endwith %}
	{% endwith %}
	{% include "_admin_edit_js.tpl" %}
{% endblock %}

{% block _js_include_extra %}
	{% lib
    	"js/modules/jquery.hotkeys.js"
	    "js/modules/tiny-init.js"
	    "js/modules/z.adminwidget.js"
	    "js/modules/z.tooltip.js"
	    "js/modules/z.feedback.js"
	    "js/modules/z.formreplace.js"
	    "js/modules/z.datepicker.js"
	    "js/modules/z.menuedit.js"

	    "js/jquery.ui.nestedSortable.js"

	    "js/apps/admin-common.js"
	%}
	{% all include "_admin_lib_js.tpl" %}
	{% include "_admin_tinymce.tpl" %}
{% endblock %}
