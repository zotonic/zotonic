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
		nav.navbar .checkbox.inline {
			color:white;
			padding-top: 0;
			margin-left: 10px;
			margin-right: 10px;
		}
		nav.navbar .btn {
			margin-bottom: 10px;
		}
	</style>
{% endblock %}

{% block content_area %}
	{% with id|menu_rsc as tree_id %}
	{% with {postback postback={admin_menu_edit} delegate=`mod_admin_frontend`} as admin_menu_edit_action %}
	<div class="row-fluid">
		{% with m.rsc[tree_id].id as tree_id %}
			{% if tree_id and tree_id.is_visible %}
				<div class="span4" id="menu-editor">
					{% block above_menu %}{% endblock%}
			        {% include "_admin_menu_menu_view.tpl" id=tree_id connect_tab="new" cat_id=m.rsc.text.id %}
					{% block below_menu %}{% endblock%}
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
					{% wire postback={admin_menu_edit id=id} delegate=`mod_admin_frontend` %}
				</div>
			{% endif %}
		{% endwith %}
	</div>
	{% endwith %}
	{% endwith %}
	{% include "_admin_edit_js.tpl" %}
{% endblock %}

{% block navbar %}
{# The buttons in the navbar click/sync with hidden counter parts in the resource edit form #}
<nav class="navbar navbar-fixed-top">
	<div class="navbar-inner">
	<div class="container-fluid">
		<div class="row-fluid">
			<div class="span4">
				<a href="{{ school_id.page_url }}" class="btn">{_ Close _}</a>
			</div>
			<div class="span8" id="save-buttons" style="display:none">
				<span class="brand visible-desktop">{_ This page _}</span>

				{% button class="btn btn-primary" text=_"Save" title=_"Save this page." 
						  action={script script="$('#save_stay').click();"}
				 %}

				{% button class="btn" text=_"Save &amp; view" title=_"Save and view the page." 
						  action={script script="$('#save_view').click();"}
				 %}

				<label for="is_published_navbar" class="checkbox inline">
		    		<input type="checkbox" id="is_published_navbar" name="is_published_navbar" value="1" checked="checked" />
		    	    {_ Published _}
	    	    </label>
	    	    {% javascript %}
	    	    	$('#is_published_navbar').change(function() {
	    	    		$('#is_published').attr('checked', $(this).is(':checked'));
		    	    });
	    	    {% endjavascript %}

				{% button class="btn pull-right" text=_"Cancel" action={update target="editcol" template="_admin_frontend_nopage.tpl"} tag="a" %}
	    	</div>
		</div>
	</div>
	</div>
</nav>
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
