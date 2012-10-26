{% extends "admin_edit_widget_std.tpl" %}

{# 
    Example of standard and simple widget for /admin/edit/N page.
    Similar skeleton also used for basis of sidebar widgets.
#}


{# Title data for widget headline. #}
{% block widget_title %}{_ Edit title _}{% endblock %}


{# Widget may be displayed to user as minimized by default
   NOTE: content precense in this block is mandatory #}
{% block widget_show_minimized %}true{% endblock %}


{# Some tags, that will be rendered before widget contained.
   Do NOT include other widgets here, because dtl compiler will throw recursion error #}
{% block widget_before %}{% endblock %}


{# The content. The body of widget. #}
{% block widget_content %}
    {% with m.rsc[id] as r %}
	<fieldset class="admin-form">
	    <div class="form-item clearfix">
		<label for="title">{_ Title _}</label>
		<input id="title" type="text" name="title" value="{{ r.title }}" />
	    </div>
	</fieldset>
    {% endwith %}
{% endblock %}


{# Some optional tags, that will be rendered immediately after widget container. 
   For example, some javascript: #}
{% block widget_after %}
    <script type="text/javascript">
	alert("Hello World!");
    </script>
{% endblock %}
