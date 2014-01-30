{% overrules %}

{% block html_head_extra %}
	{% inherit %}
	{% lib "css/admin_survey.css" %}
{% endblock %}

{% block _js_include_extra %}
	{% lib "js/admin_survey_question_editor.js" %}

	{% inherit %}

	{% javascript %}
		window.z_translations = window.z_translations || {};
		window.z_translations["OK"] = "{_ OK _}";
		window.z_translations["Cancel"] = "{_ Cancel _}";
		window.z_translations["Delete"] = "{_ Delete _}";
		window.z_translations["Confirm"] = "{_ Confirm _}";
		window.z_translations["Are you sure you want to delete this page?<br/>This also deletes all questions on this page."] = "{_ Are you sure you want to delete this page?<br/>This also deletes all questions on this page. _}";
		window.z_translations["Are you sure you want to delete this question?"] = "{_ Are you sure you want to delete this question? _}";
		window.z_translations["Are you sure you want to delete this page jump?"] = "{_ Are you sure you want to delete this page jump? _}";

		$('.pages').on('click', '.block-page a.page-connect', function(event) {
			window.zBlockConnectTrigger = this;
			z_event("admin-block-connect", {});
			event.preventDefault();
		});

	{% endjavascript %}
{% endblock %}
