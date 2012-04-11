{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}{_ Basic _}{% endblock %}
{% block widget_i18n_tab_class %}item{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-basics{% endblock %}

{% block widget_content %}
{% catinclude "_admin_edit_basics_form.tpl" id lang_code_with_brackets=lang_code_with_brackets lang_code_with_dollar=lang_code_with_dollar r_language=r_language is_i18n=is_i18n lang_code=lang_code lang=lang %}
{% endblock %}
