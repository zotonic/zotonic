{% overrules %}

{% block category %}
	{% include "_admin_catcg.tpl" cat_id=m.rsc[cat].id is_nocatselect=(cat and nocatselect) form=form %}
{% endblock %}
