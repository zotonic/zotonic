{% overrules %}

{% block import_options__rsc %}
    {% include "_admin_catcg.tpl" cat_id=m.rsc[cat].id is_nocatview=(cat and nocatselect) no_collab form=form %}
    {% inherit %}
{% endblock %}

