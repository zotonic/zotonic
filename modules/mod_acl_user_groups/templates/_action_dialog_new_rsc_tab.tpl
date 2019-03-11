{% overrules %}

{% block category %}
	{% include "_admin_catcg.tpl"
                cat_id=m.rsc[cat].id
                cat_restrict=m.rsc[cat].name|as_atom
                is_nocatselect=(cat and nocatselect)
                form=form
                is_hide_othercg
                predicate=predicate
                object_id=object_id
                subject_id=subject_id
    %}
{% endblock %}
