{% overrules %}

{% block category %}
    {% if nocgselect and nocatselect %}
        <input type="hidden" id="{{ #catsel }}" name="category_id" value="{{ m.rsc[cat].id }}"/>
        <input type="hidden" id="{{ #cgsel }}" name="content_group_id" value="{{ m.rsc[cg_id].id }}"/>
    {% else %}
    	{% include "_admin_catcg.tpl"
                    cat_id=m.rsc[cat].id
                    cat_restrict=m.rsc[cat].name|as_atom
                    is_nocatselect=(cat and nocatselect)
                    is_nocgselect=nocgselect
                    form=form
                    is_hide_othercg
                    predicate=predicate
                    object_id=object_id
                    subject_id=subject_id
        %}
    {% endif %}
{% endblock %}
