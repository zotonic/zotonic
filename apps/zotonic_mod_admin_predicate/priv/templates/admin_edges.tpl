{% extends "admin_base.tpl" %}

{% block title %}{_ Page connections _}{% endblock %}

{% block content %}
    {% with
        20,
        "20"
        as
        default_pagelen,
        default_pagelen_label
    %}
        {% with q.qpagelen|default:default_pagelen as qpagelen %}
            {% with q.qpredicate as qpredicate %}
                <form id="{{ #form }}" method="GET" action="{% url admin_edges %}" class="form-inline">
                    <div class="btn-group pull-right">
                        <div class="btn-group">
                            {% include "_admin_button_dropdown.tpl"
                               select_name="qpredicate"
                               selected_qvalue=m.rsc[qpredicate].id
                               selected_label=m.rsc[qpredicate].title
                               default_value=""
                               default_label=_"All Predicates"
                               form_id=#form
                               option_template="_admin_button_dropdown_predicates.tpl"
                               header=_"Filter on predicate"
                               align="right"
                            %}
                        </div>
                        <div class="btn-group">
                            {% include "_admin_button_dropdown.tpl"
                                select_name="qpagelen"
                                selected_qvalue=qpagelen|to_integer|default:default_pagelen
                                selected_label=qpagelen|to_integer|default:default_pagelen
                                default_value=default_pagelen
                                default_label=default_pagelen_label
                                form_id=#form
                                options=[[10,"10"], [20,"20"], [50,"50"], [100,"100"], [200,"200"], [500,"500"]]
                                header=_"Items per page"
                                align="right"
                            %}
                        </div>
                    </div>
                </form>
            {% endwith %}
            <div class="admin-header">
                <h2>
                    {% if q.qpredicate %}
                        {_ Page connections with the predicate _}: {{ m.rsc[q.qpredicate].title }}
                    {% else %}
                        {_ Page connections overview _}
                    {% endif %}
                </h2>
                {% if q.qhassubject or q.qhasobject %}
                    <p>
                        {_ Only showing connections with: _}
                        {% if q.qhassubject %}
                            {_ subject _} <a href="{% url admin_edit_rsc id=q.qhasubject %}">{{ m.rsc[q.qhassubject].title|default:_"Untitled" }}</a>
                        {% endif %}
                        {% if q.qhasobject %}
                            {_ object _} <a href="{% url admin_edit_rsc id=q.qhasobject %}">{{ m.rsc[q.qhasobject].title|default:_"Untitled" }}</a>
                        {% endif %}
                    </p>
                {% endif %}
            </div>
            <div class="well z-button-row">
                <a name="content-pager"></a>

                {% button
                    class="btn btn-primary"
                    text=_"Make a new page or media"
                    action={
                        dialog_new_rsc
                        title=""
                        cat=q.qcat
                    }
                %}

                {% all include "_admin_make_page_buttons.tpl" %}

                <a class="btn btn-default" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn btn-default" href="{% url admin_media %}">{_ All media _}</a>
                {% all include "_admin_extra_buttons.tpl" %}
            </div>

            {% with m.search.paged[{edges
                    hassubject=q.qhassubject
                    hasobject=q.qhasobject
                    predicate=q.qpredicate
                    page=q.page
                    pagelen=qpagelen
                }] as result %}
                {% include "_admin_edges_list.tpl" result=result qsort=qsort qcat=qcat %}
                {% pager result=result dispatch="admin_edges" qargs hide_single_page %}
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endblock %}
