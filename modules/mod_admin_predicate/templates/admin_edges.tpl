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
                               selected_value=m.rsc[qpredicate].id
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
                                selected_value=qpagelen
                                selected_label=qpagelen
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

            {% with m.search.paged[{edges predicate=q.qpredicate page=q.page pagelen=qpagelen}] as result %}
                {% include "_admin_edges_list.tpl" result=result qsort=qsort qcat=qcat %}
                {% pager result=result dispatch="admin_edges" qargs hide_single_page %}
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endblock %}
