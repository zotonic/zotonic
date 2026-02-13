{% extends "admin_base.tpl" %}

{% block title %}{_ Page connections _}{% endblock %}

{% block content %}
    {% with
        100,
        "100"
        as
        default_pagelen,
        default_pagelen_label
    %}
        {% with q.qpagelen|default:default_pagelen as qpagelen %}
            {% with q.qpredicate as qpredicate %}
                <form id="{{ #form }}" method="GET" action="{% url admin_edges %}" class="form-inline">
                    <input type="hidden" name="qhassubject" value="{{ q.qhassubject|escape }}">
                    <input type="hidden" name="qhasobject" value="{{ q.qhasobject|escape }}">
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
                <h2>{_ Page connections overview _}</h2>
                {% if q.qhassubject or q.qhasobject or q.qpredicate %}
                    <p>
                        {% trans "List {predicate} connections from {subject} to {object}."
                                 subject=q.qhassubject|if
                                            :("<em>"++m.rsc[q.qhassubject].title|default:_"Untitled"++"</em>")
                                            :_"any subject"
                                 object=q.qhasobject|if
                                            :("<em>"++m.rsc[q.qhasobject].title|default:_"Untitled"++"</em>")
                                            :_"any object"
                                 predicate=q.qpredicate|if
                                            :("<em>"++m.rsc[q.qpredicate].title|default:_"Untitled"++"</em>")
                                            :_"all"

                        %}
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
                <div class="text-muted clear-left">
                    {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
                    {% else %}{% trans "{n} items found." n=result.total %}
                    {% endif %}
                </div>
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endblock %}
