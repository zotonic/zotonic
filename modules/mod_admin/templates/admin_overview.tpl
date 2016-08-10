{% extends "admin_base.tpl" %}

{% block title %}{_ Pages _}{% endblock %}

{% block content %}
    {% with
        20,
        "20"
        as
        default_pagelen,
        default_pagelen_label
    %}
        {% with q.qpagelen|default:default_pagelen as qpagelen %}
            {% with q.qcat, q.qgroup as qcat, qgroup %}
                <form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc %}" class="form-inline">
                    <input type="hidden" name="qs" value="{{ q.qs|escape }}" />
                    <input type="hidden" name="qquery" value="{{ q.qquery|escape }}" />
                    <div class="btn-group pull-right">
                        {% if `mod_content_groups`|member:m.modules.enabled %}
                            {% if m.search[{query cat=`content_group`}]|length %}
                                <div class="btn-group">
                                    {% include "_admin_button_dropdown.tpl"
                                        select_name="qgroup"
                                        selected_value=qgroup
                                        selected_label=m.rsc[qgroup].title
                                        default_value=""
                                        default_label=_"All Content"
                                        form_id=#form
                                        option_template="_admin_button_dropdown_content_groups.tpl"
                                        header=_"Filter on content group"
                                        align="right"
                                    %}
                                </div>
                            {% endif %}
                        {% endif %}
                        <div class="btn-group">
                            {% include "_admin_button_dropdown.tpl"
                               select_name="qcat"
                               selected_value=qcat
                               selected_label=m.rsc[qcat].title
                               default_value=""
                               default_label=_"Selected Categories"
                               default_value2="*"
                               default_label2=_"All Categories"
                               form_id=#form
                               option_template="_admin_button_dropdown_categories.tpl"
                               header=_"Filter on category"
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
                    {% if not q.qcat %}{{ m.rsc[q.qquery].title|default:_"Pages overview" }}{% else %}{_ Pages overview _}{% endif %}{% if q.qcat == '*' %}: {_ All Categories _}{% elseif q.qcat %}: {{ m.rsc[q.qcat].title }}{% endif %}{% if q.qcat_exclude %}, {_ excluding _}: {{ m.rsc[q.qcat_exclude].title }}{% endif %}{% if q.qs %}, {_ matching _} “{{ q.qs|escape }}”{% endif %}

                    {% if q.qs %}
                        {% button text=_"show all" class="btn btn-default btn-xs"
                                  action={redirect dispatch="admin_overview_rsc" qcat=q.qcat qquery=q.qquery} %}
                    {% endif %}
                </h2>
                {% if q.qquery and not q.qcat and m.rsc[q.qquery].summary %}
                    <p>{{ m.rsc[q.qquery].summary }}</p>
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

                <a class="btn btn-default{% if not q.qcat and not q.qquery %} disabled{% endif %}" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn btn-default" href="{% url admin_media %}">{_ All media _}</a>
                {% all include "_admin_extra_buttons.tpl" %}
            </div>

            {% with
                q.qcat|replace:'\\*':'',
                q.qcat_exclude
                as
                qcat,
                qcat_exclude
            %}
                {% with q.qsort|default:"-modified" as qsort %}
                    {% with m.rsc[q.qquery|default:`admin_overview_query`].id as qquery_id %}
                          {% with (qquery_id.is_visible and not q.qcat)|
                                    if:{query query_id=qquery_id cat=qcat cat_exclude=qcat_exclude content_group=q.qgroup text=q.qs page=q.page pagelen=qpagelen sort=qsort custompivot=q.qcustompivot}
                                      :{query authoritative=1 cat=qcat cat_exclude=qcat_exclude content_group=q.qgroup text=q.qs page=q.page pagelen=qpagelen sort=qsort custompivot=q.qcustompivot}
                             as query
                          %}
                              {% with m.search.paged[query] as result %}
                                  {% catinclude "_admin_overview_list.tpl" m.category[qcat].is_a result=result qsort=qsort qcat=qcat qcat_exclude=qcat_exclude custompivot=q.qcustompivot %}
                                  {% pager result=result dispatch="admin_overview_rsc" qargs hide_single_page %}
                              {% endwith %}
                          {% endwith %}
                    {% endwith %}
                {% endwith %}
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endblock %}
