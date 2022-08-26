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
        {% with q.pagelen|default:default_pagelen as qpagelen %}
            {% with q.qcat, q.qcontent_group as qcat, qcontent_group %}
                <form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc %}" class="form-inline">
                    <input type="hidden" name="qs" value="{{ q.qs|escape }}" />
                    <input type="hidden" name="qquery_id" value="{{ q.qquery_id|escape }}" />
                    <div class="btn-group pull-right">
                        {% if `mod_content_groups`|member:m.modules.enabled %}
                            {% if m.search[{query cat=`content_group`}]|length %}
                                <div class="btn-group">
                                    {% include "_admin_button_dropdown.tpl"
                                        select_name="qcontent_group"
                                        selected_qvalue=qcontent_group
                                        selected_label=m.rsc[qcontent_group].title
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
                               selected_qvalue=qcat
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
                                select_name="pagelen"
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
                    {% if not q.qcat %}{{ m.rsc[q.qquery_id].title|default:_"Pages overview" }}{% else %}{_ Pages overview _}{% endif %}{% if q.qcat == '*' %}: {_ All Categories _}{% elseif q.qcat %}: {{ m.rsc[q.qcat].title }}{% endif %}{% if q.qcat_exclude %}, {_ excluding _}: {{ m.rsc[q.qcat_exclude].title }}{% endif %}{% if q.qs %}, {_ matching _} “{{ q.qs|escape }}”{% endif %}

                    {% if q.qs %}
                        {% button text=_"show all" class="btn btn-default btn-xs"
                                  action={redirect dispatch="admin_overview_rsc" qcat=q.qcat qquery_id=q.qquery_id} %}
                    {% endif %}
                </h2>
                {% if q.qquery_id and not q.qcat and m.rsc[q.qquery_id].summary %}
                    <p>{{ m.rsc[q.qquery_id].summary }}</p>
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

                <a class="btn btn-default{% if not q.qcat and not q.qquery_id %} disabled{% endif %}" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn btn-default" href="{% url admin_media %}">{_ All media _}</a>
                {% all include "_admin_extra_buttons.tpl" %}
            </div>

            {% if q.qquery_id and m.rsc[q.qquery_id].is_visible %}
                {% with m.search.paged[{query qargs zsort="-modified" page=q.page pagelen=q.pagelen}] as result %}
                    {% catinclude "_admin_overview_list.tpl"
                                  m.category[q.qcat].is_a
                                  result=result
                    %}
                    {% pager result=result dispatch="admin_overview_rsc" qargs pagelen=q.pagelen hide_single_page %}
                {% endwith %}
            {% else %}
                {% with m.search.paged[{query query_id=`admin_overview_query` is_published="all" qargs zsort="-modified" page=q.page pagelen=q.pagelen}] as result %}
                    {% catinclude "_admin_overview_list.tpl"
                                  m.category[q.qcat].is_a
                                  result=result
                    %}
                    {% pager result=result dispatch="admin_overview_rsc" qargs pagelen=q.pagelen hide_single_page %}
                    <div class="text-muted clear-left">{% trans "{total} items found" total=result.total %}{% if result.is_total_estimated %} ({_ estimated _}){% endif %}</div>
                {% endwith %}
            {% endif %}
        {% endwith %}
    {% endwith %}
{% endblock %}
