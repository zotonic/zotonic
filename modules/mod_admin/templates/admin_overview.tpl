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
            {% with q.qcat as qcat %}
                <form id="{{ #form }}" method="GET" action="{% url admin_overview_rsc qs=q.qs %}" class="form-inline">
                    <div class="btn-group pull-right">
                        <div class="btn-group">
                            {% include "_admin_button_dropdown.tpl"
                               select_name="qcat"
                               selected_value=qcat
                               selected_label=m.rsc[qcat].title
                               default_value=""
                               default_label=_"All Categories"
                               form_id=#form
                               option_template="_admin_button_dropdown_categories.tpl"
                               header=_"Filter on category"
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
                            %}
                        </div>
                    </div>
                </form>
            {% endwith %}
            <h2>
                {_ Pages overview _}{% if q.qcat %}: {{ m.rsc[q.qcat].title }}{% endif %}{% if q.qs %}, 
                    {_ matching _} “{{ q.qs|escape }}”
                    {% button text=_"show all" class="btn btn-default btn-xs" action={redirect dispatch="admin_overview_rsc" qcat=q.qcat} %}
                    <input type="hidden" name="qs" value="{{ q.qs|escape }}" />
                {% endif %}
            </h2>

            <div class="well">
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

                <a class="btn btn-default disabled" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn btn-default" href="{% url admin_media %}">{_ All media _}</a>
            </div>

            {% with m.search.paged[{query authoritative=1 cat=q.qcat text=q.qs page=q.page pagelen=qpagelen sort=q.qsort|default:"-modified"}] as result %}
                {% catinclude "_admin_overview_list.tpl" m.category[q.qcat].is_a result=result %}
                {% pager result=result dispatch="admin_overview_rsc" qargs hide_single_page %}
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endblock %}
