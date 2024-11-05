{% extends "admin_base.tpl" %}

{% block title %}{_ Media _}{% endblock %}

{% block search_target %}{% url admin_media %}{% endblock %}

{% block content %}

    {% with
        20,
        "20"
        as
        default_pagelen,
        default_pagelen_label
    %}
        {% with q.qpagelen|default:default_pagelen as qpagelen %}
            {% with q.qcat, q.qcontent_group as qcat, qcontent_group %}
                <form id="{{ #form }}" method="GET" action="{% url admin_media %}" class="form-inline">
                    <input type="hidden" name="qs" value="{{ q.qs|escape }}" />
                    <input type="hidden" name="qquery_id" value="{{ q.qquery_id|escape }}" />

                    <div class="btn-group pull-right">
                        <button class="btn btn-default" id="btn-filter">
                            <i class="glyphicon glyphicon-filter"></i> {_ Filter _}...
                        </button>
                        {% wire id="btn-filter"action={slide_toggle target="filter-panel"} %}

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
                <h2>{_ Media _}</h2>

                <p>
                    {_ Media encompasses all uploaded images, movies and documents. Media can be attached to pages. _}
                    {_ And media can also be viewed on their own page. _}
                </p>
            </div>

            <div class="well z-button-row">
                <a name="content-pager"></a>
                {% button
                    class="btn btn-primary"
                    text=_"Make a new media item"
                    action={dialog_media_upload intent="create"}
                %}
                <a class="btn btn-default" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
                <a class="btn btn-default{% if not q.qcat %} disabled{% endif %}" href="{% url admin_media %}">{_ All media _}</a>
                {% all include "_admin_extra_buttons.tpl" %}
            </div>

            <div id="filter-panel" class="well" style="display:none">
                {% include "_admin_overview_filter_panel.tpl"
                           on_cancel={slide_toggle target="filter-panel"}
                           qargs=q.qargs
                           dispatch=zotonic_dispatch
                %}
            </div>

            <p>
                <span id="sel-count">0</span> <span class="text-muted">{_ Selected _}</span>

                <button class="btn btn-default btn-xs" id="csel-update">{_ Update _}...</button>
                <button class="btn btn-default btn-xs" id="csel-delete">{_ Delete _}...</button>
            </p>


            {% with q.qsort|default:"-medium.created" as qsort %}
                {% with m.search.paged[{query hasmedium qargs is_published=`all` page=q.page sort=qsort pagelen=qpagelen zsort=qsort }] as result %}

                    <table class="table table-striped do_adminLinkedTable">
                        <thead>
                            <tr>
                                <th>
                                    <input id="check-all" type="checkbox" value="1">
                                </th>
                                <th width="10%">{_ Preview _}</th>
                                <th width="35%">{% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" qsort=qsort %}</th>
                                <th width="15%">{% include "_admin_sort_header.tpl" field="medium.size" caption=_"Info" qsort=qsort %}</th>
                                <th width="15%">{% include "_admin_sort_header.tpl" field="modified" caption=_"Modified" type="date" qsort=qsort %}</th>
                                <th width="25%">{% include "_admin_sort_header.tpl" field="medium.created" caption=_"Uploaded" type="date" qsort=qsort %}</th>
                            </tr>
                        </thead>

                        <tbody>
                            {% for id in result %}
                                {% if id.is_visible %}
                                    {% with id.medium as medium %}
                                        <tr id="{{ #li.id }}" {% if not id.is_published %}class="unpublished" {% endif %} data-href="{% url admin_edit_rsc id=id %}">
                                            <td class="not-clickable">
                                                <input type="checkbox" value="{{ id }}" name="csel">
                                            </td>
                                            <td>{% image medium mediaclass="admin-list-overview" class="thumb" %}</td>
                                            <td>
                                                <strong>{{ id.title|default:id.short_title|default:_"<em>Untitled</em>" }}</strong>
                                                {% if id.is_protected %}
                                                    &nbsp; <small class="fa fa-lock text-muted" title="{_ Protected, not deletable _}"></small>
                                                {% endif %}

                                                <br />
                                                {% if id.medium.filename|split:"/"|last as filename %}
                                                    <span class="text-muted">
                                                        <span class="glyphicon glyphicon-file"></span> {{ filename }}
                                                    </span>
                                                {% endif %}
                                            </td>
                                            <td>
                                                <p class="help-block">
                                                    {{ medium.mime|escape }}<br>
                                                    {{ medium.width }}&times;{{ medium.height }}<br>
                                                    {{ medium.size|filesizeformat }}
                                                </p>
                                            </td>
                                            <td>
                                                {{ id.modified|date:_"d M Y, H:i"|default:"&nbsp;" }}
                                            </td>
                                            <td>
                                                {{ medium.created|date:_"d M Y, H:i"|default:"&nbsp;" }}
                                                <div class="pull-right buttons">
                                                    {% button
                                                        class="btn btn-default btn-xs"
                                                        text=_"delete"
                                                        disabled=id.is_protected
                                                        action={
                                                            dialog_delete_rsc
                                                            id=id
                                                            on_success={
                                                                slide_fade_out
                                                                target=#li.id
                                                            }
                                                        }
                                                    %}
                                                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                                                </div>
                                            </td>
                                        </tr>
                                    {% endwith %}
                                {% endif %}
                            {% empty %}
                                <tr>
                                    <td colspan="6">
                                        {_ No media found. _}
                                    </td>
                                </tr>
                            {% endfor %}
                        </tbody>
                    </table>
                    {% pager result=result dispatch="admin_media" qargs hide_single_page %}
                    <div class="text-muted clear-left">
                        {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
                        {% else %}{% trans "{n} items found." n=result.total %}
                        {% endif %}
                    </div>
                {% endwith %}
            {% endwith %}

        {% endwith %}
    {% endwith %}

    {% include "_admin_overview_bulk_actions.tpl" %}

{% endblock %}
