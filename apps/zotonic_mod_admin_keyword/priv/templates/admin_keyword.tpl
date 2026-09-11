{% extends "admin_base.tpl" %}

{% block title %}{_ Keyword usage _}{% endblock %}
{% block bodyclass %}admin-page keyword-dashboard-page{% endblock %}

{% block head_extra %}
    {% lib "css/admin-keyword.css" %}
{% endblock %}

{% block content %}
    {% with m.admin_keyword.dashboard::%{
        keyword_category: q.qkeyword_category,
        content_category: q.qcontent_category,
        publication: q.qpublication,
        minimum: q.qminimum,
        matrix_limit: q.qmatrix_limit,
        metric: q.qmetric
    } as dashboard %}
        <div class="admin-header">
            <h2>{_ Keyword usage _}</h2>
            <p>{_ See which keywords are used and which keywords occur on the same content. _}</p>
        </div>

        <form action="{% url admin_keyword %}" method="get" class="panel panel-default keyword-dashboard__filters">
            <div class="panel-heading keyword-dashboard__filter-heading">
                <strong>{_ Filters _}</strong>
                {% if dashboard.is_available and not dashboard.has_error %}
                    <span class="keyword-dashboard__resource-count">
                        {% if dashboard.filters.content_category %}
                            {% trans "Analyzing {count} resources in {category} and its subcategories"
                                count=dashboard.content_count
                                category=m.rsc[dashboard.filters.content_category].title %}
                        {% else %}
                            {% trans "Analyzing {count} resources across all content categories"
                                count=dashboard.content_count %}
                        {% endif %}
                    </span>
                {% endif %}
            </div>
            <div class="panel-body">
                <div class="keyword-dashboard__filter-grid">
                    <div class="form-group">
                        <label for="keyword-category">{_ Keyword category _}</label>
                        <select id="keyword-category" name="qkeyword_category" class="form-control">
                            {% for category in m.category.keyword.tree_flat %}
                                <option value="{{ category.id }}"{% if category.id == dashboard.filters.keyword_category %} selected{% endif %}>
                                    {{ category.indent }}{{ category.id.title|default:category.id.name }}
                                </option>
                            {% endfor %}
                        </select>
                    </div>

                    <div class="form-group">
                        <label for="content-category">{_ Content category _}</label>
                        <select id="content-category" name="qcontent_category" class="form-control">
                            <option value="">{_ All content categories _}</option>
                            {% for category in m.category.tree_flat %}
                                <option value="{{ category.id }}"{% if category.id == dashboard.filters.content_category %} selected{% endif %}>
                                    {{ category.indent }}{{ category.id.title|default:category.id.name }}
                                </option>
                            {% endfor %}
                        </select>
                    </div>

                    <div class="form-group">
                        <label for="publication">{_ Publication flag _}</label>
                        <select id="publication" name="qpublication" class="form-control">
                            <option value="all"{% if dashboard.filters.publication == "all" %} selected{% endif %}>{_ All content _}</option>
                            <option value="published"{% if dashboard.filters.publication == "published" %} selected{% endif %}>{_ Published _}</option>
                            <option value="unpublished"{% if dashboard.filters.publication == "unpublished" %} selected{% endif %}>{_ Unpublished _}</option>
                        </select>
                    </div>

                    <div class="form-group">
                        <label for="minimum">{_ Minimum uses _}</label>
                        <select id="minimum" name="qminimum" class="form-control">
                            {% for value in [0, 1, 2, 5, 10, 25] %}
                                <option value="{{ value }}"{% if value == dashboard.filters.minimum %} selected{% endif %}>{{ value }}</option>
                            {% endfor %}
                        </select>
                    </div>

                    <div class="form-group">
                        <label for="matrix-limit">{_ Keywords in overlap matrix _}</label>
                        <select id="matrix-limit" name="qmatrix_limit" class="form-control">
                            {% for value in [10, 15, 20, 25, 30] %}
                                <option value="{{ value }}"{% if value == dashboard.filters.matrix_limit %} selected{% endif %}>{{ value }}</option>
                            {% endfor %}
                        </select>
                    </div>

                    <div class="form-group">
                        <label for="metric">{_ Overlap measure _}</label>
                        <select id="metric" name="qmetric" class="form-control">
                            <option value="jaccard"{% if dashboard.filters.metric == "jaccard" %} selected{% endif %}>{_ Jaccard similarity _}</option>
                            <option value="overlap"{% if dashboard.filters.metric == "overlap" %} selected{% endif %}>{_ Smaller-set overlap _}</option>
                            <option value="count"{% if dashboard.filters.metric == "count" %} selected{% endif %}>{_ Common content count _}</option>
                        </select>
                    </div>
                </div>

                <div class="keyword-dashboard__filter-actions">
                    <button type="submit" class="btn btn-primary">{_ Apply filters _}</button>
                    <a href="{% url admin_keyword %}" class="btn btn-default">{_ Reset _}</a>
                </div>
            </div>
        </form>

        {% if dashboard.has_error %}
            <div class="alert alert-danger">
                {_ The keyword analysis could not be calculated. Check the server log for details. _}
            </div>
        {% elseif not dashboard.is_available %}
            <div class="alert alert-info">
                {_ This site does not have a keyword category. _}
            </div>
        {% else %}
            <ul class="nav nav-tabs keyword-dashboard__tabs" role="tablist">
                <li class="active" role="presentation">
                    <a href="#keyword-usage-view"
                       id="keyword-usage-tab"
                       role="tab"
                       aria-controls="keyword-usage-view"
                       aria-expanded="true"
                       data-toggle="tab"
                       data-bs-toggle="tab">
                        {_ Usage _}
                    </a>
                </li>
                <li role="presentation">
                    <a href="#keyword-overlap-view"
                       id="keyword-overlap-tab"
                       role="tab"
                       aria-controls="keyword-overlap-view"
                       aria-expanded="false"
                       data-toggle="tab"
                       data-bs-toggle="tab">
                        {_ Heat map _}
                    </a>
                </li>
                <li role="presentation">
                    <a href="#keyword-pairs-view"
                       id="keyword-pairs-tab"
                       role="tab"
                       aria-controls="keyword-pairs-view"
                       aria-expanded="false"
                       data-toggle="tab"
                       data-bs-toggle="tab">
                        {_ Ranked pairs _}
                    </a>
                </li>
            </ul>

            <div class="tab-content keyword-dashboard__views">
            <section id="keyword-usage-view"
                     class="tab-pane active panel panel-default"
                     role="tabpanel"
                     aria-labelledby="keyword-usage-tab">
                <div class="panel-heading keyword-dashboard__panel-heading">
                    <h3 id="keyword-usage-title" class="panel-title">{_ Keyword usage _}</h3>
                    <span class="text-muted">
                        {% trans "Showing {shown} of {matched} keywords" shown=dashboard.displayed_keywords matched=dashboard.total_keywords %}
                    </span>
                </div>
                <div class="panel-body">
                    {% if dashboard.usage %}
                        <ol class="keyword-usage" aria-label="{_ Keyword usage histogram _}">
                            {% for keyword in dashboard.usage %}
                                <li class="keyword-usage__row">
                                    <div class="keyword-usage__label">
                                        {% include "_admin_keyword_link.tpl" keyword=keyword %}
                                        <small>{{ keyword.category_title|escape }}</small>
                                    </div>
                                    <progress max="{{ dashboard.max_usage|default:1 }}" value="{{ keyword.usage_count }}">
                                        {{ keyword.usage_count }}
                                    </progress>
                                    <a class="keyword-usage__count"
                                       href="{% url admin_overview_rsc qhasobject=keyword.id qhasobjectpredicate=`subject` %}"
                                       title="{_ View content using this keyword _}">
                                        {{ keyword.usage_count }}
                                    </a>
                                </li>
                            {% endfor %}
                        </ol>
                        {% if dashboard.is_truncated %}
                            <p class="help-block">
                                {% trans "Only the first {count} keywords are shown. Use the filters to narrow the list." count=dashboard.displayed_keywords %}
                            </p>
                        {% endif %}
                    {% else %}
                        <p class="text-muted">{_ No keywords match these filters. _}</p>
                    {% endif %}
                </div>
            </section>

            <section id="keyword-overlap-view"
                     class="tab-pane panel panel-default"
                     role="tabpanel"
                     aria-labelledby="keyword-overlap-tab">
                <div class="panel-heading keyword-dashboard__panel-heading">
                    <h3 id="keyword-overlap-title" class="panel-title">{_ Keyword overlap _}</h3>
                    <span class="text-muted">
                        {% if dashboard.filters.metric == "count" %}
                            {_ Cells show the number of content items using both keywords. _}
                        {% elseif dashboard.filters.metric == "overlap" %}
                            {_ Cells show which percentage of the smaller keyword set is shared. _}
                        {% else %}
                            {_ Cells show the shared percentage of the combined keyword sets. _}
                        {% endif %}
                    </span>
                </div>
                <div class="panel-body">
                    {% if dashboard.matrix %}
                        <div class="keyword-overlap-matrix" tabindex="0" aria-label="{_ Scrollable keyword overlap matrix _}">
                            <table class="keyword-overlap-matrix__table">
                                <caption class="sr-only">
                                    {_ Pairwise keyword overlap. The diagonal shows the usage count of each keyword. _}
                                </caption>
                                <thead>
                                    <tr>
                                        <th scope="col" class="keyword-overlap-matrix__corner">{_ Keyword _}</th>
                                        {% for keyword in dashboard.matrix_keywords %}
                                            <th scope="col" title="{{ keyword.title|escape }}">
                                                <span class="keyword-overlap-matrix__column-label">{{ keyword.title|escape }}</span>
                                            </th>
                                        {% endfor %}
                                    </tr>
                                </thead>
                                <tbody>
                                    {% for row in dashboard.matrix %}
                                        <tr>
                                            <th scope="row" title="{{ row.keyword.title|escape }}">
                                                {{ row.keyword.title|escape }}
                                            </th>
                                            {% for cell in row.cells %}
                                                {% if cell.is_diagonal %}
                                                    <td class="keyword-overlap-matrix__cell is-diagonal"
                                                        title="{% trans "{keyword}: {count} uses" keyword=cell.keyword.title|escape count=cell.usage_count %}">
                                                        {{ cell.usage_count }}
                                                    </td>
                                                {% else %}
                                                    <td class="keyword-overlap-matrix__cell is-level-{{ cell.bucket }}"
                                                        title="{% trans "{row_keyword} &amp; {column_keyword}: {count} common items" row_keyword=row.keyword.title|escape column_keyword=cell.keyword.title|escape count=cell.intersection %}">
                                                        {{ cell.score }}{% if dashboard.is_percent %}%{% endif %}
                                                    </td>
                                                {% endif %}
                                            {% endfor %}
                                        </tr>
                                    {% endfor %}
                                </tbody>
                            </table>
                        </div>
                    {% else %}
                        <p class="text-muted">{_ At least two used keywords are needed for an overlap matrix. _}</p>
                    {% endif %}
                </div>
            </section>

            <section id="keyword-pairs-view"
                     class="tab-pane panel panel-default"
                     role="tabpanel"
                     aria-labelledby="keyword-pairs-tab">
                <div class="panel-heading keyword-dashboard__panel-heading">
                    <h3 id="keyword-pairs-title" class="panel-title">{_ Strongest keyword pairs _}</h3>
                    <span class="text-muted">{_ Ranked by the selected overlap measure. _}</span>
                </div>
                <div class="table-responsive">
                    <table class="table table-striped table-condensed keyword-pairs">
                        <thead>
                            <tr>
                                <th>{_ Keyword _}</th>
                                <th>{_ Keyword _}</th>
                                <th class="text-right">{_ Common _}</th>
                                <th class="text-right{% if dashboard.filters.metric == "jaccard" %} is-active{% endif %}">{_ Jaccard _}</th>
                                <th class="text-right{% if dashboard.filters.metric == "overlap" %} is-active{% endif %}">{_ Smaller set _}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {% for pair in dashboard.pairs %}
                                <tr>
                                    <td>
                                        {% include "_admin_keyword_link.tpl" keyword=pair.a %}
                                        <small class="text-muted">{{ pair.a.usage_count }}</small>
                                    </td>
                                    <td>
                                        {% include "_admin_keyword_link.tpl" keyword=pair.b %}
                                        <small class="text-muted">{{ pair.b.usage_count }}</small>
                                    </td>
                                    <td class="text-right{% if dashboard.filters.metric == "count" %} is-active{% endif %}">{{ pair.intersection }}</td>
                                    <td class="text-right{% if dashboard.filters.metric == "jaccard" %} is-active{% endif %}">{{ pair.jaccard }}%</td>
                                    <td class="text-right{% if dashboard.filters.metric == "overlap" %} is-active{% endif %}">{{ pair.overlap }}%</td>
                                </tr>
                            {% empty %}
                                <tr>
                                    <td colspan="5" class="text-muted">{_ No keyword pairs have content in common. _}</td>
                                </tr>
                            {% endfor %}
                        </tbody>
                    </table>
                </div>
            </section>
            </div>

            <div class="well well-sm keyword-dashboard__method">
                <strong>{_ How to read the overlap _}</strong>
                <p>
                    {_ <em>Jaccard similarity</em> divides the common content by all distinct content using either keyword. <em>Smaller-set overlap</em> divides it by the least-used keyword and helps reveal keywords that may be redundant or nested. Very small keyword sets can score highly, so use the minimum-uses filter when looking for meaningful patterns. _}
                </p>
            </div>
        {% endif %}
    {% endwith %}
{% endblock %}
