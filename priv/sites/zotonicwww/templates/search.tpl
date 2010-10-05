{% extends "base.tpl" %}

{% block title %}{{ q.q|escape|default:"Search" }}{% endblock %}

{% block page_class %}page category{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

    {% if not q.q %}

	<article id="content" class="zp-33">
		<div class="padding">
            <p class="breadcrumb">
                <a href="{{ m.rsc.page_home.page_url }}">{{ m.rsc.page_home.short_title | default: m.rsc.page_home.title}}</a> &raquo;
                search
            </p>

            {% include "_block_search.tpl" title="Search" %}
		</div>
	</article>

    {% else %}

    {% with m.search.paged[{query text=q.q cat="documentation" pagelen=30 page=q.page}] as result %}

	<article id="content" class="zp-33">
		<div class="padding">
            <p class="breadcrumb">
                <a href="{{ m.rsc.page_home.page_url }}">{{ m.rsc.page_home.short_title | default: m.rsc.page_home.title}}</a> &raquo;
                search
            </p>

            {% if q.q %}
            <h2>{{ result.total }} result{% if result.total > 1 %}s{% endif %} found for &quot;{{ q.q|escape }}&quot;</h2>
            {% pager dispatch="search" result=result qargs hide_single_page=1 %}
            {% endif %}


            {% include "_block_search.tpl" title="Search again" %}
		</div>
	</article>


    {% with result|split_in:2 as a, b %}
        <section class="collection-members zp-33">
            <div class="padding">
                <ul class="item-list">
                    {% for id in a %}
                    <li class="list-item">
                        <h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
                        <p class="summary">{{ m.rsc[id].summary }}</p>
                    </li>
                    {% endfor %}
                </ul>
            </div>
        </section>
        <section class="collection-members zp-33">
            <div class="padding">
                <ul class="item-list">
                    {% for id in b %}
                    <li class="list-item">
                        <h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
                        <p class="summary">{{ m.rsc[id].summary }}</p>
                    </li>
                    {% endfor %}
                </ul>
            </div>
        </section>

    {% endwith %}
	{% endwith %}
    {% endif %}{# q.q #}

{% endblock %}
