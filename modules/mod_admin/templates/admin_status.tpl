{% extends "admin_base.tpl" %}

{% block title %}{_ System Status _}{% endblock %}

{% block content %}

<style>
    span.expl { float: left;margin-top: 3px; }
</style>

<div id="content" class="zp-85">
    <div class="block clearfix">
		<div class="clearfix">

            <h2>{_ System administration _}</h2>
			<div class="clearfix">
                {% button class="" text=_"Flush system caches" action={admin_tasks task='flush'} %} 
                <span class="expl">{_ Flush all URL dispatch rules, template- and library caches and other in-memory cached data. _}</span>
            </div>

			<div class="clearfix">
                {% button class="" text=_"Rebuild search indices" action={admin_tasks task='pivot_all'} %}
                <span class="expl">{_ Rebuild all search-indices by putting all pages and data from the database in the indexer queue. This can take a long time! _}</span>
            </div>

			<div class="clearfix">
                {% button class="" text=_"Renumber category tree" action={admin_tasks task='renumber_categories'} %}
                <span class="expl">{_ Recalculate the numbering of the category tree. This can take a long time. _}</span>
            </div>

			{% all include "_admin_status.tpl" %}

		</div>
		
		<hr />
		
    </div>
</div>

{% endblock %}
