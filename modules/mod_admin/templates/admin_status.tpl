{% extends "admin_base.tpl" %}

{% block title %} system status {% endblock %}

{% block content %}

<style>
    span.expl { float: left;margin-top: 3px; }
</style>

<div id="content" class="zp-85">
    <div class="block clearfix">
{#
		<h2>System status</h2>

            <p>This should show some counters, stats, etc.</p>
#}

		<div class="clearfix">

            <h2>System administration</h2>
			<div class="clearfix">
                {% button class="" text="Flush system caches" action={admin_flush} %} 
                <span class="expl">Flush all URL dispatch rules, template- and library caches and other in-memory cached data.</span>
            </div>

			<div class="clearfix">
                {% button class="" text="Rebuild search indices" action={admin_pivot_all} %}
                <span class="expl">Rebuild all search-indices by putting all pages and data from the database in the indexer queue. This can take a long time!</span>

            </div>

		</div>
		
		<hr />
		
    </div>
</div>

{% endblock %}
