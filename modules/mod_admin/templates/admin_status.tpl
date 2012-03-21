{% extends "admin_base.tpl" %}

{% block title %}{_ System Status _}{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ System administration _}</h2>
    <p>{_ This page holds a collection of administrative options to perform certain tasks on this Zotonic installation. _}</p>
</div>

<div>
    <div class="well">
	<div class="control-group">
            <div class="controls">
                {% button class="btn" text=_"Flush system caches" action={admin_tasks task='flush'} %} 
                <span class="help-inline">{_ Flush all URL dispatch rules, template- and library caches and other in-memory cached data. _}</span>
            </div>
        </div>

	<div class="control-group">
            <div class="controls">
                {% button class="btn" text=_"Rebuild search indices" action={admin_tasks task='pivot_all'} %}
                <span class="help-inline">{_ Rebuild all search-indices by putting all pages and data from the database in the indexer queue. This can take a long time! _}</span>
            </div>
        </div>

	<div class="control-group">
            <div class="controls">
                {% button class="btn" text=_"Renumber category tree" action={admin_tasks task='renumber_categories'} %}
                <span class="help-inline">{_ Recalculate the numbering of the category tree. This can take a long time. _}</span>
            </div>
        </div>
        
	{% all include "_admin_status.tpl" %}

    </div>
</div>

{% endblock %}
