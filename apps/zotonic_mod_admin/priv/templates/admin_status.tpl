{% extends "admin_base.tpl" %}

{% block title %}{_ System Status _}{% endblock %}

{% block content %}

    <div class="admin-header">
        <h2>{_ System administration _}</h2>
        <p>{_ This page holds a collection of administrative options to perform certain tasks on this Zotonic installation. _}</p>
    </div>

    <div class="row">
        <div class="col-md-12">
            {% include "_admin_status_alert.tpl" %}
        </div>
    </div>

    <div class="row">
        <div class="col-md-6">
            <div class="widget">
                <div class="widget-content">
    	            <div class="form-group">
                        {% button class="btn btn-default" text=_"Flush system caches" action={admin_tasks task='flush'} %}
                        <p class="help-block">{_ Flush all URL dispatch rules, template- and library caches and other in-memory cached data. _}</p>
                    </div>

    	            <div class="form-group">
                        {% button
                            id="btn-rebuild-indices"
                            class="btn btn-default"
                            text=_"Rebuild search indices"
                            action={admin_tasks task='pivot_all'}
                            action={script script="queueCountInfo('#pivot-queue-count', '#btn-rebuild-indices')"}
                        %}
                        <span id="pivot-queue-count">
                            {% javascript %}
                                queueCountInfo('#pivot-queue-count', '#btn-rebuild-indices');
                            {% endjavascript %}
                        </span>
                        <p class="help-block">{_ Rebuild all search-indices by putting all pages and data from the database in the indexer queue. This can take a long time! _}
                        </p>
                    </div>

        	        <div class="form-group">
                        {% button class="btn btn-default" text=_"Renumber category tree" action={admin_tasks task='renumber_categories'} %}
                        <p class="help-block">{_ Recalculate the numbering of the category tree. This can take a long time. _}</p>
                    </div>

        	        <div class="form-group">
                        {% button class="btn btn-default" text=_"Reinstall site datamodel" action={admin_tasks task='site_reinstall'} %}
                        <p class="help-block">{_ Runs the schema install command from the site's module again. _}</p>
                    </div>
                </div>
            </div>
        </div>

        <div class="col-md-6">
            <div class="widget">
                <div class="widget-content">
        	        {% all include "_admin_status.tpl" %}
                </div>
            </div>

            <div class="widget">
                <div class="widget-content">
                    {% all include "_admin_system_status.tpl" %}
                </div>
            </div>
        </div>
    </div>

    {% include "_admin_status_update_pivot_count.tpl" %}

{% endblock %}
