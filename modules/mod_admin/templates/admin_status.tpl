{% extends "admin_base.tpl" %}

{% block title %}{_ System Status _}{% endblock %}

{% block content %}

    <div class="edit-header">
        <h2>{_ System administration _}</h2>
        <p>{_ This page holds a collection of administrative options to perform certain tasks on this Zotonic installation. _}</p>
    </div>

    <div class="row">
        <div class="col-md-6">
            <div class="well">
	            <div class="form-group">
                    <div>
                        {% button class="btn btn-default" text=_"Flush system caches" action={admin_tasks task='flush'} %} 
                        <span class="help-block">{_ Flush all URL dispatch rules, template- and library caches and other in-memory cached data. _}</span>
                    </div>
                </div>

	            <div class="form-group">
                    <div>
                        {% button
                            class="btn btn-default"
                            id="btn-rebuild-indices"
                            text=_"Rebuild search indices"
                            action={
                                admin_tasks
                                task='pivot_all'
                            }
                            action={
                                script
                                script="queueCountInfo('#pivot-queue-count', '#btn-rebuild-indices')"
                            }
                        %}
                        <span id="pivot-queue-count">
                            {% if m.admin_rsc.pivot_queue_count %}
                                {% javascript %}
                                    queueCountInfo('#pivot-queue-count', '#btn-rebuild-indices');
                                {% endjavascript %}
                            {% endif %}
                        </span>
                        <span class="help-block">{_ Rebuild all search-indices by putting all pages and data from the database in the indexer queue. This can take a long time! _}</span>
                    </span>
                </div>
            </div>

	        <div class="form-group">
                <div>
                    {% button class="btn btn-default" text=_"Renumber category tree" action={admin_tasks task='renumber_categories'} %}
                    <span class="help-block">{_ Recalculate the numbering of the category tree. This can take a long time. _}</span>
                </div>
            </div>

	        <div class="form-group">
                <div>
                    {% button class="btn btn-default" text=_"Reinstall site datamodel" action={admin_tasks task='site_reinstall'} %} 
                    <span class="help-block">{_ Runs the schema install command from the site's module again. _}</span>
                </div>
            </div>
            
        </div>
    </div>

    <div class="col-md-6">
        <div class="well">
	        {% all include "_admin_status.tpl" %}
        </div>
    </div>
</div>
{% include "_admin_status_update_pivot_count.tpl" %}
{% endblock %}
