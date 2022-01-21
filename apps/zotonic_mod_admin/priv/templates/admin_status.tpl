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

            <div class="widget">
                <div class="widget-content">
                    <div class="row">
                        <div class="col-md-6">
                            <dl class="dl-horizontal">
                                <dt>{_ Zotonic Version _}</dt>
                                <dd>{{ m.admin_status.zotonic_version }}</dd>

                                {% if m.acl.is_admin %} {# Only admins are allowed to see the all versions #}
                                    <dt>{_ Erlang Version _}</dt>
                                    <dd>{{ m.admin_status.otp_version }}</dd>

                                    <dt>{_ Database Version _}</dt>
                                    <dd>{{ m.admin_status.database_version }}</dd>
                                {% endif %}
                            </dl>
                        </div>
                        <div class="col-md-6">
                            <dl class="dl-horizontal">
                                {% if m.acl.is_admin %} {# Only admins are allowed to see the full paths #}
                                    <dt>{_ Site Files Directory _}</dt>
                                    <dd>{{ m.admin_status.files_dir }}</dd>

                                    <dt>{_ Data Directory _}</dt>
                                    <dd>{{ m.admin_status.data_dir }}</dd>

                                    <dt>{_ Log Directory _}</dt>
                                    <dd>{{ m.admin_status.log_dir }}</dd>

                                    <dt>{_ Config Directory _}</dt>
                                    <dd>{{ m.admin_status.config_dir }}</dd>

                                    <dt>{_ Erlang Init Files _}</dt>
                                    <dd>{{ m.admin_status.init_arguments.config | join:"<br>" }}</dd>

                                    <dt>{_ Security Directory _}</dt>
                                    <dd>{{ m.admin_status.security_dir }}</dd>

                                    <dt>{_ Home Directory _}</dt>
                                    <dd>{{ m.admin_status.init_arguments.home }}</dd>

                                    <dt>{_ Work Directory _}</dt>
                                    <dd>{{ m.admin_status.work_dir }}</dd>

                                    <dt>{_ Erlang Installation Root  _}</dt>
                                    <dd>{{ m.admin_status.init_arguments.root }}</dd>
                                {% endif %}
                            </dl>
                        </div>
                    </div>
                </div>
            </div>
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

            <div class="widget">
                <div class="widget-content">
                    {% all include "_admin_system_status.tpl" %}
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
                    <h4>{_ Task queue _}</h4>

                    <table class="table">
                        <thead>
                            <tr>
                                <th>{_ Module _}</th>
                                <th>{_ Function _}</th>
                                <th>{_ Count _}</th>
                                <th>{_ First due _}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {% for t in m.admin_status.task_queue %}
                                <tr>
                                    <td>{{ t.module|escape }}</td>
                                    <td>{{ t.function|escape }}</td>
                                    <td>{{ t.count }}</td>
                                    <td>
                                        {{ t.due|date:"Y-m-d H:i:s" }}
                                        {% if t.error_count_total %}
                                            <br>
                                            <span class="text-danger">
                                                <span class="fa fa-warning"></span>
                                                {% trans "Some tasks are retrying.<br>Total errors: {total}<br>Highest for single task: {max}"
                                                        total=t.error_count_total
                                                        max=t.error_count_max
                                                %}
                                            </span>
                                        {% endif %}
                                    </td>
                                </tr>
                            {% empty %}
                                <tr>
                                    <td colspan="3">
                                        <p>
                                            {_ There are no tasks in the task queue. _}
                                        </p>
                                    </td>
                                </tr>
                            {% endfor %}
                        </tbody>
                    </table>

                    <p class="help-block">
                        {_ Tasks are functions scheduled to run at a certain time. _}<br>
                        {_ Tasks with errors will retry five times before being dropped. _}
                    </p>
                </div>
            </div>
        </div>
    </div>

    {% include "_admin_status_update_pivot_count.tpl" %}

{% endblock %}
