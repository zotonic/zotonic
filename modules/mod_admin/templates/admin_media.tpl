{% extends "admin_base.tpl" %}

{% block title %}{_ Media _}{% endblock %}

{% block search_target %}{% url admin_media %}{% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Media _}</h2>

    <p>
        {_ Media encompasses all uploaded images, movies and documents. Media can be attached to pages. _}
        {_ And media can also be viewed on their own page. _}
    </p>

    <div class="well">
        <a name="content-pager"></a>
	{% button
                class="btn btn-primary"
	        text=_"Make a new media item" 
	        action={dialog_media_upload}
	        %}
        <a class="btn" href="{% url admin_overview_rsc %}">{_ All pages _}</a>
        <a class="btn disabled" href="{% url admin_media %}">{_ All media _}</a>
    </div>

    {% with m.search.paged[{query cat="media" text=q.qs page=q.page sort=q.qsort|default:"-created"}] as result %}

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
		<th width="10%">{_ Preview _}</th>
 		<th width="35%">{% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}</th>
		<th width="25%">{_ Info _}</th>
		<th width="30%">{% include "_admin_sort_header.tpl" field="created" caption=_"Uploaded" %}</th>
            </tr>
        </thead>

        <tbody>
	    {% for id in result %}
	    {% if m.rsc[id].is_visible %}
	    {% with m.rsc[id] as r %}
	    {% with r.medium as medium %}
	    <tr id="{{ #li.id }}" {% if not m.rsc[id].is_published %}class="unpublished" {% endif %} data-href="{% url admin_edit_rsc id=id %}">
		<td>{% image medium mediaclass="admin-list-overview" class="thumb" %}</td>
		<td>
                    <h5>{{ r.title|striptags|default:"<em>untitled</em>" }}</h5>
                    <p class="help-block">{{ medium.filename|default:"-" }}</p>
                </td>
		<td>
                    <p class="help-block">
                        {{ medium.mime|default:"&nbsp;" }}<br />
		        {{ medium.width }}&times;{{ medium.height }}
                    </p>
                </td>
		<td>
                    {{ medium.created|date:"M d, H:i"|default:"&nbsp;" }}
                    <div class="pull-right">
			{% button class="btn btn-mini" text=_"delete" disabled=r.is_protected action={dialog_delete_rsc
                        id=id on_success={slide_fade_out target=#li.id}} %}
                        <a href="{% url admin_edit_rsc id=id %}" class="btn btn-mini">{_ edit _}</a>
                    </div>
                </td>
            </tr>
            {% endwith %}
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
    {% pager result=result dispatch="admin_media" qargs %}

    {% endwith %}
</div>

{% endblock %}
