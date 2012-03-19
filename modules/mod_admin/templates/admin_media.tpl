{% extends "admin_base.tpl" %}

{% block title %}{_ Media _}{% endblock %}

{% block search_target %}{% url admin_media %}{% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Media _}</h2>

    <p>{_ Media encompasses all uploaded images, movies and documents. Media can be attached to pages. _}</p>

    <div class="well">
	{% button
                class="btn btn-primary"
	        text=_"Make a new media item" 
	        action={dialog_media_upload}
	        %}
    </div>

    {% with m.search.paged[{query cat="media" text=q.qs page=q.page sort=q.qsort|default:"-created"}] as result %}

    <h3 class="above-list">{_ Media overview _}</h3>
    <hr />

    <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
		<th width="10%">{_ Preview _}</th>
 		<th width="20%">{% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" %}</th>
		<th width="15%">{_ Type _}</th>
		<th width="25%">{_ Filename _}</th>
		<th width="10%">{_ Dimensions _}</th>
		<th width="20%">{% include "_admin_sort_header.tpl" field="created" caption=_"Uploaded" %}</th>
            </tr>
        </thead>

        <tbody>
	    {% for id in result %}
	    {% if m.rsc[id].is_visible %}
	    {% with m.rsc[id] as r %}
	    {% with r.medium as medium %}
	    <tr id="{{ #li.id }}" {% if not m.rsc[id].is_published %}class="unpublished" {% endif %}>
		<td width="10%">{% image medium width=80 height=60 crop %}</td>
		<td width="20%">{{ r.title|striptags|default:"<em>untitled</em>" }}</td>
		<td width="15%">{{ medium.mime|default:"&nbsp;" }}</td>
		<td width="25%">{{ medium.filename|default:"-" }}</td>
		<td width="10%">{{ medium.width }}&times;{{ medium.height }}</td>
		<td width="10%">
                    {{ medium.created|date:"M d, H:i"|default:"&nbsp;" }}
                    <div class="pull-right">
			{% button class="btn btn-mini" text=_"delete" disabled=r.is_protected action={dialog_delete_rsc
                        id=id on_success={slide_fade_out target=#li.id}} %}
                        <a href="{% url admin_edit_rsc id=id %}" class="btn btn-mini row-link">{_ edit _}</a>
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
