{% extends "page.tpl" %}

{% block content %}
    {% inherit %}

    {% with id.medium as medium %}
    <table {% include "_language_attrs.tpl" language=z_language class="table table-bordered" %}>
        <tr>
            <th>{_ Filename _}</th>
            <td>{{ medium.original_filename|escape }}</td>
        </tr>
        <tr>
            <th>{_ Mime _}</th>
            <td>{{ medium.mime }}</td>
        </tr>
        <tr>
            <th>{_ Dimensions _}</th>
            <td>{{ medium.width }} x {{ medium.height }} {_ pixels _}</td>
        </tr>
        <tr>
            <th>{_ Size _}</th>
            <td>{{ medium.size|filesizeformat }}</td>
        </tr>
        <tr>
            <th>{_ Uploaded _}</th>
            <td>{{ medium.created|date:"Y-m-d H:i" }}</td>
        </tr>
        <tr>
            <th>{_ Links _}</th>
            <td>
                <a href="{% url media_attachment id=id %}">{_ Download file _}</a>
                <span class="separator">|</span>
                <a href="{% url media_inline id=id %}">{_ View in browser _}</a>
            </td>
        </tr>
    </table>
    {% endwith %}
    
    <p class="thumbnail">
        <a href="{% url media_inline id=id %}" title="{_ Click to download _}"><img src="{% image_url id mediaclass="base-media-preview" %}" alt="{_ Preview _}"/></a>
    </p>

    {% include "_content_list.tpl" list=id.s.haspart %}
    {% include "_content_list.tpl" list=id.s.depiction %}
{% endblock %}
