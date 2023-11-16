{% extends "_action_dialog_edit_basics.tpl" %}

{% block tabbar_extra %}
    <li><a data-toggle="tab" data-tab="media" href="#{{ #media }}">{_ Media content _}</a></li>
{% endblock %}

{% block tab_extra %}
    {% with id.medium as medium %}
        <div class="tab-pane" id="{{ #media }}">
            {% if medium.mime %}
                <div class="row">
                    <div class="col-md-6">
                        <div class="admin-edit-media" id="{{ #image }}" data-original-width="{{ medium.width }}">
                            {% if medium.width < 597 and medium.height < 597 %}
                                {% media medium mediaclass="admin-media-cropcenter" %}
                            {% else %}
                                {% media medium mediaclass="admin-media" %}
                            {% endif %}
                        </div>
                    </div>
                    <div class="col-md-6">

                        <dl>
                            <dt>{_ Content type _}</dt>
                            <dd>{{ medium.mime }}</dd>
                            {% if medium.width and medium.height %}
                                <dt>{_ Display size _}</dt>
                                <dd>{{ medium.width }} x {{ medium.height }} {_ pixels _}</dd>
                            {% endif %}
                            {% if medium.duration %}
                                <dt>{_ Duration _}</dt>
                                <dd>{{ medium.duration|format_duration }}</dd>
                            {% endif %}
                            {% if medium.size %}
                                <dt>{_ File size _}</dt>
                                <dd>{{ medium.size|filesizeformat }}</dd>
                            {% endif %}
                            {% if medium.filename %}
                                <dt>{_ Filename _}</dt>
                                <dd>{{ medium.filename }}</dd>
                            {% endif %}
                            <dt>{_ Uploaded on _}</dt>
                            <dd>{{ medium.created|date:"Y-m-d H:i:s" }}</dd>
                        </dl>

                        {% include "_edit_medium_language.tpl" %}
                    </div>
                </div>
            {% elseif medium.created %}
                <p>
                    {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
                </p>
            {% else %}
                <p class="text-muted">
                    {_ No media content. _}
                </p>
            {% endif %}
        </div>
    {% endwith %}
{% endblock %}