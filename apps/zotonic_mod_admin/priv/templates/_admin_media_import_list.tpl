<div id="media-import-wrapper">

{% if error %}
    <p class="alert alert-danger">
        {{ error }}
    </p>
{% elseif not media_imports  %}
    <p class="alert alert-warning">
        {_ Could not detect anything to import on that URL or embed code. _}
    </p>
{% else %}

    {% wire action={hide target=form_id} %}

    {% for mi in media_imports %}
    {% with forloop.counter as index %}

        {% wire id=#import.index type="submit"
                postback={media_url_import media=mi.media_import args=args}
                delegate=`z_admin_media_discover`
        %}
        <div id="{{ #panel.index}}" class="panel panel-default" {% if index > 1 %}style="display:none"{% endif %}>
            <form id="{{ #import.index }}" method="POST" class="form" action="postback">
                <div class="panel-body">
                    <h4>
                        <span class="label label-default">{{ m.rsc[mi.category].title }}</span>
                        {{ mi.props.title|escape }}
                    </h4>

                    {% if not args.id and not mi.props.title %}
                        <div class="form-group">
                            <input type="text" class="form-control" name="new_media_title" id="{{ #title.index }}" value="{{ mi.props.title|escape }}" placeholder="{_ Title _}" />
                        </div>
                    {% endif %}

                    {% if mi.props.summary %}
                        <p>{{ mi.props.summary|escape }}</p>
                    {% endif %}

                    {% if mi.medium_url %}
                        <p>
                            {% if mi.category == `image` %}
                                <img src="{{ mi.medium_url|escape }}" class="img-responsive">
                            {% elseif mi.category == 'video' %}
                                <video width="640" controls class="img-responsive">
                                    <source src="{{ mi.medium_url|escape }}" type="{{ mi.medium.mime|escape }}">
                                </video>
                            {% elseif mi.category == 'audio' %}
                                <audio width="480" controls class="img-responsive">
                                    <source src="{{ mi.medium_url|escape }}" type="{{ mi.medium.mime|escape }}">
                                </audio>
                            {% endif %}
                        </p>
                    {% elseif mi.medium %}
                        {% media mi.medium %}
                    {% elseif mi.preview_url %}
                        <p>
                            <img src="{{ mi.preview_url|escape }}" class="img-responsive">
                        </p>
                    {% endif %}

                    {% if mi.props.website %}
                        <p>
                            <span class="glyphicon glyphicon-link"></span>
                            <a href="{{ mi.props.website|escape }}" target="_blank">{{ mi.props.website|truncate:120:"..."|escape }}</a>
                        </p>
                    {% endif %}

                    {% if args.intent != 'update' %}
                        {% if args.subject_id %}
                            {% if m.admin.rsc_dialog_hide_dependent and not m.acl.is_admin %}
                                <input type="hidden" name="is_dependent" value="{% if args.dependent %}1{% endif %}">
                            {% else %}
                                <div class="checkbox form__is_dependent">
                                    <label>
                                        <input type="checkbox" id="{{ #dependent.index }}" name="is_dependent" value="1" {% if args.dependent %}checked{% endif %}>
                                        {_ Delete if not connected anymore _}
                                    </label>
                                </div>
                            {% endif %}
                        {% endif %}

                        <div class="checkbox form__is_published">
                            <label>
                                <input type="checkbox" id="{{ #published.index }}" name="is_published" value="1"
                                    {% if args.subject_id or m.admin.rsc_dialog_is_published %}
                                        checked
                                    {% endif %}>
                                {_ Published _}
                            </label>
                        </div>
                    {% endif %}
                </div>

                <div class="panel-footer clearfix">
                    <a href="#" id="{{ #back.index }}" class="btn btn-default">{_ Back _}</a>
                    {% wire id=#back.index
                            action={hide target=discover_id}
                            action={fade_in target=form_id}
                    %}

                    {% with index-1,
                            index+1
                         as prev,
                            next
                    %}
                        {% if forloop.first %}
                            <a href="#" id="{{ #prev.index }}" class="btn btn-default disabled">{_ &lt; Prev _}</a>
                        {% else %}
                            <a href="#" id="{{ #prev.index }}" class="btn btn-default">{_ &lt; Prev _}</a>
                            {% wire id=#prev.index action={hide target=#panel.index} action={fade_in speed="fast" target=#panel.prev} %}
                        {% endif %}
                        {% if forloop.last %}
                            <a href="#" id="{{ #prev.index }}" class="btn btn-default disabled">{_ Next &gt; _}</a>
                        {% else %}
                            <a href="#" id="{{ #next.index }}" class="btn btn-default">{_ Next &gt; _}</a>
                            {% wire id=#next.index action={hide target=#panel.index} action={fade_in speed="fast" target=#panel.next} %}
                        {% endif %}
                    {% endwith %}

                    <button type="submit" class="btn btn-primary pull-right">{% if args.intent == 'update' %}{_ Replace _}{% else %}{_ Make _}{% endif %} {{ mi.description }}</button>
                </div>
            </form>
        </div>
    {% endwith %}
    {% endfor %}

    {% javascript %}
        $('#media-import-wrapper').on('change', 'input[type=checkbox]', function() {
            var name = $(this).attr('name');
            var id = $(this).attr('id');
            var is_checked = $(this).is(':checked');
            $('#media-import-wrapper').find('input[type=checkbox]').each(
                function() {
                    if ($(this).attr('id') != id && $(this).attr('name') == name) {
                        $(this).prop('checked', is_checked);
                    }
                });
        });
    {% endjavascript %}

{% endif %}

</div>
