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
            <span class="pull-right text-muted">{{ m.rsc[mi.category].title }}</span>

            {% if mi.props.title %}
                <h4>{{ mi.props.title|escape }}</h4>
            {% elseif not id %}
                <div class="form-group">
                    <input type="text" class="form-control" name="title" id="{{ #title.index }}" value="{{ mi.props.title|escape }}" placeholder="{_ Title _}" />
                    {% validate id=#title.index name="title" type={presence} %}
                </div>
            {% endif %}

            {% if mi.props.summary %}
                <p>{{ mi.props.summary|escape }}</p>
            {% endif %}

            {% if mi.medium_url %}
                <p>
                    {% if mi.category == `image` %}
                        <img src="{{ mi.medium_url|escape }}" class="img-responsive" />
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
                <div class="embed-responsive embed-responsive-16by9">
                    {% media mi.medium %}
                </div>
            {% elseif mi.preview_url %}
                <p>
                    <img src="{{ mi.preview_url|escape }}" class="img-responsive" />
                </p>
            {% endif %}

            {% if mi.props.website %}
                <p>
                    <a href="{{ mi.props.website|escape }}" target="_blank">{{ mi.props.website|truncate:120:"..."|escape }}</a>
                </p>
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

            <button type="submit" class="btn btn-primary pull-right">{% if args.id %}{_ Replace _}{% else %}{_ Make _}{% endif %} {{ mi.description }}</button>
        </div>
    </form>
    </div>
    {% endwith %}
    {% endfor %}
{% endif %}
