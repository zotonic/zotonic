<div id="media-import-wrapper">

{% if error %}
    <br>
    <p class="alert alert-danger">
        {{ error }}
    </p>
{% elseif not media_imports  %}
    <br>
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
            {% with #import.index as form %}

                <div class="panel-body">
                    <h4>
                        <span class="label label-default">{{ m.rsc[mi.category].title }}</span>
                        {{ mi.props.title|escape_check }}
                    </h4>

                    {% if not args.id and not mi.props.title %}
                        <div class="form-group">
                            <input type="text" class="form-control" name="new_media_title" id="{{ #title.index }}" value="{{ mi.props.title|escape_check }}" placeholder="{_ Title _}" />
                        </div>
                    {% endif %}

                    {% if mi.props.summary %}
                        <p>{{ mi.props.summary|escape_check }}</p>
                    {% endif %}

                    {% if mi.medium_url %}
                        <p>
                            {% if m.rsc[mi.category].name == "image" %}
                                <img src="{{ mi.medium_url|escape }}" class="img-responsive">
                            {% elseif mi.category == 'video' %}
                                <video width="640" controls class="img-responsive">
                                    <source src="{{ mi.medium_url|escape }}" type="{{ mi.medium.mime|escape }}">
                                </video>
                            {% elseif m.rsc[mi.category].name == "audio" %}
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
                        {% with m.rsc[mi.category].id,
                                true
                             as cat,
                                nocatselect
                        %}
                        {% block import_options %}
                            <div class="row media-import__options">
                                <div class="col-md-6">
                                    {% block import_options__rsc %}
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

                                        {% if mi.medium and (
                                                   mi.props.is_authoritative|is_undefined
                                                or mi.props.is_authoritative)
                                        %}
                                            {% include "_edit_medium_language.tpl" %}
                                        {% endif %}

                                    {% endblock %}
                                </div>
                                <div class="col-md-6">
                                    {% if mi.props.is_authoritative|is_defined and not mi.props.is_authoritative %}
                                        {% block import_options__import %}
                                            <div class="form-group">
                                                <label class="control-label">{_ Create _}</label>
                                                <div class="radio form__is_authoritative">
                                                    <label>
                                                        <input type="radio" name="is_authoritative" value="0" {% if not is_authoritative %}checked{% endif %}>
                                                        {_ A copy that will remain connected so that a new version can be fetched. _}
                                                    </label>
                                                    <label>
                                                        <input type="radio" name="is_authoritative" value="1" {% if is_authoritative %}checked{% endif %}>
                                                        {_ A local copy, disconnected from the remote site. _}
                                                    </label>
                                                </div>
                                            </div>

                                            <div class="form-group form__import_edges">
                                                <label class="control-label">{_ Connections _}</label>
                                                <div class="radio">
                                                    <label>
                                                        <input type="radio" name="z_import_edges" value="0">
                                                        {_ Do not import connections. _}
                                                    </label>
                                                    <label>
                                                        <input type="radio" name="z_import_edges" value="1" checked>
                                                        {_ Import only direct connections (shallow copy). _}
                                                    </label>
                                                    <label>
                                                        <input type="radio" name="z_import_edges" value="10">
                                                        {_ Follow connections and import all (deep copy). _}
                                                    </label>
                                                </div>
                                                {% include "_rsc_import_deleted_options.tpl" import_edges=1 %}
                                            </div>

                                            {% if m.modules.active.mod_websub %}
                                                <div class="form-group form__subscribe">
                                                    <label class="control-label">{_ Subscribe _}</label>
                                                    <div class="checkbox">
                                                        <label>
                                                            <input type="checkbox" name="z_import_subscribe" value="1"
                                                                {% if not mi.props.is_websub_supported or is_authoritative %}disabled{% endif %}
                                                                data-websub-supported="{% if mi.props.is_websub_supported %}1{% else %}0{% endif %}">
                                                            {_ Automatically fetch updates from the original website. _}
                                                            {% if not mi.props.is_websub_supported %}
                                                                <a href="#" class="z-btn-help do_dialog"
                                                                    title="{_ Automatic updates unavailable _}" aria-label="{_ Automatic updates unavailable _}"
                                                                    data-dialog="{{ %{
                                                                        title: _"Automatic updates unavailable",
                                                                        text: _"The original website does not advertise automatic updates.",
                                                                        level: 10
                                                                    }|escape }}"></a>
                                                            {% endif %}
                                                        </label>
                                                        <div class="websub-sub-options" hidden>
                                                            <label>
                                                                <input type="checkbox" name="z_import_subscribe_connections" value="1">
                                                                {_ Also subscribe to connected resources. _}
                                                                <a href="#" class="z-btn-help do_dialog"
                                                                    title="{_ Subscribing to connected resources _}" aria-label="{_ Subscribing to connected resources _}"
                                                                    data-dialog="{{ %{
                                                                        title: _"Subscribing to connected resources",
                                                                        text: _"Follows the Connections option. Connected resources keep their subscriptions when disconnected from this page.",
                                                                        level: 10
                                                                    }|escape }}"></a>
                                                            </label>
                                                        </div>
                                                    </div>
                                                </div>
                                            {% endif %}
                                        {% endblock %}
                                    {% endif %}
                                </div>
                            </div>
                        {% endblock %}
                    {% endwith %}
                    {% else %}
                        <div class="media-import__options">
                            {% block import_update_options__rsc %}
                                {% if mi.medium and (
                                           mi.props.is_authoritative|is_undefined
                                        or mi.props.is_authoritative)
                                %}
                                    {% include "_edit_medium_language.tpl" %}
                                {% endif %}
                            {% endblock %}
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

            {% endwith %}
            </form>
        </div>
    {% endwith %}
    {% endfor %}

    {% javascript %}
        document.getElementById('media-import-wrapper').addEventListener('change', (event) => {
            const name = event.target.name;
            if (name !== 'is_authoritative' && name !== 'z_import_subscribe') return;
            const checkbox = event.target.form.querySelector('[name="z_import_subscribe"]');
            if (!checkbox) return;
            if (name === 'is_authoritative') {
                checkbox.disabled = event.target.value === '1' || checkbox.dataset.websubSupported !== '1';
                if (checkbox.disabled) checkbox.checked = false;
            }
            event.target.form.querySelector('.websub-sub-options').hidden = !checkbox.checked;
        });
        $('#media-import-wrapper').on('change', 'input[type=checkbox]', function() {
            var name = $(this).attr('name');
            if (name === 'z_import_subscribe') return;
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
