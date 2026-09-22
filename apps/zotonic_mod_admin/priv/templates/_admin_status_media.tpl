{% with m.admin_status.media_processing as media %}
{% if media %}
<div class="widget" id="media-processing-status">
    <div class="widget-header">
        <h3>{_ Media processing _}</h3>
    </div>
    <div class="widget-content">
        {% if media.configuration_error %}
            <p class="alert alert-danger" role="alert">{_ The media runner configuration is invalid. _}</p>
        {% elseif media.remote %}
            <p>{_ Media commands use remote runners. _}
                {% if media.local_fallback %}
                    {_ Local fallback is enabled. _}
                {% else %}
                    {_ Local fallback is disabled. _}
                {% endif %}
            </p>
        {% else %}
            <p>{_ Media commands run locally. _}</p>
        {% endif %}

        <h4>{_ Local sandbox _}</h4>
        {% if media.sandbox == `available` %}
            <p class="text-success">{_ Sandbox isolation is available. _}</p>
        {% elseif media.sandbox == `unsupported` %}
            <p class="alert alert-warning" role="alert">{_ Sandbox isolation is not supported on this system. Local media commands run without isolation. _}</p>
        {% else %}
            <p class="alert alert-danger" role="alert">{_ Sandbox isolation is unavailable. Local sandboxed media commands cannot run. _}</p>
        {% endif %}
        <p>{_ Local ImageMagick _}:
            {% if media.local_imagemagick.available %}
                {{ media.local_imagemagick.version|escape }}
            {% else %}
                {_ Unavailable _}
            {% endif %}
        </p>

        {% if media.runners %}
            <h4>{_ Media runners _}</h4>
            <div class="table-responsive">
                <table class="table table-striped">
                    <thead>
                        <tr><th>{_ Endpoint _}</th><th>{_ Connection _}</th><th>ImageMagick</th></tr>
                    </thead>
                    <tbody>
                        {% for runner in media.runners %}
                            <tr>
                                <td>{{ runner.url|escape }}</td>
                                <td>{% if runner.reachable %}<span class="text-success">{_ Available _}</span>{% else %}<span class="text-danger">{_ Unavailable _}</span>{% endif %}</td>
                                <td>{% if runner.imagemagick.available %}{{ runner.imagemagick.version|escape }}{% else %}{_ Unavailable _}{% endif %}</td>
                            </tr>
                        {% endfor %}
                    </tbody>
                </table>
            </div>
            <p class="help-block">{_ Runner availability and ImageMagick versions are cached for up to one minute. Failed probes are retried after five seconds. Refresh this page to check again. _}</p>
            <p class="help-block">{_ Remote sandbox isolation and queue details are available on each runner’s dashboard. _}</p>
        {% endif %}
    </div>
</div>
{% endif %}
{% endwith %}
