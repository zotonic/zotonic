{% if m.acl.use.mod_import_feed %}
    <h3>{_ Example entries _}</h3>
    <p class="import-feed-preview-loading" role="status" aria-live="polite">
        <img src="/lib/images/spinner.gif" width="16" height="16" alt="">
        {_ Loading example entries… _}
    </p>
    {% if preview %}
        <p>{_ Preview only: no resources have been imported. _}</p>
        {% for e in preview.entries %}
            <article class="panel panel-default">
                <div class="panel-heading">
                    <strong>{{ e.title|truncatechars:100|escape }}</strong>
                    <a href="{{ e.url|sanitize_url|escape }}" target="_blank" rel="noopener noreferrer" class="pull-right">{_ Original article _} <span class="fa fa-external-link"></span></a>
                </div>
                <div class="panel-body">
                    {% if e.zotonic_export %}
                        <p class="text-info">{_ Zotonic resource: uses the native resource importer. _}</p>
                    {% endif %}
                    {% if e.preview_error %}
                        <p class="alert alert-warning">{{ e.preview_error|escape }}</p>
                    {% endif %}
                    <table class="table table-sm">
                        <tbody>
                            <tr>
                                <th scope="row">{_ Identity _}</th>
                                <td>{{ e.uri|escape }}</td>
                            </tr>
                            <tr>
                                <th scope="row">{_ Author _}</th>
                                <td>{{ e.author|escape }}</td>
                            </tr>
                            <tr>
                                <th scope="row">{_ Published _}</th>
                                <td>{{ e.published|date:"Y-m-d H:i" }}</td>
                            </tr>
                            <tr>
                                <th scope="row">{_ Tags _}</th>
                                <td>{{ e.tags|join:", "|escape }}</td>
                            </tr>
                            <tr>
                                <th scope="row">{_ Media found _}</th>
                                <td>{{ e.media|length }}</td>
                            </tr>
                        </tbody>
                    </table>

                    {% if e.target_category %}
                        <p>
                            {_ Import category _}: {{ e.target_category.title }} ·
                            {_ Dependent media _}: {{ e.dependent_count }}
                            {% if e.attached_medium %}
                                · {_ Medium attached directly _}
                            {% endif %}
                        </p>
                    {% endif %}
                    {% if e.wordpress_error %}
                        <p class="alert alert-warning">{_ WordPress enrichment unavailable; the feed data will be used. _}</p>
                    {% endif %}
                    {{ e.body|sanitize_html }}
                </div>
            </article>
        {% empty %}
            <p>{_ The feed has no entries. _}</p>
        {% endfor %}
    {% else %}
        <p>{_ Use Test / preview to fetch example entries. _}</p>
    {% endif %}
{% endif %}
