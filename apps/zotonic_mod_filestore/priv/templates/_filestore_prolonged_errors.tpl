{% if m.acl.is_admin %}
    {% with m.filestore.prolonged_errors[q.errors_page|default:1] as errors %}
        {% if errors.entries or errors.previous %}
            <div class="widget" id="filestore-errors">
                <h3 class="widget-header">{_ Read errors older than 24 hours _}</h3>
                <div class="widget-content">
                    <p>{_ Unresolved files are listed oldest first. The error shown is the most recent recorded error for each file. _}</p>
                    <div class="table-responsive">
                        <table class="table table-striped">
                            <thead>
                                <tr>
                                    <th scope="col">{_ File _}</th>
                                    <th scope="col">{_ Service _}</th>
                                    <th scope="col">{_ Last error _}</th>
                                    <th scope="col">{_ Consecutive failures _}</th>
                                    <th scope="col">{_ Failing since _}</th>
                                    <th scope="col">{_ Retry status _}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {% for entry in errors.entries %}
                                    <tr>
                                        <td class="text-break">{{ entry.path|escape }}</td>
                                        <td>{{ entry.service|escape }}</td>
                                        <td><code>{{ entry.error|escape }}</code></td>
                                        <td>{{ entry.error_count|escape }}</td>
                                        <td>{{ entry.error_since|date:"Y-m-d H:i T" }}</td>
                                        <td>{% if entry.is_unrecoverable %}
                                                <span class="label label-default">{_ Unrecoverable _}</span>
                                            {% else %}
                                                {% if entry.retry_after %}{{ entry.retry_after|date:"Y-m-d H:i T" }}{% else %}{_ Now _}{% endif %}
                                                {% if m.acl.user == 1 %}
                                                    {% button class="btn btn-danger btn-xs"
                                                        text=_"Mark unrecoverable"
                                                        action={confirm
                                                            text=_"Stop retrying this file and mark it unrecoverable? Its record and error details will be kept. No files will be deleted."
                                                            ok=_"Mark unrecoverable"
                                                            is_danger
                                                            postback={filestore_unrecoverable id=entry.id}
                                                            delegate=`filestore_admin`
                                                        }
                                                    %}
                                                {% endif %}
                                            {% endif %}</td>
                                    </tr>
                                {% empty %}
                                    <tr><td colspan="6">{_ No errors on this page. _}</td></tr>
                                {% endfor %}
                            </tbody>
                        </table>
                    </div>
                    <p>{_ Retries happen when a file is requested, not at a scheduled time. _}</p>
                    <nav aria-label="{_ Error list pages _}">
                        {% if errors.previous %}<a class="btn btn-default" href="{% url admin_filestore errors_page=errors.previous %}#filestore-errors">{_ Previous _}</a>{% endif %}
                        {% if errors.next %}<a class="btn btn-default" href="{% url admin_filestore errors_page=errors.next %}#filestore-errors">{_ Next _}</a>{% endif %}
                    </nav>
                </div>
            </div>
        {% endif %}
    {% endwith %}
{% endif %}
