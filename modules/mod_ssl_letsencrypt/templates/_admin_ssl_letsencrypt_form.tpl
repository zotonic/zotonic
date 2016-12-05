{% if not m.site.hostname|is_letsencrypt_valid_hostname %}
    <div class="alert alert-danger">
        <p>{_ Make sure your hostname is reachable and resolves to a routable address. _}</p>

        <p>{_ Check the following urls: _}</p>

        <ul>
            <li><a href="http://{{ m.site.hostname }}/">http://{{ m.site.hostname }}/</a></li>
            <li><a href="https://{{ m.site.hostname }}/">https://{{ m.site.hostname }}/</a></li>
        </ul>
    </div>
{% else %}

    <div id="{{ #form_wrapper }}" class="well">
        <p>{_ The checked hostnames will be added to the certificate. _}</p>

        {% wire id=#letsencrypt_hosts type="submit" 
            postback={request_cert hostname=m.site.hostname wrapper=#form_wrapper}
            delegate=`mod_ssl_letsencrypt`
        %}
        <form id="{{ #letsencrypt_hosts }}" method="POST" action="postback">
            <ul class="list-unstyled">
                <li>
                    <label>
                        <input type="checkbox" checked disabled /> {{ m.site.hostname }}
                    </label>
                    <span class="text-success">√ – primary</span>
                </li>
            </ul>

            {% if m.site.hostalias %}
                <div class="form-group">
                    <ul class="list-unstyled">
                    {% for hostalias in m.site.hostalias %}
                        {% if hostalias|is_letsencrypt_valid_hostname %}
                            <li>
                                <label>
                                    <input type="checkbox" name="san" value="{{ hostalias }}" checked /> {{ hostalias }}
                                </label>
                                <span class="text-success">√ – alias</span>
                            </li>
                        {% else %}
                            <li>
                                <label class="text-muted">
                                    <input type="checkbox" disabled /> {{ hostalias }}
                                </label>
                                <em class="text-danger">&times; – {_ not reachable or local IP _}</em>
                            </li>
                        {% endif %}
                    {% endfor %}
                    </ul>
                </div>
            {% endif %}
            <div class="form-group">
                <button class="btn btn-primary" type="submit">{_ Request Certificate _}</button>
            </div>
        </form>
    </div>
{% endif %}
