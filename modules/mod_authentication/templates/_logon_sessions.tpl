<ol class="logon-session-list">
    {% for sess in m.session.list_sessions %}
        {% with forloop.counter as ct %}
            <li class="logon-session" id="{{ #sess.ct }}">
                <p>
                    <span>{_ Started _}</span>
                    <b>{{ sess.auth_timestamp|date:_"Y-m-d H:i:s" }}</b>
                    &ndash; <span class="text-muted">{{ sess.auth_timestamp|timesince }}</span>
                    {% if sess.is_current %}
                        <span class="label label-primary">{_ Current _}</span>
                    {% endif %}

                    <br>

                    <span>{_ From _}</span>
                    <b>{{ sess.peer|default:"unknown" }}</b>

                    {% if sess.peer %}
                    <span class="text-muted">
                        / <a href="https://whatismyipaddress.com/ip/{{ sess.peer|urlencode }}" target="_blank">{_ Location Lookup _}</a>
                        / <a href="https://whois.domaintools.com/{{ sess.peer|urlencode }}" target="_blank">{_ Whois _}</a>
                    </span>
                    {% endif %}

                    <br>

                    <span class="text-muted">
                        <span>{_ Browser _}</span>
                        <b>{{ sess.user_agent|escape }}</b>
                    </span>
                </p>

                <p>
                    <button class="btn btn-primary" id="{{ #alert.ct }}">
                        {_ Show Alert _}
                    </button>
                    {% wire id=#alert.ct
                            postback={session_alert session_nr=sess.session_nr}
                            delegate=`mod_authentication`
                    %}

                    <button class="btn btn-danger" id="{{ #close.ct }}">
                        {_ Stop Session _}
                    </button>
                    {% wire id=#close.ct
                            action={confirm
                                title=_"Stop Session"
                                text=_"Are you sure you want to close this session? All unsaved work will be lost."
                                ok=_"Stop Session"
                                postback={session_stop session_nr=sess.session_nr element_id=#sess.ct}
                                delegate=`mod_authentication`
                            }
                    %}
                </p>
            </li>
        {% endwith %}
    {% endfor %}
</ol>

{% if unmask_id %}
    {% wire action={unmask target=unmask_id} %}
{% endif %}
