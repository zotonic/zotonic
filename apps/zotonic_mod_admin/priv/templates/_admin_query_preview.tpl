{% if not is_empty %}
    <p class="text-info">
        <span class="glyphicon glyphicon-info-sign"></span>
        {_ Query format: _}
        <strong>{{ query_type_label|escape }}</strong>
        {% if query_type %}<code>{{ query_type|escape }}</code>{% endif %}
    </p>
{% endif %}

{% if error %}
    <p class="text-danger">
        <span class="glyphicon glyphicon-alert"></span>
        <strong>{_ There is an error in the query. _}</strong><br>
        {% if error_line and error_column %}
            {% trans "Line {line}, column {column}:" line=error_line column=error_column %}
        {% elseif error_line %}
            {% trans "Line {line}:" line=error_line %}
        {% endif %}
        {{ reason|escape }}
    </p>
{% elseif is_empty %}
    <p class="text-info">
        <span class="glyphicon glyphicon-info-sign"></span>
        {_ The query text is empty. _}
    </p>
{% else %}
    <h4>{_ Query results _}</h4>

    <div class="row">
        <div class="col-lg-{% if show_parsed %}6{% else %}12{% endif %}">
            <p>
                {% if result.is_total_estimated %}{% trans "About {n} items found." n=result.total|round_significant:2 %}
                {% else %}{% trans "{n} items found." n=result.total %}
                {% endif %}
            </p>

            <ul class="tree-list connections-list">
            {% for id in result %}
                <li class="menu-item">
                    {% with forloop.counter as index %}
                    <div class="menu-wrapper">
                        <a id="{{ #qres.index }}" href="{% url admin_edit_rsc id=id %}" title="{_ Edit _}">
                            {% catinclude "_rsc_edge_item.tpl" id %}
                        </a>
                        {% wire id=#qres.index
                                action={dialog_edit_basics
                                    id=id
                                    update_element=#qres.index
                                    template="_rsc_edge_item.tpl"
                                    is_update
                                }
                        %}
                    </div>
                    {% endwith %}
                </li>
            {% empty %}
                <li><span class="text-muted">{_ No results. _}</span></li>
            {% endfor %}
            </ul>
        </div>
        {% if show_parsed %}
        <div class="col-lg-6">
            <details>
                <summary>{_ Show query as JSON _}</summary>
                <pre><code id="{{ #json }}">{{ parsed.q|to_json|escape }}</code></pre>
            </details>

            {% javascript %}
            {
                const json = $('#{{ #json }}').text();
                if (json) {
                    const elt = $('#{{ #json }}')
                        .text(JSON.stringify(JSON.parse(json), null, 2))
                        .addClass("language-json")
                        .get(0);
                    Prism?.highlightElement(elt);
                }
            }
            {% endjavascript %}
        </div>
        {% endif %}
    </div>
{% endif %}

{% javascript %}
{
    const typeInput = document.getElementById('{{ query_type_id }}');
    const liveGroup = document.getElementById('{{ live_group_id }}');
    const liveInput = document.getElementById('{{ live_input_id }}');
    const isLive = {% if is_live %}true{% else %}false{% endif %};

    if (typeInput) {
        typeInput.value = '{{ query_type|escapejs }}';
    }
    if (liveGroup) {
        liveGroup.classList.toggle('hidden', !isLive);
    }
    if (liveInput) {
        if (!isLive) {
            liveInput.checked = false;
        }
    }
}
{% endjavascript %}
