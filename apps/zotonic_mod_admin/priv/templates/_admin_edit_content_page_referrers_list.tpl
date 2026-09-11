{% with m.search.paged[{referrers id=id page=1 pagelen=50}] as incoming %}
{% if incoming %}
    <p class="help-block">{_ Newest incoming connections. _}</p>

    <ul class="tree-list connections-list">
        {% for s_id, predicate_id in incoming %}
            {% with forloop.counter as index %}
            <li id="{{ #unlink_wrapper.index }}" class="menu-item">
                <div class="menu-wrapper">
                    <a id="{{ #edit.index }}" href="{% url admin_edit_rsc id=s_id %}" title="{_ Edit _}">
                        {% catinclude "_rsc_edge_item.tpl" s_id %}
                    </a>
                    <span class="text-muted">{{ predicate_id.title }}</span>
                    {% if m.acl.is_allowed.link[s_id] %}
                        <button type="button" id="{{ #unlink.index }}" title="{_ Disconnect _}" aria-label="{_ Disconnect _}" class="z-btn-remove"></button>
                        {% wire id=#unlink.index
                            action={unlink
                                subject_id=s_id
                                predicate=predicate_id.name
                                object_id=id
                                hide=#unlink_wrapper.index
                                undo_message_id=undo_message_id
                            }
                        %}
                    {% endif %}
                    {% wire id=#edit.index
                        action={dialog_edit_basics id=s_id update_element=#edit.index template="_rsc_edge_item.tpl" is_update}
                    %}
                </div>
            </li>
            {% endwith %}
        {% endfor %}
    </ul>

    {% if incoming.total > 50 %}
        <p class="help-block">
            <a href="{% url admin_edges qhasobject=id %}">
                {% trans "And {n} more." n=incoming.total-50 %}
            </a>
        </p>
    {% endif %}
{% else %}
    <p class="help-block">{_ There are no incoming connections. _}</p>
{% endif %}
{% endwith %}
