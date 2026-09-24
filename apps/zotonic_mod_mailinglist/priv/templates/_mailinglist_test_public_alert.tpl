{% with m.rsc.mailinglist_test.id as test_list_id %}
    {% if test_list_id and m.acl.anonymous.view[test_list_id] %}
        <div class="alert alert-danger" role="alert">
            <p><strong>{_ The test mailing list is publicly visible. _}</strong></p>
            <p>{_ Visitors who are not logged in can view this list. Test mailings are intended for internal review. Restrict the list’s visibility to internal users. _}</p>
            {% if m.rsc[test_list_id].is_editable %}
                <a class="alert-link" href="{% url admin_edit_rsc id=test_list_id %}">{_ Review the test mailing list’s access settings _}</a>
            {% else %}
                <p>{_ Ask an administrator to change the test mailing list’s access settings. _}</p>
            {% endif %}
        </div>
    {% endif %}
{% endwith %}
