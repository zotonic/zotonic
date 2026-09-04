{# Mailing lists are recipients for mailings, not mailing content. #}
{% if not id.is_a.mailinglist %}
    {% include "_admin_edit_sidebar_mailinglist.tpl" id=id %}
{% endif %}
