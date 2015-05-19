{% include "_admin_acl_rule_header.tpl" %}

{% for rule in m.acl_rule[kind].edit[{all group=q.g}] %}
    {% include "_admin_acl_rule_row.tpl" rule=rule %}
{% empty %}
    {_ No ACL rules _}
{% endfor %}

{% javascript %}
    var elements = $("select[name=acl_user_group_id]");
    for (var i=0; i<elements.length-1; i++) {
        if ($(elements[i]).val() != $(elements[i+1]).val()) {
        var target = $(elements[i+1]).parents("form:first");
        target.before($("div.row.acl-header:first").clone().addClass("sep"));
        }
        }
{% endjavascript %}
