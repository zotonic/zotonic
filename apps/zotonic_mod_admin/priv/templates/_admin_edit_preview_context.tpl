{# Keep this inside the rscform block: site overrides set view_location in that scope. #}
{% wire name="admin-preview-resolve"
    postback={preview id=id view_location=view_location}
    delegate=`controller_admin_edit`
%}
{% javascript %}
cotonic.ready.then(function() {
    cotonic.broker.publish("admin/preview/context", {
        id: {{ id|default:"undefined" }},
        tree_id: {{ tree_id|default:"undefined" }}
    }, { retain: true });
});
{% endjavascript %}
