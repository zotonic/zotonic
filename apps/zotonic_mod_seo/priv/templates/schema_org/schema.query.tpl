{% with m.search.paged[{query query_id=id page=q.page pagelen=100}] as result %}
    {% include "schema_org/types/item_list.tpl" items=result %}
{% endwith %}
