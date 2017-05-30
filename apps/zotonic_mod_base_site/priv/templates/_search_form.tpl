<form class="well form-inline" method="GET" action="{% url search %}#content-pager">
    <input type="hidden" name="qcat" value="{{ q.qcat|escape }}" />
    <input class="search-query form-control" type="text" name="qs" value="{{ q.qs|escape }}" />
    <input class="btn btn-primary form-control" type="submit" value="{_ Search _} {{ m.rsc[q.qcat].title }}" />
</form>
