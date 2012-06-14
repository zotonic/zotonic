<form class="well form-search" method="GET" action="{% url search %}#content-pager">
    <input class="search-query" type="text" name="qs" value="{{ q.qs|escape }}" />
    <input class="btn btn-primary" type="submit" value="{_ Search _}" />
</form>
