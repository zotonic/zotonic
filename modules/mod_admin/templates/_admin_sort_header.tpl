<a href="?qs={{ q.qs|urlencode }}&qcat={{ q.qcat|urlencode }}&qsort={% ifequal q.qsort field %}-{% endifequal %}{{ field }}{{ url_append }}">
    {{ caption }}
    {% ifequal q.qsort field %}
    +
    {% endifequal %}
    {% ifequal q.qsort "-"|append:field %}
    -
    {% endifequal %}
</a>
