{#
    Page table sort header.
    
    Params:
    field: database column
    caption: display text
    type: used to distinguish type "date"
    qsort: the sort id passed in url param qsort
#}
{% with
    "<span class='arrow up'></span>",
    "<span class='arrow down'></span>"
    as
    up_arrow,
    down_arrow
%}
{% with
    (qsort == field)|if
        : 1
        : ((qsort == "-" ++ field)|if
            : (-1)
            : 0)
    as
    sort_status
%}
    {% with
        (sort_status == 0)|if
            : ((type == "date")|if
                : (-1)
                : 1)
            : (-sort_status)
        as
        next_modifier
    %}
        {% with
            (sort_status == 0)|if
                : ""
                : ((sort_status == 1)|if
                    : down_arrow
                    : up_arrow)
            as
            status_modifier_char
        %}
            {% with
                (next_modifier == 0)|if
                    : ""
                    : ((next_modifier == 1)|if
                        : ""
                        : "-")
                as
                next_modifier_param_char
            %}
                <a href="?qs={{ q.qs }}&qcat={{ q.qcat }}&qsort={{next_modifier_param_char}}{{ field }}{{ url_append }}">{{ caption }}{{ status_modifier_char }}</a>
            {% endwith %}
        {% endwith %}
    {% endwith %}
{% endwith %}
{% endwith %}