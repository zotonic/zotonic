{% for blk in m.rsc[id].blocks %}
    {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join blk=blk id=id %}
{% endfor %}
