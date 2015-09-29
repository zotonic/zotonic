{% if comment.is_visible %}
   {% button id=#btnpublish
             class="btn btn-default btn-xs disabled"
             text=_"publish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
   {% button id=#btnunpublish
             class="btn btn-default btn-xs"
             text=_"unpublish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
{% else %}
   {% button id=#btnpublish
             class="btn btn-default btn-xs"
             text=_"publish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
   {% button id=#btnunpublish
             class="btn btn-default btn-xs disabled"
             text=_"unpublish"
             postback={comment_toggle id=id element=element  btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
{% endif %}
