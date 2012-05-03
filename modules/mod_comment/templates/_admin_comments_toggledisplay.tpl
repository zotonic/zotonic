{% if comment.is_visible %}
   {% button id=#btnpublish
             class="btn btn-mini disabled"
             text=_"publish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
   {% button id=#btnunpublish
             class="btn btn-mini"
             text=_"unpublish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
{% else %}
   {% button id=#btnpublish
             class="btn btn-mini"
             text=_"publish"
             postback={comment_toggle id=id element=element btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
   {% button id=#btnunpublish
             class="btn btn-mini disabled"
             text=_"unpublish"
             postback={comment_toggle id=id element=element  btnpublish=#btnpublish btnunpublish=#btnunpublish} %}
{% endif %}