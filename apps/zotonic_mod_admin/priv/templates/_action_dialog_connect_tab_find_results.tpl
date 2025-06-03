<div id="dialog_connect_results" class="connect-results">
    {% if intent == 'connect' %}
        <p class="text-muted">
            {% trans "Showing pages you can connect to using <b>{predicate}</b>."
                     predicate=m.rsc[predicate].title
            %}
        </p>
    {% endif %}

    {% with m.search.paged[
            {query text=text cat=cat page=1 pagelen=(6*3)
                   creator_id=creator_id content_group=content_group
                   zsort="-created"
            }]
        as result
    %}
        <div id="dialog_connect_loop_results" class="thumbnails">
            {% if text|trim as qs %}
                {% with m.rsc[qs].id as rid %}
                {% with m.rsc['-'].lookup.page_path[qs] as path %}
                    {% if rid.is_visible or path.id.is_visible %}
                        <h4>{_ Matching id, name, page path or redirection _}</h4>
                        <div class="row">
                            {% for id in [ rid ]
                                        ++ (rid =/= path.id)|if:[path.id]:[]
                            %}
                                {% if id.is_visible %}
                                    {% catinclude "_action_dialog_connect_tab_find_results_item.tpl" id
                                        predicate=predicate
                                        subject_id=subject_id
                                        object_id=object_id
                                    %}
                                {% endif %}
                            {% endfor %}
                        </div>
                        <hr>
                        <h4>{_ Search results _}</h4>
                    {% endif %}
                {% endwith %}
                {% endwith %}
            {% endif %}

            {% include "_action_dialog_connect_tab_find_results_loop.tpl"
                id
                result=result
            %}
        </div>
	    {% lazy
	        action={
	            moreresults
	            result=result
	            target="dialog_connect_loop_results"
				template="_action_dialog_connect_tab_find_results_loop.tpl"
                predicate=predicate|as_atom
                subject_id=subject_id
                object_id=object_id
                creator_id=creator_id
                content_group=content_group
                intent=intent
                is_result_render
				visible
			}
        %}
    {% endwith %}
</div>
