<div id="dialog_connect_results" class="connect-results">
    {% with m.search.paged[
            {query text=text cat=cat page=1 pagelen=(6*3)
                   creator_id=creator_id content_group=content_group
                   zsort="-created"
            }]
        as result
    %}
        <div id="dialog_connect_loop_results" class="thumbnails">
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
                is_result_render
				visible
			}
        %}
    {% endwith %}
</div>
