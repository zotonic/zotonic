<div id="dialog_new_rsc_results">
    {% with m.search.paged[
            {query text=text cat=cat page=1 pagelen=10
                   creator_id=creator_id content_group=content_group
                   zsort="-created"
            }]
        as result
    %}
        <div id="dialog_new_rsc_loop_results" class="items">
            {% include "_action_dialog_new_rsc_tab_find_results_loop.tpl"
                id
                result=result
            %}
        </div>
	    {% lazy
	        action={
	            moreresults
	            result=result
	            target="dialog_new_rsc_loop_results"
				template="_action_dialog_new_rsc_tab_find_results_loop.tpl"
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
