{% wire action={update
        target="new-find-results-description"
        template="_action_dialog_new_rsc_tab_find_description.tpl"
        text=text
        predicate=predicate
        is_multi_cat=is_multi_cat
        category_id=category_id
    }
%}

<div id="dialog_new_rsc_results">
    {% with m.search.paged[
            {query page=1 pagelen=10
                    text=text
                    cat=cat
                    creator_id=creator_id
                    content_group=content_group
                    cat_exclude=cat_exclude
                    zsort="-modified"
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
                is_zlink=is_zlink
                creator_id=creator_id
                content_group=content_group
                is_result_render
				visible
			}
        %}
    {% endwith %}
</div>
