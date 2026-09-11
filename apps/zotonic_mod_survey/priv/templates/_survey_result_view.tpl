{% with m.survey.get_result[id][answer_id] as result %}
    {% if result %}
        {% if id.is_editable %}
            {% wire id=#form
                    type="submit"
                    postback={survey_result_view_save
                        id=id
                        answer_id=answer_id
                        on_success=on_success
                    }
                    delegate=`mod_survey`
            %}
            <form id="{{ #form }}" method="POST" action="postback" class="form form-survey survey-result-view">
                <section class="survey-result-status">
                    {% include "_survey_answer_status_fields.tpl"
                        result=result
                        status_labels=status_labels
                    %}
                </section>

                {% include "_survey_result_test_score.tpl" id=id result=result %}

                <fieldset class="survey-result-answers">
                    {% for blk in id.blocks %}
                        {% if question_name and blk.name != question_name %}
                            {# A caller can limit a result dialog to one question block. #}
                        {% elseif blk.is_editor_only %}
                            <div class="survey-editor-only-answer">
                                <span class="survey-editor-only-label">{_ Editor only _}</span>
                                {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
                                    id=id
                                    blk=blk
                                    answers=result.answers_edit
                                    answer_user_id=result.user_id
                                    editing
                                    nr=forloop.counter
                                %}
                            </div>
                        {% elseif blk.type == 'header' or blk.type == 'text' %}
                            {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
                                id=id
                                blk=blk
                                answer_user_id=result.user_id
                                editing
                                nr=forloop.counter
                                is_survey_answer_view
                            %}
                        {% elseif blk.name|member:result.answered_blocks %}
                            {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
                                id=id
                                blk=blk
                                result=result
                                answer_user_id=result.user_id
                                editing
                                nr=forloop.counter
                                is_survey_answer_view
                            %}
                        {% endif %}
                    {% endfor %}
                </fieldset>

                <div class="modal-footer">
                    {% optional include extra_actions_template
                        id=id
                        answer_id=answer_id
                        result=result
                    %}
                    {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
                    {% button class="btn btn-default"
                        type="button"
                        text=_"Edit all answers"
                        action={dialog_open
                            level=0
                            width="large"
                            backdrop="static"
                            title=_"Edit survey result"
                            template=edit_dialog_template|default:"_dialog_survey_editor.tpl"
                            id=id
                            answer_id=answer_id
                            user_id=result.user_id
                            action=action
                            on_submit=action
                        }
                    %}
                    {% button class="btn btn-primary" type="submit" text=_"Save" %}
                    {% if id.is_a.survey %}
                        <button class="btn btn-primary" type="submit" name="submit-email">{_ Save &amp; Email _}</button>
                    {% endif %}
                </div>
            </form>
        {% else %}
            <div class="survey-result-view">
                {% include "_survey_result_test_score.tpl" id=id result=result %}

                <fieldset class="survey-result-answers">
                    {% for blk in id.blocks %}
                        {% if (not question_name or blk.name == question_name) and (blk.type == 'header' or blk.type == 'text' or blk.name|member:result.answered_blocks) %}
                            {% optional include ["blocks/_block_view_",blk.type,".tpl"]|join
                                id=id
                                blk=blk
                                result=result
                                answer_user_id=result.user_id
                                nr=forloop.counter
                                is_survey_answer_view
                            %}
                        {% endif %}
                    {% endfor %}
                </fieldset>
                <div class="modal-footer">
                    {% optional include extra_actions_template
                        id=id
                        answer_id=answer_id
                        result=result
                    %}
                    {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
                </div>
            </div>
        {% endif %}
    {% else %}
        <div class="alert alert-danger">{_ This survey answer could not be found or you are not allowed to view it. _}</div>
    {% endif %}
{% endwith %}
