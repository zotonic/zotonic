{% if id.survey_test_percentage %}
    {% if id|survey_test_max_points as max_points %}
        {% with (result.points / max_points * 100)|round as percentage %}
            {% with result.points >= max_points * (id.survey_test_percentage / 100) as is_passed %}
                <section class="survey-result-test-score {% if is_passed %}is-passed{% else %}is-failed{% endif %}"
                         aria-label="{_ Test result _}">
                    <div class="survey-result-test-score__outcome">
                        <span class="survey-result-test-score__icon" aria-hidden="true">
                            {% if is_passed %}&#10003;{% else %}&#10005;{% endif %}
                        </span>
                        <div>
                            <span class="survey-result-test-score__label">{_ Test result _}</span>
                            <strong class="survey-result-test-score__status">
                                {% if is_passed %}{_ Passed _}{% else %}{_ Failed _}{% endif %}
                            </strong>
                        </div>
                    </div>

                    <div class="survey-result-test-score__score">
                        <strong>{{ percentage }}%</strong>
                        <span>{% trans "{points} of {max} points" points=result.points max=max_points %}</span>
                    </div>

                    <div class="survey-result-test-score__threshold">
                        {% trans "Passing score: {percentage}%" percentage=id.survey_test_percentage %}
                    </div>
                </section>
            {% endwith %}
        {% endwith %}
    {% endif %}
{% endif %}
