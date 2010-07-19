<p>{_ Are you sure you want to delete this question? _}</p>

{% button text="Delete" delegate=delegate postback={survey_delete_question id=id question_id=question_id} action={dialog_close} %}
{% button text="Cancel" action={dialog_close} %}
