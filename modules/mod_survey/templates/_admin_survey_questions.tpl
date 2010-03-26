<li id="ci2">
<p class="textBlock">This is a paragraph-length textblock, useful for instructions. If you need more than one paragraph, simply select a textBlock for each paragraph.<p>
</li>
{% draggable id="ci2" tag={q type="textblock"} to_sorter="survey" %}

<li id="ci3">
<h2>This is a subhead</h2>
</li>
{% draggable id="ci3" tag={q type="subhead"} to_sorter="survey" %}

<li id="ci4">
<p class="prompt">This is a prompt.</p>
</li>
{% draggable id="ci4" tag={q type="prompt"} to_sorter="survey" %}

<li id="ci5">
<p class="question">Type: Yes/No - Do you like split pea soup?<p>
<p class="binary">
<input type="radio" name="questionX" value="yes">Yes
<input type="radio" name="questionX" value="no">No
<p>
</li>
{% draggable id="ci5" tag={q type="yesno"} to_sorter="survey" %}

<li id="ci6">
<p class="question">Type: True/False - The earth is flat.<p>
<p class="binary">
<input type="radio" name="questionY" value="true">True
<input type="radio" name="questionY" value="false">False
<p>
</li>
{% draggable id="ci6" tag={q type="truefalse"} to_sorter="survey" %}

<li id="ci7">
<p class="question">Type: Likert scale - Weasels make great pets.<p>
<p class="likert">
<p>Strongly Disagree
<input type="radio" name="likert" value=1> 1 
<input type="radio" name="likert" value=2> 2
<input type="radio" name="likert" value=3> 3 
<input type="radio" name="likert" value=4> 4 
<input type="radio" name="likert" value=5> 5
Strongly Agree<p>
<p>
</li>
{% draggable id="ci7" tag={q type="likert"} to_sorter="survey" %}


<li id="ci8">
<p class="question">Type: Thurstone scale.<p>
<p class="thurstone">
<input type="radio" name="thurstone" value=1> This editor is very intuitive.<br />
<input type="radio" name="thurstone" value=2> This editor is fairly easy to use.<br />
<input type="radio" name="thurstone" value=3> This editor is gets the job done.<br />
<input type="radio" name="thurstone" value=4> This editor is not that easy to use.<br />
<input type="radio" name="thurstone" value=5> This editor is very confusing.<br />
<p>
</li>
{% draggable id="ci8" tag={q type="thurstone"} to_sorter="survey" %}


<li id="ci9">
<p class="question">Type: short-answer open-ended question - Please enter your name.<p>
<p class="shortAnswer">
<input name="name" SIZE=12>
<p>
</li>
{% draggable id="ci9" tag={q type="shortanswer"} to_sorter="survey" %}


<li id="ci10">
<p class="question">Type: long answer open-ended question - Please write an essay.<p>
<p class="longAnswer">
<textarea name="longAnswer">
</textarea>
<p>
</li>
{% draggable id="ci10" tag={q type="longanswer"} to_sorter="survey" %}


<li id="ci11">
<p class="narrative">Type: narrative sequence - 
I am <input name="age" size=3> years old. I like <select name="flavor"><option>Vanilla <option>Strawberry <option>Chocolate <option>Other </select> ice cream and my favorite color is <input name="color" size=12>.
</p>
</li>
{% draggable id="ci11" tag={q type="narrative"} to_sorter="survey" %}

