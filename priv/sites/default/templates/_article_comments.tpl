<h2>Comments</h2>

<ul class="comments-list">
	<li>
		<h3>Arjan Scherpenisse</h3>
		<p class="comment-meta">Posted 3 days, 15 hours ago.</p>
		<p class="comment-body">Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
	</li>
</ul>

{% wire id="comments-form" type="submit" postback="postcomment" delegate="resource_default_postcomment" %}
<form id="comments-form" method="post" action="postback">
	<fieldset>
		<div class="zp-30">
			<div class="form-item">
				<label for="name">Name</label>
				<input type="text" name="name" id="name" />
				{% validate id="name" type={presence} %}
			</div>
			<div class="form-item">
				<label for="mail">E-mail</label>
				<input type="text" name="mail" id="mail" />
				{% validate id="mail" type={presence} type={email} %}
			</div>
		</div>
		<div class="zp-70 last">
			<div class="form-item">
				<label for="message">Message</label>
				<textarea name="message" id="message" cols="60" rows="8"></textarea>
				{% validate id="message" type={presence} %}
			</div>
			<div class="form-item button-wrapper">
				<button type="submit">Send</button>
			</div>
		</div>
	</fieldset>
</form>