{% wire id="comments-form" type="submit" postback={newcomment id=id} delegate="mod_comment" %}
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