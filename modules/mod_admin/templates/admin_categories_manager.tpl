{% extends "admin_base.tpl" %}

{% block title %} Categories Manager {% endblock %}
{% block js_extra %}
	<script type="text/javascript">
		$(function()
		{
			$('.finder-column').sortable(
			{
				handle: '.finder-folder', 
				axis: 'y',
				containment: 'parent',
				forcePlaceholderSize: true,
				placeholder: 'finder-placeholder',
				start: function(event, ui)
				{
					$('.finder-columns').addClass('finder-sorting');
				},
				stop: function(event, ui)
				{
					$('.finder-columns').removeClass('finder-sorting');
				}
			})
			.resizable(
			{
				handles: 'e',
				minWidth: 200,
				maxWidth: 400,
				start: function(event, ui) {
					$('.finder-columns').addClass('finder-resizing');
				},
				stop: function(event, ui) {
					$('.finder-columns').removeClass('finder-resizing');
				}			
			});
		});
	</script>
{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Categories Manager</h2>
		
		<div class="finder-wrapper clearfix">
			<ul class="finder-columns">
				<li>
					<ul class="finder-column" id="finder-column-1">
						<li class="first">
							<h3>Base categories</h3>
							<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Testen'}}">Testen</span>
							<span class="finder-arrow"></span>
						</li>
						<li class="finder-item-selected-alt">
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Asseccoires'}}">Asseccoires</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Onderdelen'}}">Onderdelen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Tassen / kleding'}}">Tassen / kleding</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Brillen'}}">Brillen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Fietsen'}}">Fietsen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Asseccoires'}}">Asseccoires</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Onderdelen'}}">Onderdelen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Tassen / kleding'}}">Tassen / kleding</span>
							<span class="finder-arrow"></span>
						</li>
					</ul>
				</li>
				<li>
					<ul class="finder-column" id="finder-column-2">
						<li class="first">
							<h3>Asseccoires</h3>
							<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Helmen'}}">Helmen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Brillen'}}">Brillen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Helmen'}}">Helmen</span>
							<span class="finder-arrow"></span>
						</li>
						<li>
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'Brillen'}}">Brillen</span>
							<span class="finder-arrow"></span>
						</li>
						<li class="finder-item-selected">
							<span class="finder-folder"></span>
							<span class="finder-title do_editinplace {type: 'input', event: 'dblclick', data: {title: 'lorem'}}">lorem</span>
							<span class="finder-arrow"></span>
						</li>
					</ul>
				</li>
				<li>
					<div class="finder-info" id="finder-column-3">
						<input type="text" value="Lorem" />
						<textarea>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</textarea>
					</div>
				</li>
			</ul>
		</div>
	</div>
{% endblock %}