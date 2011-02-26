<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\">
    <head>
        <title>zotonic</title>

        {% lib 
        "/css/zp-base.css"
        "/css/zp-type.css"
        "/css/zp-forms.css"
        "/css/zp-project.css"
        "/css/zp-growl.css" 
        "/css/zp-dialog.css" 
        %}

		<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
    </head>

    <body>
		{% wire type="unload" action={alert text="Bye"} %}
		<div class="zp-wrapper"> 
	        <h1>Hello, new world {{ helloworld_counter }}</h1>
		
			<div class="zp-50 content"> 
				<div class="padding"> 

					<h2>The menu should be below</h2>
					
					{% menu %}

					<h2>Postbacks which give some content</h2>
					<p>
						{% button text="Fill the content div" postback="fill_content"  %}
						{% button text="Insert Top" postback="insert_top"  %}
						{% button text="Insert Bottom" postback="insert_bottom" %}
					</p>
	
					<div class="clear"></div>

					<div id="content" style="border: 1px solid green; padding: 10px; width: 100px">
						This is the content div
					</div>
		
					<h2>Some drag/drop testing</h2>

					{% draggable id="drag1" tag="drag1" %}
					<div id="drag1" style="border: 2px solid #c33; background-color: #e99; padding: 10px; width: 100px; height: 50px">
						<p>
							Drag Me
						</p>
					</div>
		
					<div class="clear">&nbsp;</div>

					{% droppable id="drop1" tag="drop1" %}
					<div id="drop1" style="border: 2px solid #3c3; background-color: #9e9; padding: 10px; width: 100px; height: 50px">
						<p>
							Drop Here
						</p>
					</div>

					<h2>Sorter and sortables</h2>

					<div id="sort" class="padding">
						<div id="s1">
							<p>sortable 1</p>
						</div>
						<div id="s2">
							<p>sortable 2</p>
						</div>
						<div id="s3">
							<p>sortable 3</p>
						</div>
						<div id="s4">
							<p>sortable 4</p>
						</div>
						<div id="s5">
							<p>sortable 5</p>
						</div>
					</div>

					{% sorter id="sort" %}
					{% sortable id="s1" %}
					{% sortable id="s2" %}
					{% sortable id="s3" %}
					{% sortable id="s4" %}
					{% sortable id="s5" %}
					
					<h2>Some effects</h2>
					
					<div class="zp-20">
						{% button text="Hide" action={hide target="ef"} %}
						{% button text="Show" action={show target="ef"} %}<br/><br/>
						{% button text="Fade" action={fade_out target="ef"} %}
						{% button text="Appear" action={fade_in target="ef"} %}<br/><br/>
						{% button text="Toggle" action={toggle target="ef"} %}<br/><br/>
						
						<a id="tgl" href="#">wired toggle</a>
						{% wire id="tgl" action={toggle target="ef"} %}
					</div>

					<div class="zp-40">
						<div id="ef" style="border:1px solid black; background-color: #99c; padding: 10px">
							<p>Use the buttons on the left to test different effects on this div.</p>
						</div>
					</div>

				</div>
			</div>

			<div class="zp-50"> 
				<div class="padding"> 
					<h2>Some postbacks</h2>

					<h3>Javascript confirm</h3>
					<p>
						{% button text="Show Confirm" postback="show_confirm" %}<br/><br/>
					</p>
				
					<h3>Growl</h3>
					<p>
						{% button text="Show Temporary Growl" postback="show_growl"  %}<br/><br/>
						{% button text="Show Permanent Growl" postback="show_growl_stay" %}<br/><br/>
					</p>
					
					<hr/>
					
					<h2>Form validation</h2>
				
					{% wire id="myform" type="submit" postback="some_tag" action={toggle target="ef"} %}
					<form id="myform" method="POST" action="postback">
					{# You could also use: delegate="module_handling_form" #}

						<label for="email">e-mail</label>
						<input type="text" id="email" name="email" value="" />
						{% validate id="email" type={presence} type={email} %}
						
						<div class="clear"></div>
						<button type="submit">Submit</button>
						<div class="clear"></div>

					</form>

					<h2>Redirect to somewhere</h2>
					
					{% button text="Redirect to /" action={redirect location="/"} %}
				</div>
			</div>
		</div>
		
		<hr/>
		
		<div class="zp-wrapper">
			<div class="padding">
				<h2>Dynamic id with <tt>#id</tt></h2>

				{{ #id }}

				{% button id=#id %}
				<div class="clear"></div>

				<h2>Cached include</h2>

				<p>Include below is cached for 10 seconds</p>
				
				{% include "test_included.tpl" maxage=10 %}

				<h2>Some images</h2>

				<p>
				{% image "koe.jpg" width=200 height=200 crop="east" mono blur style="border: 1px solid black" %}
				{% image "koe.jpg" width=200 height=200 crop style="border: 1px solid black" %}
				{% image "koe.jpg" width=200 height=200 crop="west" blur="20x8" style="border: 1px solid black" %}
				</p>

				<div class="clear"></div>

				<p>
				{% image "koe.jpg" width=200 height=200 crop="east" grey style="border: 1px solid black" %}
				{% image "koe.jpg" width=200 height=200 crop style="border: 1px solid black" %}
				{% image "koe.jpg" width=200 height=200 crop="west" grey blur="20x8" style="border: 1px solid black" %}
				</p>

				<div class="clear"></div>

				<h3>And the url of an image</h3>

				{% image_url "koe.jpg" width=400 crop="east" blur grey %}

				<h2>Id test (cont)</h2>

				And the id of above again: {{ #id }}
				
				<h2>catinclude test</h2>
				
				<p><strong>A person</strong></p>
				{% catinclude "catinclude_test.tpl" 1 %}

				<p><strong>Not a person</strong></p>
				{% catinclude "catinclude_test.tpl" 2 %}

				<h2>all catinclude test</h2>
				
				{% all catinclude "catinclude_test.tpl" 1 %}
			</div>
		</div>

		{% include "_js_include.tpl" %}
		{% script %}

    </body>
</html>
