{# A Location has an a map interface and an address #}

<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
		<span class="title">{_ Geodata _}</span>
		<span class="arrow">{_ make smaller _}</span>
	</h3>
	
	<div class="item">
		<fieldset class="admin-form">
			<div class="notification notice">
				{_ Here you add the latitude and longitude data. Please click on the map to select the location. _} <a href="javascript:;" id="fill-geo">{_ Try to fill the fields automatically _}</a>
			</div>
			
			<div class="zp-15">
				<div class="form-item clearfix">
					<label for="location_lat">{_ Latitude _}</label>
					<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" style="width: 60%" />
				</div>
			</div>

			<div class="zp-15">
				<div class="form-item clearfix">
					<label for="location_lng">{_ Longitude _}</label>
					<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" style="width: 60%" />
				</div>
			</div>
		</fieldset>
		
		<div id="map_canvas" class="map-wrapper"></div>
		
	</div>
</div>

{% include "_admin_edit_content_address.tpl" id=id %}