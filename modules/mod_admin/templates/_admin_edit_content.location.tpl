{# A Location has an a map intercace and an address #}

<div class="item-wrapper">
	<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
		<span class="title">Geodata</span>
		<span class="arrow">make smaller</span>
	</h3>
	
	<div class="item">
		<fieldset class="admin-form">
			<div class="notification notice">
				Here you add the latitude and longitude data. Please click on the map to select the loacation. <a href="javascript:;" id="fill-geo">Try to fill the fields automatically</a>
			</div>
			
			<div class="zp-15">
				<div class="form-item clearfix">
					<label for="location_lat">latitude</label>
					<input id="location_lat" type="text" name="location_lat" value="{{ m.rsc[id].location_lat }}" style="width: 60%" />
				</div>
			</div>

			<div class="zp-15">
				<div class="form-item clearfix">
					<label for="location_lng">Longitude</label>
					<input id="location_lng" type="text" name="location_lng" value="{{ m.rsc[id].location_lng }}" style="width: 60%" />
				</div>
			</div>
		</fieldset>
		
		<div id="map_canvas" class="map-wrapper"></div>
		
	</div>
</div>

{% include "_admin_edit_content_address.tpl" id=id %}