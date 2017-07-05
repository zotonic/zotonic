<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:mx="http://schemas.microsoft.com/office/mac/excel/2008/main" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac" xmlns:xm="http://schemas.microsoft.com/office/excel/2006/main">
<sheetData>
<row r="1">
{% for key in keys %}{% with forloop.counter as colnr %}{{ encode_cell[[1, forloop.counter, lookup_header[key]]] }}{% endwith %}{% endfor %}
</row>
{% for row in rows %}{% with forloop.counter+1 as rownr %}
<row r="{{ rownr }}">
{% if row|is_list %}{% for v in row %}{{ encode_cell[[rownr, forloop.counter, v]] }}{% endfor %}
{% else %}{% for key in keys %}{{ encode_cell[[rownr, forloop.counter, lookup_value[[row,key]]]] }}{% endfor %}
{% endif %}
</row>
{% endwith %}{% endfor %}
</sheetData>
</worksheet>
