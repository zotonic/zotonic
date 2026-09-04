<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006">
    <numFmts count="1">
        <numFmt numFmtId="164" formatCode="yyyy/m/d hh:mm:ss"/>
    </numFmts>
    <fonts count="2">
        <font>
            <sz val="10.0"/>
            <color rgb="FF000000"/>
            <name val="Arial"/>
        </font>
        <font/>
    </fonts>
    <fills count="{{ fill_count }}">
        <fill>
            <patternFill patternType="none"/>
        </fill>
        <fill>
            <patternFill patternType="lightGray"/>
        </fill>
        {% for color in fill_colors %}
        <fill>
            <patternFill patternType="solid">
                <fgColor rgb="{{ color }}"/>
                <bgColor indexed="64"/>
            </patternFill>
        </fill>
        {% endfor %}
    </fills>
    <borders count="1">
        <border>
            <left/><right/><top/><bottom/>
        </border>
    </borders>
    <cellStyleXfs count="1"><xf borderId="0" fillId="0" fontId="0" numFmtId="0" applyAlignment="1" applyFont="1"/>
    </cellStyleXfs>
    <cellXfs count="{{ cell_xf_count }}">
        <xf borderId="0" fillId="0" fontId="0" numFmtId="0" xfId="0" applyAlignment="1" applyFont="1">
           <alignment/>
        </xf>
        <xf borderId="0" fillId="0" fontId="1" numFmtId="0" xfId="0" applyAlignment="1" applyFont="1">
            <alignment/>
        </xf>
        <xf borderId="0" fillId="0" fontId="1" numFmtId="164" xfId="0" applyAlignment="1" applyFont="1" applyNumberFormat="1">
            <alignment/>
        </xf>
        {% for style in cell_styles %}
        <xf borderId="0" fillId="{{ style.fill_id }}" fontId="{{ style.font_id }}" numFmtId="{{ style.num_fmt_id }}" xfId="0" applyAlignment="1" applyFont="1" applyFill="1"{% if style.apply_number_format %} applyNumberFormat="1"{% endif %}>
            <alignment/>
        </xf>
        {% endfor %}
    </cellXfs>
    <cellStyles count="1">
        <cellStyle xfId="0" name="Normal" builtinId="0"/>
    </cellStyles>
    <dxfs count="0"/>
</styleSheet>
