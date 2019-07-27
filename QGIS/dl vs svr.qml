<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.4.4-Madeira" styleCategories="AllStyleCategories" hasScaleBasedVisibilityFlag="0" maxScale="0" minScale="1e+08">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <rasterrenderer classificationMax="0.794" classificationMin="-0.756" alphaBand="-1" opacity="1" band="1" type="singlebandpseudocolor">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader clip="0" colorRampType="INTERPOLATED" classificationMode="1">
          <colorramp name="[source]" type="gradient">
            <prop v="255,0,4,255" k="color1"/>
            <prop v="0,189,72,255" k="color2"/>
            <prop v="0" k="discrete"/>
            <prop v="gradient" k="rampType"/>
            <prop v="0.497596;253,142,60,255:0.498798;253,142,60,255:0.498798;253,142,60,255:0.498798;247,253,0,255:0.5;253,141,60,255" k="stops"/>
          </colorramp>
          <item alpha="255" label="-0.756 (min)" color="#ff0004" value="-0.756"/>
          <item alpha="255" label="-0.5" color="#fd8e3c" value="-0.5"/>
          <item alpha="255" label="-0.05" color="#fdd700" value="-0.05"/>
          <item alpha="255" label="0" color="#f7fd00" value="0"/>
          <item alpha="255" label="0.0169" color="#bafd00" value="0.05"/>
          <item alpha="255" label="0.5" color="#45fd3b" value="0.5"/>
          <item alpha="255" label="0.794 (max)" color="#00bd48" value="0.794"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeRed="255" colorizeStrength="100" saturation="0" colorizeGreen="128" colorizeBlue="128" colorizeOn="0"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
