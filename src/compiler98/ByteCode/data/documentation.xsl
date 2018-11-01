<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

	<xsl:output method="xml" indent="no"
		doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
		doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
		encoding="iso-8859-1" />
	
	<!--
PAGE LEVEL METHODS
	-->
	<xsl:template match="bytecodes">
		<html xmlns="http://www.w3.org/1999/xhtml">
			<head>
				<title>Yhc - Bytecode Documentation</title>
				<style type="text/css">
				h3 {background-color:#bbd;font-size:12pt;padding:2px;}
				.type {font-family:monospace;font-size:10pt;margin-bottom:6px;}
				pre {border:2px solid gray;background-color:#eee;margin-left:20px;margin-right:20px;padding:2px;margin-top:8px;margin-bottom:8px;}
				</style>
			</head>
			<body>
				<h1>Yhc - Bytecode Documentation</h1>
				
				<xsl:variable name="elems" select="bytecode[not(@primitive) and not(@depreciated)]" />
				
				<h2>Index</h2>
				<ul>
					<xsl:for-each select="$elems">
						<xsl:sort select="@name" data-type="text" order="ascending" />
						<li>
							<a href="#{@name}">
								<xsl:value-of select="@name" />
							</a>
						</li>
					</xsl:for-each>
				</ul>
				
				<h2>Bytecodes</h2>
				
				<xsl:apply-templates select="$elems" />
				
				<h2>Primitives</h2>
				
				<xsl:for-each select="bytecode[@primitive]">
					<xsl:value-of select="@name" />
					<xsl:if test="position() != last()">, </xsl:if>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>


	<xsl:template match="bytecode">
		<h3><a name="{@name}"><xsl:value-of select="@name" /></a></h3>
		
		<p class="type">
			<xsl:choose>
				<xsl:when test="not(arg)">
					No arguments or variants
				</xsl:when>
				<xsl:otherwise>
					[
					<xsl:for-each select="arg">
						<xsl:apply-templates select="." />
						<xsl:if test="position() != last()">, </xsl:if>
					</xsl:for-each>
					]
					<xsl:variable name="variants" select="count(specific)" />
					<xsl:choose>
						<xsl:when test="$variants = 0"></xsl:when>
						<xsl:when test="$variants = 1">1 variant</xsl:when>
						<xsl:otherwise><xsl:value-of select="$variants" /> variants</xsl:otherwise>
					</xsl:choose>
				</xsl:otherwise>
			</xsl:choose>
		</p>
		
		<xsl:apply-templates select="description" />
	</xsl:template>
	
	<xsl:template match="arg[@type]">
		<xsl:value-of select="@type" />
		<xsl:if test="@mode">
			<xsl:text>:</xsl:text><xsl:value-of select="@mode" />
		</xsl:if>
		<xsl:if test="text()">
			<xsl:text>(</xsl:text><xsl:value-of select="text()" />)
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="arg[@repeat]">
		{<xsl:value-of select="@repeat" /> | 
		<xsl:for-each select="arg">
			<xsl:apply-templates select="." />
			<xsl:if test="position() != last()">, </xsl:if>
		</xsl:for-each>
		}
	</xsl:template>

	<xsl:template match="code">
		<pre><xsl:apply-templates /></pre>
	</xsl:template>

	<xsl:template match="br">
		<br /><br />
	</xsl:template>

	<xsl:template match="a">
		<a href="{@href}"><xsl:value-of select="text()" /></a>
	</xsl:template>

</xsl:stylesheet>