<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

	<xsl:output method="text" encoding="iso-8859-1" />
	
	<xsl:template match="bytecodes">
		<xsl:text>/* Generated from bytecode.xml */
/* DO NOT EDIT DIRECTLY */
</xsl:text>
		<xsl:variable name="items" select="bytecode" />

		<xsl:apply-templates select="$items|$items/specific">
			<xsl:sort select="@value" data-type="number" />
		</xsl:apply-templates>
	</xsl:template>
	
	<xsl:template match="bytecode|bytecode/specific">
		<xsl:variable name="name">
			<xsl:choose>
				<xsl:when test="name() = 'specific'">
					<xsl:value-of select="../@name" />
					<xsl:text>_</xsl:text>
					<xsl:choose>
						<xsl:when test="arg/@value">
							<xsl:value-of select="arg/@value" />
						</xsl:when>
						<xsl:otherwise>P1</xsl:otherwise>
					</xsl:choose>
				</xsl:when>
				<xsl:when test="specific/arg/@type"><xsl:value-of select="@name" />_P2</xsl:when>
				<xsl:otherwise><xsl:value-of select="@name" /></xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
	
		<xsl:choose>
			<xsl:when test="arg/@repeat and count(arg)=2">opT</xsl:when>
			<xsl:when test="arg/@repeat and count(arg)=3">opL</xsl:when>
			<xsl:when test="$name='JUMP' or $name='JUMP_FALSE'">opJ</xsl:when>
			<xsl:when test="not(arg[@type])">op</xsl:when>
			<xsl:otherwise>
				<xsl:text>op</xsl:text>
				<xsl:for-each select="arg">
					<xsl:apply-templates select="." />
					<xsl:if test="position() != last()">_</xsl:if>
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
		
		<xsl:text>(</xsl:text>
		<xsl:value-of select="@value" />
		<xsl:text>, </xsl:text>
		<xsl:value-of select="$name" />
		<xsl:text>)
</xsl:text>
	</xsl:template>
	
	<xsl:template match="arg[@type='u8']">1</xsl:template>
	<xsl:template match="arg[@type='u16']">2</xsl:template>
	<xsl:template match="arg[@type='s8']">1S</xsl:template>
	<xsl:template match="arg[@type='s16']">2S</xsl:template>
	
	<xsl:template match="arg[@value]"><xsl:value-of select="@value" /></xsl:template>
</xsl:stylesheet>

