<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

	<xsl:output method="text" encoding="iso-8859-1" />
	
	<xsl:template match="bytecodes">
		<xsl:text>bytecodes = {
</xsl:text>
			<xsl:for-each select="bytecode[not(@depreciated)]">
				<xsl:apply-templates select="." />
			</xsl:for-each>
		<xsl:text>}
</xsl:text>
	</xsl:template>
	
	<xsl:template match="bytecode">
		<xsl:variable name="name" select="@name" />
		
		<xsl:for-each select=".|specific">
			<xsl:text>    </xsl:text><xsl:value-of select="@value" /><xsl:text> : ([</xsl:text>
				<xsl:for-each select="arg">
					<xsl:apply-templates select="." />
					<xsl:if test="position() != last()">, </xsl:if>
				</xsl:for-each>
			<xsl:text>], </xsl:text><xsl:value-of select="$name" /><xsl:text>),
</xsl:text>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="arg[@repeat]">
		<xsl:text>('</xsl:text><xsl:value-of select="@repeat" /><xsl:text>', [</xsl:text>
		<xsl:for-each select="arg">
			<xsl:apply-templates select="." />
			<xsl:if test="position() != last()">, </xsl:if>
		</xsl:for-each>
		<xsl:text>])</xsl:text>
	</xsl:template>
	
	<xsl:template match="arg[@type]">
		<xsl:text>('</xsl:text><xsl:value-of select="@type" /><xsl:text>', None)</xsl:text>
	</xsl:template>
	
	<xsl:template match="arg[@value]">
		<xsl:text>(None, "</xsl:text><xsl:value-of select="@value" /><xsl:text>")</xsl:text>
	</xsl:template>

<!--	
	bytecodes = {0:  ([], [], END_CODE),
	 3:  ([(None, "16") ], NEED_STACK),
	 4:  ([('u8', None) ], NEED_STACK),
	 94: ([('u16', None), ('u16', None), ('1', [('s16', None), ('u16', None)])], INT_SWITCH)
	}
-->
</xsl:stylesheet>