<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns="http://www.w3.org/1999/xhtml">

	<xsl:output method="text" encoding="iso-8859-1" />
	
	<xsl:template match="bytecodes">
<xsl:text>-- Generated from bytecode.xml
-- DO NOT EDIT DIRECTLY

module ByteCode.Raw(readRaw, writeRaw, RawCodes(..)) where

</xsl:text>

		<xsl:apply-templates mode="data" select="." />
		<xsl:apply-templates mode="read" select="." />
		<xsl:apply-templates mode="write" select="." />
	</xsl:template>

	
	<xsl:template mode="data" match="bytecodes">
		<xsl:variable name="items" select="bytecode/arg/@mode" />
		<xsl:for-each select="$items">
			<xsl:variable name="pos" select="position()" />
			<xsl:if test="not($items[(position() &lt; $pos) and (. = $items[$pos])])">
				<xsl:text>type </xsl:text>
				<xsl:value-of select="concat(translate(substring(.,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),substring(.,2))" />
				<xsl:text> = Int
</xsl:text>
			</xsl:if>
		</xsl:for-each>


<xsl:text>
data RawCode =</xsl:text>

		<xsl:for-each select="bytecode[not(@depreciated)]">
<xsl:text>
    </xsl:text>
    	<xsl:choose>
    		<xsl:when test="position() != 1">| </xsl:when>
    		<xsl:otherwise><xsl:text>  </xsl:text></xsl:otherwise>
    	</xsl:choose>
    	<xsl:value-of select="@name" />
    		<xsl:apply-templates mode="data" select="arg" />
		</xsl:for-each>
		<xsl:text>

</xsl:text>
	</xsl:template>

	
	<xsl:template mode="data" match="arg">
		<xsl:if test="not(position()=1) or not(../arg[position()=last()]/@repeat)">
			<xsl:text> </xsl:text>
			<xsl:choose>
				<xsl:when test="@mode">
					<xsl:value-of select="concat(translate(substring(@mode,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),substring(@mode,2))" />
				</xsl:when>
				<xsl:otherwise>Int</xsl:otherwise>
			</xsl:choose>
		</xsl:if>
	</xsl:template>
	
	<xsl:template mode="data" match="arg[@repeat]">
		<xsl:text> [</xsl:text>
		<xsl:choose>
			<xsl:when test="count(arg) = 1">
				<xsl:text>Int</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>(</xsl:text>
				<xsl:for-each select="arg">
					<xsl:text>Int</xsl:text>
					<xsl:if test="position() != last()">, </xsl:if>
				</xsl:for-each>
				<xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>]</xsl:text>
	</xsl:template>
	
	<xsl:template mode="read" match="bytecodes">
<xsl:text>
readRawIns :: Int -> BinaryRead RawCode
readRawIns x = case x of
</xsl:text>
		<xsl:apply-templates mode="read" select="bytecode" />
	</xsl:template>
	
	<xsl:template mode="read" match="bytecode">
		<xsl:variable name="name" select="@name" />
		<xsl:for-each select=".|specific">
			<xsl:sort select="@value" data-type="number" />
		
			<xsl:text>    </xsl:text><xsl:value-of select="@value" /><xsl:text> -> do {</xsl:text>
			<xsl:apply-templates mode="read1" select="arg" />
			<xsl:text>return </xsl:text>
			<xsl:if test="arg">(</xsl:if>
			<xsl:value-of select="$name" />
			<xsl:apply-templates mode="read2" select="arg" />
			<xsl:if test="arg">)</xsl:if>
			<xsl:text>}
</xsl:text>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template mode="read1" match="arg[@value]">
	</xsl:template>
	
	<xsl:template mode="read1" match="arg[@type]">
		<xsl:text>r</xsl:text><xsl:value-of select="position()" />
		<xsl:text> &lt;- </xsl:text>
		<xsl:choose>
			<xsl:when test="@type = 'u8'">rUByte</xsl:when>
			<xsl:when test="@type = 's8'">rSByte</xsl:when>
			<xsl:when test="@type = 'u16'">rUShort</xsl:when>
			<xsl:when test="@type = 's16'">rSShort</xsl:when>
			<xsl:otherwise>error "unknown type"</xsl:otherwise>
		</xsl:choose>
		<xsl:text>; </xsl:text>
	</xsl:template>
	
	<xsl:template mode="read1" match="arg[@repeat]">
		<xsl:text>r</xsl:text><xsl:value-of select="position()" />
		<xsl:text> &lt;- sequence (replicate r</xsl:text><xsl:value-of select="@repeat" />
		<xsl:text> (do {</xsl:text>
		<xsl:apply-templates mode="read1" select="arg" />
		<xsl:text>return (</xsl:text>
		<xsl:for-each select="arg">
			<xsl:text>r</xsl:text><xsl:value-of select="position()" />
			<xsl:if test="position() != last()">, </xsl:if>
		</xsl:for-each>
		<xsl:text>)})); </xsl:text>
	</xsl:template>
	
	<xsl:template mode="read2" match="arg[@type or @repeat]">
		<xsl:if test="not(position()=1) or not(../arg[position()=last()]/@repeat)">
			<xsl:text> r</xsl:text><xsl:value-of select="position()" />
		</xsl:if>
	</xsl:template>
	
	<xsl:template mode="read2" match="arg[@value]">
		<xsl:text> </xsl:text><xsl:value-of select="@value" />
	</xsl:template>


	<xsl:template mode="write" match="bytecodes">
<xsl:text>

writeRawIns :: RawCode -> BinaryWrite
writeRawIns x = case x of
</xsl:text>
		<xsl:apply-templates mode="write" select="bytecode" />
	</xsl:template>
	
	<xsl:template mode="write" match="bytecode">
		<xsl:variable name="name" select="@name" />
		<xsl:for-each select=".|specific">
			<xsl:sort select="arg/@value" order="descending" />
			<xsl:text>    </xsl:text>
			<xsl:value-of select="$name" />
			<xsl:apply-templates mode="write1" select="arg" />
			<xsl:if test="not(@name) and arg[position()=1]/@type">
				<xsl:apply-templates mode="write3" select="arg[position()=1]" />
			</xsl:if>
			<xsl:text> -> do {wUByte </xsl:text><xsl:value-of select="@value" />
			<xsl:apply-templates mode="write2" select="arg" />
			<xsl:text>}
</xsl:text>
		</xsl:for-each>
	</xsl:template>
		
	<xsl:template mode="write3" match="arg">
		<xsl:text> | </xsl:text>
		<xsl:choose>
			<xsl:when test="@type = 'u8'">isUByte</xsl:when>
			<xsl:when test="@type = 's8'">isSByte</xsl:when>
			<xsl:otherwise>error "doh"</xsl:otherwise>
		</xsl:choose>
		<xsl:text> r1</xsl:text>
	</xsl:template>
	
	<xsl:template mode="write1" match="arg">
		<xsl:text> r</xsl:text><xsl:value-of select="position()" />
	</xsl:template>
	
	<xsl:template mode="write1" match="arg[@value]">
		<xsl:text> </xsl:text><xsl:value-of select="@value" />
	</xsl:template>
	
	<xsl:template mode="write2" match="arg[@value]">
	</xsl:template>
	
	<xsl:template mode="write2" match="arg[@type]">
		<xsl:choose>
			<xsl:when test="not(position()=1) or not(../arg[position()=last()]/@repeat)">
				<xsl:text>; </xsl:text>
				<xsl:choose>
					<xsl:when test="@type = 'u8'">wUByte</xsl:when>
					<xsl:when test="@type = 's8'">wSByte</xsl:when>
					<xsl:when test="@type = 'u16'">wUShort</xsl:when>
					<xsl:when test="@type = 's16'">wSShort</xsl:when>
					<xsl:otherwise>error "doh"</xsl:otherwise>
				</xsl:choose>
				<xsl:text> r</xsl:text><xsl:value-of select="position()" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>; wUShort (length r</xsl:text>
				<xsl:value-of select="last()" />
				<xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template mode="write2" match="arg[@repeat]">
		<xsl:text>; mapM_ (\(</xsl:text>
		<xsl:apply-templates mode="write5" select="arg" />
		<xsl:text>) -> do {</xsl:text>
		<xsl:apply-templates mode="write4" select="arg" />
		<xsl:text>}) r</xsl:text>
		<xsl:value-of select="last()" />
	</xsl:template>
	
	<xsl:template mode="write4" match="arg">
		<xsl:choose>
			<xsl:when test="@type = 'u8'">wUByte</xsl:when>
			<xsl:when test="@type = 's8'">wSByte</xsl:when>
			<xsl:when test="@type = 'u16'">wUShort</xsl:when>
			<xsl:when test="@type = 's16'">wSShort</xsl:when>
			<xsl:otherwise>error "doh"</xsl:otherwise>
		</xsl:choose>
		<xsl:text> s</xsl:text>
		<xsl:value-of select="position()" />
		<xsl:if test="position() != last()">; </xsl:if>
	</xsl:template>
	
	<xsl:template mode="write5" match="arg">
		<xsl:text>s</xsl:text><xsl:value-of select="position()" />
		<xsl:if test="position() != last()">, </xsl:if>
	</xsl:template>
</xsl:stylesheet>
