<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Stylesheet parameters -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Whether labels should start on their own line -->
  <xsl:param name="labels-on-own-line">
    <xsl:text>0</xsl:text>
  </xsl:param>
  <!-- Environment -->
  <!-- name of current article (upper case) -->
  <xsl:param name="aname">
    <xsl:value-of select="string(/*/@articleid)"/>
  </xsl:param>
  <!-- name of current article (lower case) -->
  <xsl:param name="anamelc">
    <xsl:value-of select="translate($aname, $ucletters, $lcletters)"/>
  </xsl:param>
  <!-- .idx file with identifier names -->
  <xsl:param name="evl">
    <xsl:value-of select="concat($anamelc, &apos;.evl&apos;)"/>
  </xsl:param>
  <!-- prefix given to all variables -->
  <xsl:param name="variable-prefix">
    <xsl:text/>
  </xsl:param>
  <!-- suppress printing the environment.  Set this to 1 to skip printing -->
  <!-- the environment. -->
  <xsl:param name="suppress-environment">
    <xsl:text/>
  </xsl:param>
  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Utilities -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <xsl:variable name="supported-version">
    <xsl:text>7.13.01</xsl:text>
  </xsl:variable>

  <xsl:template name="pad-version-info">
    <xsl:param name="s"/>
    <xsl:value-of select="concat ($s, &quot;
&quot;, &quot;This stylesheet is known to support version &quot;, $supported-version, &quot; of the Mizar system.&quot;, &quot;
&quot;, &quot;It may not support earlier or later versions.&quot;)"/>
  </xsl:template>

  <xsl:template match="*[@line and @col]" mode="die">
    <xsl:param name="message"/>
    <xsl:variable name="line" select="@line"/>
    <xsl:variable name="col" select="@col"/>
    <xsl:variable name="final_message" select="concat ($message, &quot; (line &quot;, $line, &quot;, column &quot;, $col, &quot;)&quot;)"/>
    <xsl:variable name="with-version-info">
      <xsl:call-template name="pad-version-info">
        <xsl:with-param name="s" select="$final_message"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:message terminate="yes">
      <xsl:value-of select="$with-version-info"/>
    </xsl:message>
  </xsl:template>

  <xsl:template match="*[not(@line) or not(@col) and preceding::*[@line and @col]]" mode="die">
    <xsl:param name="message"/>
    <xsl:variable name="nearest-with-line-and-col-info" select="preceding::*[@line and @col][1]"/>
    <xsl:variable name="line" select="$nearest-with-line-and-col-info/@line"/>
    <xsl:variable name="col" select="$nearest-with-line-and-col-info/@col"/>
    <xsl:variable name="final_message" select="concat ($message, &quot; (we were unable to detemine line and column information for the current context node, but the nearest preceding node with line and column is at line &quot;, $line, &quot; and column &quot;, $col, &quot;)&quot;)"/>
    <xsl:variable name="with-version-info">
      <xsl:call-template name="pad-version-info">
        <xsl:with-param name="s" select="$final_message"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:message terminate="yes">
      <xsl:value-of select="$with-version-info"/>
    </xsl:message>
  </xsl:template>

  <xsl:template match="*[not(@line) and not(@col) and not(preceding::*[@line and @col])]" mode="die">
    <xsl:param name="message"/>
    <xsl:variable name="final_message" select="concat ($message, &quot; (unable to determine line and column information)&quot;)"/>
    <xsl:variable name="with-version-info">
      <xsl:call-template name="pad-version-info">
        <xsl:with-param name="s" select="$final_message"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:message terminate="yes">
      <xsl:value-of select="$with-version-info"/>
    </xsl:message>
  </xsl:template>

  <!-- List utilities.  Stolen from Josef.  Thanks, Josef. -->
  <xsl:template name="list">
    <xsl:param name="separ"/>
    <xsl:param name="elems"/>
    <xsl:for-each select="$elems">
      <xsl:apply-templates select="."/>
      <xsl:if test="not(position()=last())">
        <xsl:copy-of select="$separ"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:variable name="lcletters">
    <xsl:text>abcdefghijklmnopqrstuvwxyz</xsl:text>
  </xsl:variable>
  <xsl:variable name="ucletters">
    <xsl:text>ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:text>
  </xsl:variable>

  <xsl:template name="lc">
    <xsl:param name="s"/>
    <xsl:value-of select="translate($s, $ucletters, $lcletters)"/>
  </xsl:template>

  <xsl:template name="uc">
    <xsl:param name="s"/>
    <xsl:value-of select="translate($s, $lcletters, $ucletters)"/>
  </xsl:template>

  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Utility templates -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <xsl:template name="ensure-spelling">
    <xsl:if test="not(@spelling)">
      <xsl:variable name="n" select="name ()"/>
      <xsl:variable name="message" select="concat (&quot;We expected an element (&quot;, $n, &quot;) to have a spelling attribute, but it lacks one&quot;)"/>
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message" select="$message"/>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="ensure-shape">
    <xsl:if test="not(@shape)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>We expected to find an element with a shape attribute</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="ensure-variable">
    <xsl:if test="not(Variable)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>A variable was expected, but none was found!</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="ensure-variables">
    <xsl:if test="not(Variables)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>A variables list was expected, but none was found!</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="apply-variable">
    <xsl:apply-templates select="Variable[1]"/>
  </xsl:template>

  <xsl:template name="variable-list">
    <xsl:choose>
      <xsl:when test="Variables">
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text> , </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Variables/Variable"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text> , </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Variable"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="ensure-type">
    <xsl:if test="not(Standard-Type | Clustered-Type | Struct-Type)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>A type was expected, but we didn&apos;t get one</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="apply-type">
    <xsl:apply-templates select="(Standard-Type | Clustered-Type | Struct-Type)[1]"/>
  </xsl:template>

  <xsl:template name="ensure-proposition">
    <xsl:if test="not(Proposition)">
      <xsl:variable name="n" select="name ()"/>
      <xsl:choose>
        <xsl:when test="@kind">
          <xsl:variable name="k" select="@kind"/>
          <xsl:variable name="message" select="concat (&quot;We expected an element (&quot;, $n, &quot;) of kind &quot;, $k, &quot; to have a Proposition child, but it doesn&apos;t&quot;)"/>
          <xsl:call-template name="die">
            <xsl:with-param name="message" select="$message"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="message" select="concat (&quot;We expected an element (&quot;, $n, &quot;) to have a Proposition child, but it doesn&apos;t&quot;)"/>
          <xsl:call-template name="die">
            <xsl:with-param name="message" select="$message"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template name="apply-proposition">
    <xsl:apply-templates select="Proposition[1]"/>
  </xsl:template>

  <xsl:template name="ensure-term">
    <xsl:if test="not(Infix-Term | Simple-Term | Circumfix-Term | Private-Functor-Term | Fraenkel-Term | Aggregate-Term | Numeral-Term | Placeholder-Term | it-Term | Selector-Term | Forgetful-Functor-Term | Qualification-Term | Global-Choice-Term)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>A term was expected, but one was not given</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="apply-term">
    <xsl:apply-templates select="(Infix-Term | Simple-Term | Circumfix-Term | Private-Functor-Term | Fraenkel-Term | Aggregate-Term | Numeral-Term | Placeholder-Term | it-Term | Selector-Term | Forgetful-Functor-Term | Qualification-Term | Global-Choice-Term)[1]"/>
  </xsl:template>

  <xsl:template name="ensure-formula">
    <xsl:if test="not(Predicative-Formula | Private-Predicate-Formula | Negated-Formula | Conjunctive-Formula | Contradiction | Disjunctive-Formula | Conditional-Formula | Biconditional-Formula | Existential-Quantifier-Formula | Universal-Quantifier-Formula | Attributive-Formula | Qualifying-Formula)">
      <xsl:apply-templates select="." mode="die">
        <xsl:with-param name="message">
          <xsl:text>A formula was expected, but one was not given</xsl:text>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

  <xsl:template name="apply-formula">
    <xsl:apply-templates select="(Predicative-Formula | Private-Predicate-Formula | Negated-Formula | Conjunctive-Formula | Contradiction | Disjunctive-Formula | Conditional-Formula | Biconditional-Formula | Existential-Quantifier-Formula | Universal-Quantifier-Formula | Attributive-Formula | Qualifying-Formula)[1]"/>
  </xsl:template>

  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Element templates -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- By default, if we don't handle something explicitly, abort! abort! -->
  <xsl:template match="*">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Unexpected element.  How did we arrive here?</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="/">
    <xsl:choose>
      <xsl:when test="Text-Proper">
        <xsl:apply-templates select="Text-Proper"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="die">
          <xsl:with-param name="message">
            <xsl:text>The Text-Proper document element is missing.</xsl:text>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Environ">
    <xsl:text>environ</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="Ident">
    <xsl:value-of select="@name"/>
  </xsl:template>

  <xsl:template match="Directive">
    <xsl:variable name="name_lc">
      <xsl:call-template name="lc">
        <xsl:with-param name="s" select="@name"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:if test="Ident[not(@name = &quot;HIDDEN&quot;)]">
      <xsl:value-of select="$name_lc"/>
      <xsl:text> </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text>,</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="Ident[not(@name = &quot;HIDDEN&quot;)]"/>
      </xsl:call-template>
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Text-Proper">
    <xsl:if test="not($suppress-environment = &quot;1&quot;)">
      <xsl:for-each select="document ($evl, /)">
        <xsl:apply-templates select="*"/>
      </xsl:for-each>
    </xsl:if>
    <xsl:for-each select="Item">
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="Variables">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Variable"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Variable">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="$variable-prefix"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Internal-Selector-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:text>the </xsl:text>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Standard-Type">
    <xsl:call-template name="ensure-spelling"/>
    <!-- this is so ugly.  How can I get rid of this? -->
    <xsl:choose>
      <xsl:when test="Infix-Term | Simple-Term | Private-Functor-Term | Circumfix-Term | Qualification-Term | it-Term | Placeholder-Term | Internal-Selector-Term | Selector-Term | Numeral-Term | Fraenkel-Term | Forgetful-Functor-Term | Aggregate-Term | Global-Choice-Term">
        <xsl:text>(</xsl:text>
        <xsl:value-of select="@spelling"/>
        <xsl:text> of </xsl:text>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text> , </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Infix-Term | Simple-Term | Private-Functor-Term | Circumfix-Term | Qualification-Term | it-Term | Placeholder-Term | Internal-Selector-Term | Selector-Term | Numeral-Term | Fraenkel-Term | Forgetful-Functor-Term | Aggregate-Term | Global-Choice-Term"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@spelling"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Reservation&apos;]">
    <xsl:text>reserve </xsl:text>
    <xsl:call-template name="variable-list"/>
    <xsl:text> </xsl:text>
    <xsl:text>for </xsl:text>
    <xsl:call-template name="ensure-type"/>
    <xsl:call-template name="apply-type"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Section-Pragma&apos;]">
    <xsl:text>begin</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Case blocks -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <xsl:template match="Item[@kind=&apos;Per-Cases&apos;]">
    <xsl:text>per cases </xsl:text>
    <xsl:call-template name="apply-justification-if-present">
      <xsl:with-param name="end">
        <xsl:text>1</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Block&apos; and Block[@kind = &quot;Suppose&quot;]]">
    <xsl:apply-templates select="Block[@kind=&apos;Suppose&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Block&apos; and Block[@kind = &quot;Case&quot;]]">
    <xsl:apply-templates select="Block[@kind=&apos;Case&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Block&apos; and not(Block[@kind = &quot;Case&quot;]) and not(Block[@kind = &quot;Suppose&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Case-Block item that lacks both a Suppose block and Case block child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Case&apos; and Item[@kind = &quot;Case-Head&quot;]]">
    <xsl:choose>
      <xsl:when test="Item[@kind=&apos;Case-Head&apos;][1]/Collective-Assumption">
        <!-- skip applying the Case-Head template; we'll do the work here -->
        <xsl:text>case that </xsl:text>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text>
and
</xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Item[@kind=&apos;Case-Head&apos;][1]/Collective-Assumption/Conditions/Proposition"/>
        </xsl:call-template>
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="*[position() &gt; 1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="Item[@kind=&apos;Case-Head&apos;][1]"/>
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="*[position() &gt; 1]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>end;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Case&apos; and not(Item[@kind = &quot;Case-Head&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Case block that lacks a Case-Head item child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Suppose&apos; and Item[@kind = &quot;Suppose-Head&quot;]]">
    <xsl:choose>
      <xsl:when test="Item[@kind=&apos;Suppose-Head&apos;][1]/Collective-Assumption">
        <!-- skip applying the Suppose-Head template; we'll do the work here -->
        <xsl:text>suppose that </xsl:text>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text>
and
</xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Item[@kind=&apos;Suppose-Head&apos;][1]/Collective-Assumption/Conditions/Proposition"/>
        </xsl:call-template>
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="*[position() &gt; 1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="Item[@kind=&apos;Suppose-Head&apos;][1]"/>
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="*[position() &gt; 1]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>end;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Suppose&apos; and not(Item[@kind = &quot;Suppose-Head&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Suppose block that lacks a Suppose-Head child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Head&apos; and Single-Assumption]">
    <xsl:text>case </xsl:text>
    <xsl:apply-templates select="Single-Assumption[1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Head&apos; and Collective-Assumption]">
    <xsl:text>case </xsl:text>
    <xsl:apply-templates select="Collective-Assumption[1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Case-Head&apos; and not(Single-Assumption) and not(Collective-Assumption)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Case-Head item that lacks a Single-Assumption child and a Collective-Assumption child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Suppose-Head&apos; and Single-Assumption]">
    <xsl:text>suppose </xsl:text>
    <xsl:apply-templates select="Single-Assumption[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Suppose-Head&apos; and not(Single-Assumption)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Suppose-Head item that lacks a Single-Assumption child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Schemes -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <xsl:template match="Item[@kind=&apos;Scheme-Block-Item&apos; and Block[@kind = &quot;Scheme-Block&quot;]]">
    <xsl:apply-templates select="Block[@kind=&apos;Scheme-Block&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Scheme-Block-Item&apos; and not(Block[@kind = &quot;Scheme-Block&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Scheme-Block child of a Scheme-Block-Item is missing!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Scheme-Block&apos; and Item[@kind = &quot;Scheme-Head&quot;]]">
    <xsl:apply-templates select="Item[@kind=&apos;Scheme-Head&apos;][1]"/>
    <xsl:if test="*[2]">
      <!-- I guess this is the proof -->
      <xsl:text>
</xsl:text>
      <xsl:text>proof</xsl:text>
      <xsl:text>
</xsl:text>
      <xsl:apply-templates select="*[position() &gt; 1]"/>
      <xsl:text>end</xsl:text>
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Scheme-Block&apos; and not(Item[@kind = &quot;Scheme-Head&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Scheme-Head child of Scheme-Block is missing!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Definition-Item&apos;]">
    <xsl:apply-templates select="*[1]"/>
  </xsl:template>

  <xsl:template match="Type-Specification">
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="Equals">
    <xsl:text>equals</xsl:text>
  </xsl:template>

  <xsl:template match="Means">
    <xsl:text>means</xsl:text>
  </xsl:template>

  <xsl:template match="it-Term">
    <xsl:text>it</xsl:text>
  </xsl:template>

  <xsl:template match="Standard-Mode[Definiens]">
    <xsl:apply-templates select="Definiens[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Standard-Mode[Type-Specification]">
    <xsl:apply-templates select="Type-Specification[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Standard-Mode[not(Definiens) and not(Type-Specification)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Standard-Mode element that lacks a Definiens child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Expandable-Mode">
    <xsl:call-template name="ensure-type"/>
    <xsl:text> is </xsl:text>
    <xsl:call-template name="apply-type"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Mode-Definition&apos; and not(Mode-Pattern)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Mode-Definition element lacks a Mode-Pattern child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Mode-Definition&apos; and Mode-Pattern]">
    <xsl:if test="Redefine">
      <xsl:text>redefine </xsl:text>
    </xsl:if>
    <xsl:text>mode </xsl:text>
    <xsl:apply-templates select="Mode-Pattern[1]"/>
    <xsl:text>
</xsl:text>
    <xsl:if test="Standard-Mode/Type-Specification">
      <xsl:text> -&gt; </xsl:text>
      <xsl:apply-templates select="Standard-Mode/Type-Specification[1]"/>
      <xsl:choose>
        <xsl:when test="Standard-Mode/Definiens"/>
        <xsl:otherwise>
          <xsl:text>;</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>
</xsl:text>
    </xsl:if>
    <xsl:if test="Standard-Mode/Definiens">
      <xsl:text>means </xsl:text>
      <xsl:apply-templates select="Standard-Mode/Definiens[1]"/>
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
    <xsl:if test="Expandable-Mode">
      <xsl:apply-templates select="Expandable-Mode[1]"/>
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Predicate-Definition&apos; and not(Predicate-Pattern)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Predicate-Definition element lacks a Predicate-Pattern child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Predicate-Definition&apos; and Predicate-Pattern]">
    <xsl:if test="Redefine">
      <xsl:text>redefine </xsl:text>
    </xsl:if>
    <xsl:text>pred </xsl:text>
    <xsl:apply-templates select="Predicate-Pattern[1]"/>
    <xsl:text>
</xsl:text>
    <xsl:choose>
      <xsl:when test="Definiens">
        <xsl:text>means</xsl:text>
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="Definiens[1]"/>
        <xsl:text>
</xsl:text>
        <xsl:text>;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Functor-Definition&apos; and not(Operation-Functor-Pattern) and not(Bracket-Functor-Pattern)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Functor-Definition lacks both an Operation-Functor-Pattern child and a Bracket-Functor-Pattern child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Functor-Definition&apos;]">
    <xsl:if test="Redefine">
      <xsl:text>redefine </xsl:text>
    </xsl:if>
    <xsl:text>func </xsl:text>
    <xsl:apply-templates select="(Operation-Functor-Pattern | Bracket-Functor-Pattern)[1]"/>
    <!-- always exists? -->
    <xsl:if test="Type-Specification">
      <xsl:text> -&gt; </xsl:text>
      <xsl:apply-templates select="Type-Specification[1]"/>
    </xsl:if>
    <xsl:call-template name="ensure-shape"/>
    <xsl:variable name="shape" select="@shape"/>
    <xsl:if test="$shape = &quot;No-Definiens&quot;">
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
    <xsl:if test="$shape = &quot;Equals&quot;">
      <xsl:text> equals </xsl:text>
      <xsl:text>
</xsl:text>
      <xsl:choose>
        <xsl:when test="Definiens">
          <xsl:apply-templates select="Definiens[1]"/>
          <xsl:text>;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="." mode="die">
            <xsl:with-param name="message">
              <xsl:text>Functor-Definition defined by an equation lacks a Definiens child!</xsl:text>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="$shape = &quot;Means&quot;">
      <xsl:text> means </xsl:text>
      <xsl:text>
</xsl:text>
      <xsl:choose>
        <xsl:when test="Definiens">
          <xsl:apply-templates select="Definiens[1]"/>
          <xsl:text>;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="." mode="die">
            <xsl:with-param name="message">
              <xsl:text>Functor-Definition defined by a formula lacks a Definiens child!</xsl:text>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Predicate-Pattern[not(Loci[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Predicate-Pattern does not have two Loci children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Predicate-Pattern[Loci[2]]">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:apply-templates select="Loci[1]"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@spelling"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="Loci[2]"/>
  </xsl:template>

  <xsl:template match="Mode-Pattern">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="Loci">
      <xsl:if test="Loci/*[1]">
        <xsl:text> of </xsl:text>
        <xsl:apply-templates select="Loci[1]"/>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Operation-Functor-Pattern[not(Loci[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Operation-Functor-Pattern lacks two Loci children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Operation-Functor-Pattern">
    <xsl:call-template name="ensure-spelling"/>
    <!-- If either the first or the second Loci children have multiple -->
    <!-- arguments, put parentheses around them. -->
    <xsl:choose>
      <xsl:when test="Loci[1]/*[2]">
        <xsl:apply-templates select="Loci[1]">
          <xsl:with-param name="parentheses">
            <xsl:text>1</xsl:text>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="Loci[1]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="Loci[1]/*[1]">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="Loci[2]/*[1]">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="Loci[2]/*[2]">
        <xsl:apply-templates select="Loci[2]">
          <xsl:with-param name="parentheses">
            <xsl:text>1</xsl:text>
          </xsl:with-param>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="Loci[2]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Definitional-Block&apos;]">
    <xsl:text>definition</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>end</xsl:text>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Registration-Block&apos;]">
    <xsl:text>registration</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>end</xsl:text>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Identify&apos; and not((Operation-Functor-Pattern | Bracket-Functor-Pattern)[2]])">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Identify item lacks two Operation-Functor-Pattern/Bracket-Functor-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Identify&apos;]">
    <xsl:text>identify </xsl:text>
    <xsl:apply-templates select="(Operation-Functor-Pattern | Bracket-Functor-Pattern)[2]"/>
    <xsl:text> with </xsl:text>
    <xsl:apply-templates select="(Operation-Functor-Pattern | Bracket-Functor-Pattern)[1]"/>
    <xsl:if test="*[3]">
      <!-- conditions for the identification -->
      <xsl:text> when </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*[position() &gt; 2]"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Reduction&quot;]">
    <xsl:text>reduce </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text> to </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Cluster&apos;]">
    <xsl:apply-templates select="*[1]"/>
  </xsl:template>

  <xsl:template match="Existence">
    <xsl:text>existence</xsl:text>
  </xsl:template>

  <xsl:template match="Uniqueness">
    <xsl:text>uniqueness</xsl:text>
  </xsl:template>

  <xsl:template match="Coherence">
    <xsl:text>coherence</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Proof&apos;]">
    <xsl:text>proof</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>end</xsl:text>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Compatibility">
    <xsl:text>compatibility</xsl:text>
  </xsl:template>

  <xsl:template match="Consistency">
    <xsl:text>consistency</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Correctness-Condition&quot; and not(@condition)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Correctness-Condition items must have a condition attribute</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Correctness-Condition&quot; and @condition]">
    <xsl:value-of select="@condition"/>
    <xsl:choose>
      <xsl:when test="Block[@kind=&apos;Proof&apos;]">
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="Block[@kind=&apos;Proof&apos;][1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="apply-justification-if-present">
          <xsl:with-param name="end">
            <xsl:text>1</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="NegatedAdjective">
    <xsl:text>non </xsl:text>
    <xsl:apply-templates select="Adjective"/>
  </xsl:template>

  <xsl:template match="Adjective">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:choose>
      <xsl:when test="*[2]">
        <xsl:text>(</xsl:text>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text> , </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="*"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*[1]"/>
        <xsl:text> </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Adjective-Cluster">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Existential-Registration">
    <xsl:text>cluster </xsl:text>
    <xsl:apply-templates select="Adjective-Cluster[1]"/>
    <!-- always exists, right? -->
    <xsl:text> for </xsl:text>
    <xsl:call-template name="ensure-type"/>
    <xsl:call-template name="apply-type"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Functorial-Registration[not(Adjective-Cluster)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Functorial-Registation lacks an Adjective-Cluster child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Functorial-Registration[Adjective-Cluster]">
    <xsl:text>cluster </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> -&gt; </xsl:text>
    <xsl:apply-templates select="Adjective-Cluster[1]"/>
    <xsl:if test="Standard-Type | Clustered-Type | Struct-Type">
      <xsl:text> for </xsl:text>
      <xsl:call-template name="apply-type"/>
    </xsl:if>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Conditional-Registration[not(Adjective-Cluster[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Conditional-Registration lacks two Adjective-Cluster children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Conditional-Registration">
    <xsl:text>cluster </xsl:text>
    <xsl:apply-templates select="Adjective-Cluster[1]"/>
    <xsl:text> -&gt; </xsl:text>
    <xsl:apply-templates select="Adjective-Cluster[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="*[3]"/>
    <!-- this is the type, hopefully -->
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Loci-Declaration&apos;]">
    <xsl:text>let </xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Definiens">
    <xsl:if test="Label">
      <xsl:text>:</xsl:text>
      <!-- special case: labels of definiens look like :this:, not like this: -->
      <xsl:apply-templates select="Label[1]"/>
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:call-template name="ensure-shape"/>
    <xsl:variable name="shape" select="@shape"/>
    <xsl:choose>
      <xsl:when test="Partial-Definiens">
        <xsl:for-each select="Partial-Definiens">
          <xsl:apply-templates select="."/>
          <xsl:choose>
            <xsl:when test="position()=last()">
              <xsl:if test="following-sibling::*[1]">
                <!-- dangerous: the idea is that after all the -->
                <!-- Partial-Definiens, there is an element like -->
                <!-- "<Formula-Expression/>" or "<Term-Expression>"; we want -->
                <!-- the thing *after* that -->
                <xsl:text> otherwise </xsl:text>
                <xsl:apply-templates select="../*[position()=last()]"/>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text> , </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="Label">
            <xsl:apply-templates select="*[position() &gt; 1]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="*"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Partial-Definiens">
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> if </xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>

  <xsl:template match="Qualifying-Formula">
    <xsl:call-template name="ensure-term"/>
    <xsl:call-template name="apply-term"/>
    <xsl:text> is </xsl:text>
    <xsl:call-template name="ensure-type"/>
    <xsl:call-template name="apply-type"/>
  </xsl:template>

  <xsl:template match="Negated-Formula">
    <xsl:text>(</xsl:text>
    <xsl:text>not </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attribute-Definition&apos; and not(Definiens)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Attribute-Definition item lacks a Definiens child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attribute-Definition&apos;]">
    <xsl:if test="Redefine">
      <xsl:text>redefine </xsl:text>
    </xsl:if>
    <xsl:text>attr </xsl:text>
    <xsl:apply-templates select="Attribute-Pattern[1]"/>
    <!-- is this always present? -->
    <xsl:text>
</xsl:text>
    <xsl:text>means</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="Definiens[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Loci">
    <xsl:param name="parentheses"/>
    <xsl:if test="Locus">
      <xsl:if test="$parentheses = &apos;1&apos;">
        <xsl:text>(</xsl:text>
      </xsl:if>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="Locus"/>
      </xsl:call-template>
      <xsl:if test="$parentheses = &apos;1&apos;">
        <xsl:text>)</xsl:text>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Locus">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Attribute-Pattern">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:apply-templates select="Locus[1]"/>
    <xsl:text> is </xsl:text>
    <xsl:apply-templates select="Loci[1]"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Type-List">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Standard-Type | Clustered-Type | Struct-Type"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Functor-Segment[not(Type-List)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Missing Type-List child of a Functor-Segment element!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Functor-Segment[not(Type-Specification)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Missing Type-Specification child of a Functor-Segment element!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Functor-Segment[Type-List and Type-Specification]">
    <xsl:call-template name="ensure-variables"/>
    <xsl:call-template name="variable-list"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="Type-List[1]"/>
    <xsl:text>)</xsl:text>
    <xsl:text> -&gt; </xsl:text>
    <xsl:apply-templates select="Type-Specification[1]"/>
  </xsl:template>

  <xsl:template match="Predicate-Segment[not(Type-List)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Type-List child of Predicate-Segment is missing!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Predicate-Segment[Type-List]">
    <xsl:call-template name="ensure-variables"/>
    <xsl:call-template name="variable-list"/>
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="Type-List[1]"/>
    <xsl:text>]</xsl:text>
  </xsl:template>

  <xsl:template match="Scheme">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:text>scheme </xsl:text>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Implicitly-Qualified-Segment">
    <xsl:call-template name="ensure-variable"/>
    <xsl:call-template name="variable-list"/>
  </xsl:template>

  <xsl:template match="Selector">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Field-Segment[not(Selector)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Field-Segment element that lacks a Selector child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Field-Segment">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Selector"/>
    </xsl:call-template>
    <xsl:call-template name="ensure-type"/>
    <xsl:text> -&gt; </xsl:text>
    <xsl:call-template name="apply-type"/>
  </xsl:template>

  <xsl:template match="Structure-Pattern">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="Loci">
      <xsl:if test="Loci/*[1]">
        <xsl:text> over </xsl:text>
        <xsl:apply-templates select="Loci[1]"/>
      </xsl:if>
    </xsl:if>
    <xsl:text>(# </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Field-Segment"/>
    </xsl:call-template>
    <xsl:text> #)</xsl:text>
  </xsl:template>

  <xsl:template match="Ancestors">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Structure-Definition&apos; and not(Ancestors)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Structure-Definition item that lacks an Ancestors child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Structure-Definition&apos; and not(Structure-Pattern)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Structure-Definition that lacks a Structure-Pattern</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Structure-Definition&apos;]">
    <xsl:text>struct </xsl:text>
    <xsl:if test="Ancestors/*[1]">
      <xsl:text>(</xsl:text>
      <xsl:apply-templates select="Ancestors[1]"/>
      <xsl:text>) </xsl:text>
    </xsl:if>
    <xsl:apply-templates select="Structure-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Explicitly-Qualified-Segment">
    <xsl:param name="verb"/>
    <xsl:call-template name="ensure-variables"/>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Variables/Variable"/>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:choose>
      <xsl:when test="$verb = &quot;&quot;">
        <xsl:text>being</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$verb"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text> </xsl:text>
    <xsl:call-template name="ensure-type"/>
    <xsl:call-template name="apply-type"/>
  </xsl:template>

  <xsl:template match="Simple-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="$variable-prefix"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Forgetful-Functor-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:text>( </xsl:text>
    <xsl:text>the </xsl:text>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="*[1]">
      <xsl:text> of </xsl:text>
      <xsl:apply-templates select="*[1]"/>
    </xsl:if>
    <xsl:text> )</xsl:text>
  </xsl:template>

  <xsl:template match="Aggregate-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text> (# </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*"/>
    </xsl:call-template>
    <xsl:text> #)</xsl:text>
  </xsl:template>

  <xsl:template match="Selector-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:text>(the </xsl:text>
    <xsl:value-of select="@spelling"/>
    <xsl:text> of </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Arguments">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Numeral-Term[not[@number]]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Numeral-Term lacks a number!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Numeral-Term[@number]">
    <xsl:value-of select="@number"/>
  </xsl:template>

  <xsl:template match="Private-Functor-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="./*"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Contradiction">
    <xsl:text>contradiction</xsl:text>
  </xsl:template>

  <xsl:template match="Global-Choice-Term">
    <xsl:text>the </xsl:text>
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <xsl:template match="Struct-Type">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="*[1]">
      <!-- dependent struct type -->
      <xsl:text> over </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Clustered-Type[not(Adjective-Cluster)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Clustered-Type lacks an Adjective-Cluster child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Clustered-Type[Adjective-Cluster]">
    <xsl:apply-templates select="Adjective-Cluster[1]"/>
    <!-- adjectives -->
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Constant-Definition&apos;]">
    <xsl:call-template name="ensure-variable"/>
    <xsl:text>set </xsl:text>
    <xsl:call-template name="apply-variable"/>
    <xsl:text> = </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Private-Functor-Definition&apos;]">
    <xsl:call-template name="ensure-variable"/>
    <xsl:text>deffunc </xsl:text>
    <xsl:call-template name="apply-variable"/>
    <xsl:if test="Type-List">
      <xsl:if test="Type-List/*[1]">
        <xsl:text>(</xsl:text>
        <xsl:call-template name="list">
          <xsl:with-param name="separ">
            <xsl:text> , </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="elems" select="Type-List/*"/>
        </xsl:call-template>
        <xsl:text>)</xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:text> = </xsl:text>
    <xsl:apply-templates select="*[3]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Private-Predicate-Definition&apos; and not(Type-List)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Private-Predicate-Definition lacks a Type-List child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Private-Predicate-Definition&apos; and Type-List]">
    <xsl:call-template name="ensure-variable"/>
    <xsl:text>defpred </xsl:text>
    <xsl:call-template name="apply-variable"/>
    <!-- sanity check: Type-List is present -->
    <xsl:text>[ </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Type-List[1]/*"/>
    </xsl:call-template>
    <xsl:text> ]</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:text> means</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*[3]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Theorem-Reference[not(@number)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Theorem-Reference lacks a number attribute!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Theorem-Reference[@number]">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="@number"/>
  </xsl:template>

  <xsl:template match="Definition-Reference[not(@number)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Definition-Reference lacks a number attribute!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Definition-Reference[@number]">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text>:def </xsl:text>
    <xsl:value-of select="@number"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Exemplification&apos;]">
    <xsl:text>take </xsl:text>
    <xsl:choose>
      <xsl:when test="*[2]">
        <!-- take X = Y -->
        <xsl:apply-templates select="*[1]"/>
        <xsl:text> = </xsl:text>
        <xsl:apply-templates select="*[2]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*[1]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Generalization&apos;]">
    <!-- very ugly.  I've asked Czeslaw to refactor -->
    <xsl:text>let </xsl:text>
    <xsl:apply-templates select="Explicitly-Qualified-Segment | Implicitly-Qualified-Segment"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*[position() &gt; 1]"/>
  </xsl:template>

  <xsl:template match="Private-Predicate-Formula">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text>[ </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*"/>
    </xsl:call-template>
    <xsl:text> ]</xsl:text>
  </xsl:template>

  <xsl:template match="Conjunctive-Formula[*[3]]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to handle a Conjunctive-Formula that has more than two children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Conjunctive-Formula[not(*[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to handle a Conjunctive-Formula that has fewer than two children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Conjunctive-Formula">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> &amp; </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Disjunctive-Formula">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> or </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Conditional-Formula">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> implies </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Biconditional-Formula">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> iff </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Placeholder-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Attributive-Formula">
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> is </xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>

  <xsl:template match="Predicative-Formula">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:apply-templates select="Arguments[1]"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@spelling"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="Arguments[2]"/>
  </xsl:template>

  <xsl:template match="Thesis">
    <xsl:text>thesis</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;PropertyRegistration&apos;]">
    <!-- shouldn't this be "Property-Registration", for parallelism? -->
    <xsl:apply-templates select="*[1]"/>
    <!-- property -->
    <xsl:text> of </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:choose>
      <xsl:when test="*[3]">
        <!-- justification for the property registration -->
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="*[3]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- //////////////////////////////////////////////////////////////////// -->
  <!-- Properties -->
  <!-- //////////////////////////////////////////////////////////////////// -->
  <xsl:template match="Item[@kind = &quot;Property&quot; and not(@property)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>We expected a Property item to have a property attribute!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Property&quot; and Block[@kind = &quot;Proof&quot;] and Straightforward-Justification]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to handle a Property item that has both a Proof block child and a Straightforward-Justificiation child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Property&quot; and @property and Block[@kind = &quot;Proof&quot;] and not(Straightforward-Justification)]">
    <xsl:value-of select="@property"/>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="Block[@kind=&apos;Proof&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Property&quot; and @property and not(Block[@kind = &quot;Proof&quot;]) and Straightforward-Justification]">
    <xsl:value-of select="@property"/>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="Straightforward-Justification"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Property&quot; and @property and not(Straightforward-Justification) and not(Block[@kind = &quot;Proof&quot;])]">
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&quot;Correctness&quot;]">
    <xsl:text>correctness</xsl:text>
    <xsl:call-template name="apply-justification-if-present">
      <xsl:with-param name="end">
        <xsl:text>1</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Proposition">
    <xsl:choose>
      <xsl:when test="Label">
        <xsl:apply-templates select="Label[1]"/>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="*[2]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*[1]"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Label">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:if test="$labels-on-own-line = &quot;1&quot;">
      <xsl:text>
</xsl:text>
    </xsl:if>
    <xsl:value-of select="@spelling"/>
    <xsl:text>:</xsl:text>
  </xsl:template>

  <xsl:template match="Local-Reference">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Scheme-Justification[not(@spelling) or not(@idnr) or not(@nr)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Scheme-Justification lacks either a spelling, idnr, or nr attribute!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Scheme-Justification[@spelling and @idnr and @nr]">
    <xsl:text> from </xsl:text>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="@nr &gt; &quot;0&quot;">
      <xsl:text>:sch </xsl:text>
      <xsl:value-of select="@idnr"/>
    </xsl:if>
    <xsl:if test="*[1]">
      <!-- there are scheme arguments -->
      <xsl:text>(</xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*"/>
      </xsl:call-template>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Infix-Term[not(Arguments[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Infix-Term lacks two Arguments children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Infix-Term">
    <xsl:call-template name="ensure-spelling"/>
    <!-- Troublesome example: "M#".  "(M #)" is bad, "M #" is fine. -->
    <!-- Solution: write "( M # )" -->
    <xsl:text>( </xsl:text>
    <xsl:if test="Arguments[1]/*[1]">
      <xsl:choose>
        <xsl:when test="Arguments[1]/*[2]">
          <xsl:text>(</xsl:text>
          <xsl:call-template name="list">
            <xsl:with-param name="separ">
              <xsl:text> , </xsl:text>
            </xsl:with-param>
            <xsl:with-param name="elems" select="Arguments[1]/*"/>
          </xsl:call-template>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="Arguments[1]/*[1]"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:value-of select="@spelling"/>
    <xsl:if test="Arguments[2]/*[1]">
      <xsl:text> </xsl:text>
      <xsl:choose>
        <xsl:when test="Arguments[2]/*[2]">
          <xsl:text>(</xsl:text>
          <xsl:call-template name="list">
            <xsl:with-param name="separ">
              <xsl:text> , </xsl:text>
            </xsl:with-param>
            <xsl:with-param name="elems" select="Arguments[2]/*"/>
          </xsl:call-template>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="Arguments[2]/*[1]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:text> )</xsl:text>
  </xsl:template>

  <xsl:template match="Bracket-Functor-Pattern[not(Loci)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Bracket-Functor-Pattern lacks a Loci child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Bracket-Functor-Pattern[not(Right-Cirumflex-Symbol)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Bracket-Functor-Pattern lacks a Right-Circumflex-Symbol child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Bracket-Functor-Pattern">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Loci/Locus"/>
    </xsl:call-template>
    <xsl:apply-templates select="Right-Circumflex-Symbol[1]"/>
  </xsl:template>

  <xsl:template match="Fraenkel-Term">
    <xsl:call-template name="ensure-term"/>
    <xsl:text>{ </xsl:text>
    <xsl:call-template name="apply-term"/>
    <xsl:if test="Explicitly-Qualified-Segment">
      <xsl:text> where </xsl:text>
      <!-- manual listing.  Using the list template would be nicer, but -->
      <!-- I need to pass in the verb "is" -->
      <xsl:for-each select="Explicitly-Qualified-Segment">
        <xsl:apply-templates select=".">
          <xsl:with-param name="verb">
            <xsl:text>is</xsl:text>
          </xsl:with-param>
        </xsl:apply-templates>
        <xsl:if test="not(position()=last())">
          <xsl:text> , </xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:if>
    <xsl:call-template name="ensure-formula"/>
    <xsl:text>: </xsl:text>
    <xsl:call-template name="apply-formula"/>
    <xsl:text> }</xsl:text>
  </xsl:template>

  <xsl:template match="Circumfix-Term[not(Right-Circumflex-Symbol)]">
    <xsl:apply-templates mode="die">
      <xsl:with-param name="message">
        <xsl:text>Circumfix-Term lacks a right-Circumflex-Symbol child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Circumfix-Term">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:value-of select="@spelling"/>
    <xsl:text> </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="*[position() &gt; 1]"/>
    </xsl:call-template>
    <!-- everything after the Right-Circumflex-Symbol -->
    <xsl:apply-templates select="Right-Circumflex-Symbol[1]"/>
  </xsl:template>

  <xsl:template match="Right-Circumflex-Symbol">
    <xsl:call-template name="ensure-spelling"/>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@spelling"/>
  </xsl:template>

  <xsl:template match="Qualification-Term">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> qua </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="maybe-link">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>then </xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Choice-Statement&apos; and not(Conditions)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>We expected to find a Conditions child of a Choice-Statement item, but none was found</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Choice-Statement&quot; and Conditions]">
    <xsl:call-template name="maybe-link"/>
    <xsl:text>consider </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text>, </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Explicitly-Qualified-Segment | Implicitly-Qualified-Segment"/>
    </xsl:call-template>
    <xsl:if test="Conditions/Proposition">
      <xsl:text> such that </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> and </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="Conditions/Proposition"/>
      </xsl:call-template>
      <xsl:call-template name="apply-justification-if-present"/>
    </xsl:if>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Straightforward-Justification[Link]">
    <xsl:if test="*[2]">
      <xsl:text> by </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*[position() &gt; 1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Straightforward-Justification[not(Link)]">
    <xsl:if test="*[1]">
      <xsl:text> by </xsl:text>
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text> , </xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Single-Assumption">
    <xsl:call-template name="ensure-proposition"/>
    <xsl:call-template name="apply-proposition"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Assumption&apos;]">
    <xsl:if test="Single-Assumption">
      <xsl:text>assume </xsl:text>
      <xsl:apply-templates select="Single-Assumption"/>
      <xsl:text>;</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:if>
    <xsl:if test="Collective-Assumption">
      <xsl:apply-templates select="Collective-Assumption[1]"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Iterative-Step">
    <xsl:call-template name="ensure-term"/>
    <xsl:call-template name="apply-term"/>
    <xsl:call-template name="apply-justification-if-present"/>
  </xsl:template>

  <xsl:template name="apply-justification-if-present">
    <xsl:param name="end"/>
    <xsl:if test="Scheme-Justification">
      <xsl:apply-templates select="Scheme-Justification[1]"/>
      <xsl:if test="not($end = &quot;&quot;)">
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:if test="Straightforward-Justification">
      <xsl:apply-templates select="Straightforward-Justification[1]"/>
      <xsl:if test="not($end = &quot;&quot;)">
        <xsl:text>;</xsl:text>
        <xsl:text>
</xsl:text>
      </xsl:if>
    </xsl:if>
    <xsl:if test="Block[@kind=&apos;Proof&apos;]">
      <xsl:text>
</xsl:text>
      <xsl:apply-templates select="Block[@kind=&apos;Proof&apos;][1]"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Collective-Assumption[not(Conditions)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Collective-Assumption elements must have a Conditions child</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Collective-Assumption">
    <xsl:text>assume that</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text>
and
</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Conditions/Proposition"/>
    </xsl:call-template>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Conditions">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text>
and
</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Proposition"/>
    </xsl:call-template>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Existential-Assumption&apos; and not(Implicitly-Qualifed-Segment | Explicitly-Qualified-Segment)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Existential-Assumption item lacks an Implicitly-QualifiedSegment child and an Explicitly-Qualified-Segment child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Existential-Assumption&apos; and not(Conditions)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with an Existential-Assumption item that lacks a Conditions child!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Existential-Assumption&apos;]">
    <xsl:text>given </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Implicitly-Qualified-Segment | Explicitly-Qualified-Segment"/>
    </xsl:call-template>
    <xsl:text> such that</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="Conditions[1]"/>
    <xsl:call-template name="apply-justification-if-present"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Conclusion&quot; and not(Iterative-Equality or Diffuse-Statement or Compact-Statement)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to deal with a Conclusion that is not an Iterative-Equality, Diffuse-Statement, nor a Compact-Statement</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Conclusion&quot; and not(@shape)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>We expected to find an element with a shape attribute</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and @shape = &quot;Iterative-Equality&quot;]">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>hence </xsl:text>
    </xsl:if>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:if test="not(Straightforward-Justification/Link) and not(Scheme-Justification/Link)">
      <xsl:text>thus </xsl:text>
    </xsl:if>
    <xsl:call-template name="ensure-proposition"/>
    <xsl:call-template name="apply-proposition"/>
    <xsl:call-template name="apply-justification-if-present"/>
    <xsl:text>
</xsl:text>
    <xsl:text>.= </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text>
.= </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Iterative-Step"/>
    </xsl:call-template>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and @shape = &quot;Diffuse-Statement&quot; and Block[@kind = &quot;Hereby-Reasoning&quot;]]">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>hence </xsl:text>
    </xsl:if>
    <xsl:apply-templates select="Block[@kind=&apos;Hereby-Reasoning&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and @shape = &quot;Diffuse-Statement&quot; and not(Block[@kind = &quot;Hereby-Reasoning&quot;])]">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>hence </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="Straightforward-Justification/Link | Scheme-Justification/Link">
        <xsl:text>hence </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>thus </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:apply-templates select="Block[1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and @shape = &quot;Compact-Statement&quot; and Block[@kind = &quot;Proof&quot;]]">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>hence </xsl:text>
    </xsl:if>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:if test="not(Straightforward-Justification/Link) and not(Scheme-Justification/Link)">
      <xsl:text>thus </xsl:text>
    </xsl:if>
    <xsl:apply-templates select="Proposition[1]"/>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="Block[@kind=&apos;Proof&apos;][1]"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and @shape = &quot;Compact-Statement&quot; and not(Block[@kind = &quot;Proof&quot;])]">
    <xsl:if test="Straightforward-Justification/Link | Scheme-Justification/Link">
      <xsl:text>hence </xsl:text>
    </xsl:if>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:if test="not(Straightforward-Justification/Link) and not(Scheme-Justification/Link)">
      <xsl:text>thus </xsl:text>
    </xsl:if>
    <xsl:apply-templates select="Proposition[1]"/>
    <xsl:if test="Straightforward-Justification | Scheme-Justification">
      <xsl:apply-templates select="(Straightforward-Justification | Scheme-Justification)[1]"/>
    </xsl:if>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Conclusion&apos; and not(@shape = &quot;Compact-Statement&quot; or @shape = &quot;Diffuse-Statement&quot; or @shape = &quot;Iterative-Equality&quot;)]">
    <xsl:variable name="message" select="concat (&quot;Don&apos;t know how to deal with a Conclusion item whose shape is &apos;&quot;, $shape, &quot;&apos;&quot;)"/>
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message" select="$message"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Mode-Synonym&apos; and not(Mode-Pattern[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Mode-Synonym item missing two Mode-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Mode-Synonym&apos;]">
    <xsl:text>synonym </xsl:text>
    <xsl:apply-templates select="Mode-Pattern[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="Mode-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Func-Synonym&apos; and not([Operation-Functor-Pattern | Bracket-Functor-Pattern][2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Func-Synonym item missing two Operation-Functor-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Func-Synonym&apos;]">
    <xsl:text>synonym </xsl:text>
    <xsl:apply-templates select="(Operation-Functor-Pattern | Bracket-Functor-Pattern)[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="(Operation-Functor-Pattern | Bracket-Functor-Pattern)[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attr-Synonym&apos; and not(Attribute-Pattern[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Attr-Synonym item missing two Attribute-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attr-Synonym&apos;]">
    <xsl:text>synonym </xsl:text>
    <xsl:apply-templates select="Attribute-Pattern[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="Attribute-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Pred-Synonym&apos; and not(Predicate-Synonym[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Predicate-Synonym item missing two Predicate-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Pred-Synonym&apos;]">
    <xsl:text>synonym </xsl:text>
    <xsl:apply-templates select="Predicate-Pattern[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="Predicate-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attr-Antonym&apos; and not(Attribute-Pattern[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Attr-Antonym item missing two Attribute-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Attr-Antonym&apos;]">
    <xsl:text>antonym </xsl:text>
    <xsl:apply-templates select="Attribute-Pattern[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="Attribute-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Pred-Antonym&apos; and not(Predicate-Pattern[2])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Pred-Antonym item missing two Predicate-Pattern children!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Pred-Antonym&apos;]">
    <xsl:text>antonym </xsl:text>
    <xsl:apply-templates select="Predicate-Pattern[2]"/>
    <xsl:text> for </xsl:text>
    <xsl:apply-templates select="Predicate-Pattern[1]"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Notation-Block&apos;]">
    <xsl:text>notation</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:choose>
      <xsl:when test="Item[@kind=&apos;Loci-Declaration&apos;]">
        <xsl:apply-templates select="Item[@kind=&quot;Loci-Declaration&quot;][1]"/>
        <xsl:apply-templates select="Item[position() &gt; 1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>end;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Theorem-Item&apos;]">
    <xsl:call-template name="ensure-proposition"/>
    <xsl:text>theorem</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:call-template name="apply-proposition"/>
    <xsl:choose>
      <xsl:when test="Block[@kind=&apos;Proof&apos;]">
        <xsl:text>
</xsl:text>
        <xsl:apply-templates select="Block[@kind=&apos;Proof&apos;][1]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="apply-justification-if-present">
          <xsl:with-param name="end">
            <xsl:text>1</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Now-Reasoning&apos;]">
    <xsl:text>now</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>end;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Block[@kind=&apos;Hereby-Reasoning&apos;]">
    <xsl:text>hereby</xsl:text>
    <xsl:text>
</xsl:text>
    <xsl:apply-templates select="*"/>
    <xsl:text>end;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Type-Changing-Statement&apos;]">
    <xsl:call-template name="maybe-link"/>
    <xsl:text>reconsider </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Variable | Equality"/>
    </xsl:call-template>
    <xsl:call-template name="ensure-type"/>
    <xsl:text> as </xsl:text>
    <xsl:call-template name="apply-type"/>
    <xsl:call-template name="apply-justification-if-present">
      <xsl:with-param name="end">
        <xsl:text>1</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Equality">
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> = </xsl:text>
    <xsl:apply-templates select="*[2]"/>
  </xsl:template>

  <xsl:template match="Item[@kind = &quot;Regular-Statement&quot; and not(@shape)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>We expected to find an element with a shape attribute</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Regular-Statement&apos; and @shape = &quot;Iterative-Equality&quot;]">
    <xsl:call-template name="maybe-link"/>
    <xsl:call-template name="ensure-proposition"/>
    <xsl:call-template name="apply-proposition"/>
    <xsl:call-template name="apply-justification-if-present"/>
    <xsl:text>
</xsl:text>
    <xsl:text>.= </xsl:text>
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text>
.= </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Iterative-Step"/>
    </xsl:call-template>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Regular-Statement&apos; and @shape = &quot;Diffuse-Statement&quot; and not(Block[@kind = &quot;Now-Reasoning&quot;] or Block[@kind = &quot;Hereby-Reasoning&quot;])]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Don&apos;t know how to handle this piece of diffuse reasoning it is neither a &apos;now&apos; nor a &apos;hereby&apos; block.</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Regular-Statement&apos; and @shape = &quot;Diffuse-Statement&quot; and (Block[@kind = &quot;Now-Reasoning&quot;] or Block[@kind = &quot;Hereby-Reasoning&quot;])]">
    <xsl:call-template name="maybe-link"/>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:apply-templates select="Block[@kind=&apos;Now-Reasoning&apos; or @kind = &apos;Hereby-Reasoning&apos;][1]"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Regular-Statement&apos; and @shape = &quot;Compact-Statement&quot;]">
    <xsl:call-template name="maybe-link"/>
    <xsl:if test="Label">
      <xsl:apply-templates select="Label[1]"/>
    </xsl:if>
    <xsl:call-template name="ensure-proposition"/>
    <xsl:call-template name="apply-proposition"/>
    <xsl:call-template name="apply-justification-if-present">
      <xsl:with-param name="end">
        <xsl:text>1</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Universal-Quantifier-Formula">
    <xsl:text>(</xsl:text>
    <xsl:text>for </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> holds </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Existential-Quantifier-Formula">
    <xsl:text>(</xsl:text>
    <xsl:text>ex </xsl:text>
    <xsl:apply-templates select="*[1]"/>
    <xsl:text> st </xsl:text>
    <xsl:apply-templates select="*[2]"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="Schematic-Variables">
    <xsl:call-template name="list">
      <xsl:with-param name="separ">
        <xsl:text> , </xsl:text>
      </xsl:with-param>
      <xsl:with-param name="elems" select="Functor-Segment | Predicate-Segment"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="Provisional-Formulas">
    <xsl:if test="*[1]">
      <!-- there's something to do -->
      <xsl:call-template name="list">
        <xsl:with-param name="separ">
          <xsl:text>
and
</xsl:text>
        </xsl:with-param>
        <xsl:with-param name="elems" select="*"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Scheme-Head&apos; and Scheme[2]]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>More than one Scheme child of a Scheme-Head element!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Scheme-Head&apos; and not(Scheme)]">
    <xsl:apply-templates select="." mode="die">
      <xsl:with-param name="message">
        <xsl:text>Scheme child of a Scheme-Head item is missing!</xsl:text>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Scheme-Head&apos;]">
    <xsl:apply-templates select="Scheme[1]"/>
    <xsl:text> { </xsl:text>
    <xsl:apply-templates select="Schematic-Variables[1]"/>
    <xsl:text> } : </xsl:text>
    <xsl:apply-templates select="*[3]"/>
    <!-- the scheme formula -->
    <xsl:if test="Provisional-Formulas">
      <xsl:text>
</xsl:text>
      <xsl:text>provided</xsl:text>
      <xsl:text>
</xsl:text>
      <xsl:apply-templates select="Provisional-Formulas[1]"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Canceled-Pragma&apos; and not(@number)]">
    <xsl:apply-templates select="Canceled"/>
  </xsl:template>

  <xsl:template match="Item[@kind=&apos;Canceled-Pragma&apos; and @number]">
    <xsl:text>canceled </xsl:text>
    <xsl:value-of select="@number"/>
    <xsl:text>;</xsl:text>
    <xsl:text>
</xsl:text>
  </xsl:template>

  <xsl:template match="Canceled">
    <xsl:choose>
      <xsl:when test="@number">
        <xsl:text>canceled </xsl:text>
        <xsl:value-of select="@number"/>
        <xsl:text>;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>canceled;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>
</xsl:text>
  </xsl:template>
</xsl:stylesheet>
