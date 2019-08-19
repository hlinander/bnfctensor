<TeXmacs|1.99.2>

<style|generic>

<\body>
  <section|Cosets>

  <\eqnarray*>
    <tformat|<table|<row|<cell|g\<in\>S<rsub|n>>|<cell|>|<cell|>>|<row|<cell|U\<subset\>S<rsub|n>>|<cell|>|<cell|>>|<row|<cell|g\<nocomma\>U
    <above|=|def> <around*|{|g\<cdot\>u<around*|\||
    u\<in\>U|\<nobracket\>>|}>\<subset\>S<rsub|n>>|<cell|>|<cell|>>>>
  </eqnarray*>

  <subsection|Lagrange theorem>

  Show that all cosets of a subgroup have the same order.

  Find a bijective map between cosets:

  Injective:

  <\eqnarray*>
    <tformat|<table|<row|<cell|g\<nocomma\>U>|<cell|>|<cell|h\<nocomma\>U>>|<row|<cell|g\<nocomma\>u<rsub|1>>|<cell|>|<cell|h\<nocomma\>u<rsub|1>>>|<row|<cell|f:\<nocomma\>g\<nocomma\>U\<rightarrow\>h\<nocomma\>U>|<cell|>|<cell|>>|<row|<cell|f<around*|(|x|)>>|<cell|=>|<cell|h\<nocomma\>g<rsup|-1>x\<in\>h\<nocomma\>U>>|<row|<cell|f<around*|(|g\<nocomma\>u<rsub|1>|)>>|<cell|=>|<cell|h\<nocomma\>g<rsup|-1>g\<nocomma\>u<rsub|1>=h\<nocomma\>u<rsub|1>>>|<row|<cell|f<rsup|-1><around*|(|y|)>>|<cell|=>|<cell|g\<nocomma\>h<rsup|-1>\<nocomma\>y>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|f<around*|(|x<rsub|1>|)>>|<cell|=>|<cell|f<around*|(|x<rsub|2>|)>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|x<rsub|1>>|<cell|=>|<cell|x<rsub|2>>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|h\<nocomma\>g<rsup|-1>x<rsub|1>>|<cell|=>|<cell|h\<nocomma\>g<rsup|-1>x<rsub|2>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|x<rsub|1>>|<cell|=>|<cell|x<rsub|2>>>>>
  </eqnarray*>

  Surjective:

  <\eqnarray*>
    <tformat|<table|<row|<cell|y\<in\>h\<nocomma\>U>|<cell|\<Rightarrow\>>|<cell|\<exists\>x\<in\>h\<nocomma\>U
    \<mid\> f<around*|(|x|)>=y>>|<row|<cell|y=h\<nocomma\>u<rsub|1>>|<cell|>|<cell|>>|<row|<cell|f<around*|(|x|)>=h\<nocomma\>g<rsup|-1>x>|<cell|=>|<cell|h\<nocomma\>u<rsub|1>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|g<rsup|-1>x>|<cell|=>|<cell|u<rsub|1>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|x>|<cell|=>|<cell|g\<nocomma\>u<rsub|1>>>>>
  </eqnarray*>

  The order of a subgroup <math|U\<in\>G> divides the order
  <math|<around*|\||G|\|>>.

  <\eqnarray*>
    <tformat|<table|<row|<cell|x\<in\>g\<nocomma\>U\<wedge\>
    x\<in\>h\<nocomma\>U>|<cell|\<Rightarrow\>>|<cell|g\<nocomma\>U=h\<nocomma\>U>>|<row|<cell|x=g\<nocomma\>u<rsub|1>=h\<nocomma\>u<rsub|2>>|<cell|>|<cell|>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|\<Rightarrow\>>|<cell|>|<cell|>>>>
  </eqnarray*>

  Lagrange

  <\eqnarray*>
    <tformat|<cwith|1|1|3|3|cell-valign|c>|<table|<row|<cell|<around*|\||G|\|>>|<cell|=>|<cell|<around*|\||H|\|>\<cdot\><around*|[|G:H|]>=<around*|\||H|\|>
    <around*|\||G/H|\|>>>>>
  </eqnarray*>

  <subsection|Orbit stabilizer theorem>

  <\eqnarray*>
    <tformat|<table|<row|<cell|f:G/G<rsub|x>>|<cell|\<rightarrow\>>|<cell|\<Delta\><rsup|G><rsub|x>>>|<row|<cell|g\<nocomma\>G<rsub|x>>|<cell|\<mapsto\>>|<cell|g\<cdot\>x>>>>
  </eqnarray*>

  <math|f> is a bijection

  <math|f> is injective

  <\eqnarray*>
    <tformat|<table|<row|<cell|f<around*|(|\<nocomma\>g\<nocomma\>G<rsub|x>|)>>|<cell|=>|<cell|f<around*|(|h\<nocomma\>G<rsub|x>|)>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|g\<nocomma\>G<rsub|x>>|<cell|=>|<cell|h\<nocomma\>G<rsub|x>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|>>|<row|<cell|h<rsup|-1>g\<nocomma\>G<rsub|x>>|<cell|=>|<cell|G<rsub|x>>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|g\<cdot\>x>|<cell|=>|<cell|h\<cdot\>x>>|<row|<cell|<around*|(|h<rsup|-1>\<cdot\>g|)>\<cdot\>x>|<cell|=>|<cell|x>>|<row|<cell|h<rsup|-1>\<cdot\>g\<in\>G<rsub|x>>|<cell|>|<cell|>>|<row|<cell|h<rsup|-1>\<cdot\>g>|<cell|=>|<cell|g<rsub|x>>>|<row|<cell|g\<nocomma\>G<rsub|x>\<ni\>g\<cdot\>id>|<cell|=>|<cell|h\<cdot\>\<nocomma\>g<rsub|x>\<in\>h\<nocomma\>G<rsub|x>>>>>
  </eqnarray*>

  <math|f> is surjective

  <\eqnarray*>
    <tformat|<table|<row|<cell|p\<in\>\<Delta\><rsup|G><rsub|x>>|<cell|\<Rightarrow\>>|<cell|\<exists\>
    g\<nocomma\>G<rsub|x> \<mid\> f<around*|(|g\<nocomma\>G<rsub|x>|)>=p>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|p>|<cell|=>|<cell|g<rsub|p>\<nocomma\>\<cdot\>x>>|<row|<cell|f<around*|(|g\<nocomma\>G<rsub|x>|)>>|<cell|=>|<cell|g<rsub|p>\<cdot\>x>>|<row|<cell|g\<cdot\>x>|<cell|=>|<cell|g<rsub|p>\<cdot\>x>>|<row|<cell|>|<cell|\<Rightarrow\>>|<cell|>>|<row|<cell|f<around*|(|g\<nocomma\>G<rsub|x>|)>=g<rsub|p>x
    \<mid\> g=g<rsub|p>>|<cell|>|<cell|>>>>
  </eqnarray*>

  <\eqnarray*>
    <tformat|<table|<row|<cell|G/G<rsub|x>>|<cell|\<rightarrow\>>|<cell|\<Delta\><rsup|G><rsub|x>>>|<row|<cell|>|<cell|\<Rightarrow\>>|<cell|>>|<row|<cell|<around*|\||G/G<rsub|x>|\|>>|<cell|=>|<cell|<around*|\||\<Delta\><rsup|G><rsub|x>|\|>>>|<row|<cell|>|<cell|\<Leftrightarrow\>>|<cell|<around*|(|Lagrange|)>>>|<row|<cell|<frac|<around*|\||G|\|>|<around*|\||G<rsub|x>|\|>>>|<cell|=>|<cell|<around*|\||\<Delta\><rsup|G><rsub|x>|\|>>>|<row|<cell|<around*|\||G|\|>>|<cell|=>|<cell|<around*|\||\<Delta\><rsup|G><rsub|x>|\|>
    <around*|\||G<rsub|x>|\|>>>>>
  </eqnarray*>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?|../../../.TeXmacs/texts/scratch/no_name_2.tm>>
    <associate|auto-2|<tuple|1.1|?|../../../.TeXmacs/texts/scratch/no_name_2.tm>>
    <associate|auto-3|<tuple|1.2|?|../../../.TeXmacs/texts/scratch/no_name_2.tm>>
  </collection>
</references>