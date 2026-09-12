// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#show: doc => article(
  title: [EC614: Macroeconomic Forecasting],
  subtitle: [School of Economics, QAU, Islamabad],
  margin: (x: 1cm,y: 1cm,),
  paper: "a4",
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

#line(length: 100%, stroke: 1pt)

#horizontalrule

#show: columns.with(2, gutter: 1em)
= Section A: Multiple Choice Questions (25 marks)
<section-a-multiple-choice-questions-25-marks>
#emph[Select the best answer for each question.]

=== Question 1
<question-1>
In a reduced-form VAR, the error terms across equations are:

#block[
#set enum(numbering: "a)", start: 1)
+ Always uncorrelated by construction
+ May be correlated if variables are related
+ Always normally distributed
+ Independent of past values
]

=== Question 2
<question-2>
The primary advantage of ARDL over traditional cointegration methods is:

#block[
#set enum(numbering: "a)", start: 1)
+ It requires all variables to be I(1)
+ It works with variables of mixed integration orders I(0) and I(1)
+ It is computationally simpler
+ It doesn't require lag selection
]

=== Question 3
<question-3>
In Granger causality testing, if X Granger-causes Y, it means:

#block[
#set enum(numbering: "a)", start: 1)
+ X definitely causes Y in a causal sense
+ Past values of X help predict Y beyond Y's own past
+ Y cannot Granger-cause X simultaneously
+ X and Y must be cointegrated
]

=== Question 4
<question-4>
The Pesaran-Shin-Smith (2001) bounds test in ARDL tests the null hypothesis:

#block[
#set enum(numbering: "a)", start: 1)
+ All variables are I(1)
+ Variables are cointegrated
+ No long-run relationship exists
+ Variables are stationary
]

=== Question 5
<question-5>
In a structural VAR (SVAR), identification restrictions are imposed on:

#block[
#set enum(numbering: "a)", start: 1)
+ The lag length only
+ The contemporaneous relationships among variables
+ The forecast horizon
+ The variance of residuals
]

=== Question 6
<question-6>
Which statement about impulse response functions (IRFs) is FALSE?

#block[
#set enum(numbering: "a)", start: 1)
+ They trace dynamic effects of shocks over time
+ They require the VAR to have uncorrelated errors
+ Confidence bands indicate statistical uncertainty
+ They are always symmetric between variables
]

=== Question 7
<question-7>
In a recursive VAR, the ordering of variables matters because:

#block[
#set enum(numbering: "a)", start: 1)
+ It determines computational efficiency
+ It imposes assumptions about contemporaneous effects
+ It affects lag length selection
+ It changes the sample size
]

=== Question 8
<question-8>
The forecast error variance decomposition (FEVD) answers:

#block[
#set enum(numbering: "a)", start: 1)
+ How accurate the forecast is
+ What fraction of forecast error variance is due to each shock
+ Whether the model is correctly specified
+ If variables are cointegrated
]

=== Question 9
<question-9>
If two I(1) variables are cointegrated, their linear combination is:

#block[
#set enum(numbering: "a)", start: 1)
+ I(1)
+ I(2)
+ I(0)
+ Non-stationary with no defined order
]

=== Question 10
<question-10>
The error correction term in a VECM represents:

#block[
#set enum(numbering: "a)", start: 1)
+ The forecasting error
+ Deviation from long-run equilibrium
+ Short-run dynamics
+ Measurement error
]

=== Question 11
<question-11>
In the Johansen cointegration test, the trace statistic tests:

#block[
#set enum(numbering: "a)", start: 1)
+ Whether rank(Π) = 0
+ Whether rank(Π) ≤ r against rank(Π) \> r
+ Stationarity of individual series
+ Granger causality
]

=== Question 12
<question-12>
Weak exogeneity in a VECM context means:

#block[
#set enum(numbering: "a)", start: 1)
+ The variable doesn't respond to disequilibrium
+ The variable is determined outside the model
+ The variable is stationary
+ The variable has weak predictive power
]

=== Question 13
<question-13>
The primary problem with estimating a VAR with non-stationary I(1) variables that are NOT cointegrated is:

#block[
#set enum(numbering: "a)", start: 1)
+ Computational complexity
+ Spurious regression
+ Loss of degrees of freedom
+ Heteroskedasticity
]

=== Question 14
<question-14>
In ARDL modeling, if the F-statistic falls between the lower and upper bounds:

#block[
#set enum(numbering: "a)", start: 1)
+ We reject cointegration
+ We accept cointegration
+ The result is inconclusive
+ We need to increase lag length
]

=== Question 15
<question-15>
The Cholesky decomposition in recursive VAR estimation:

#block[
#set enum(numbering: "a)", start: 1)
+ Tests for cointegration
+ Orthogonalizes the VAR residuals
+ Selects optimal lag length
+ Estimates structural parameters
]

=== Question 16
<question-16>
Compared to univariate ARIMA models, VARs:

#block[
#set enum(numbering: "a)", start: 1)
+ Are always superior for forecasting
+ Capture cross-variable dynamics
+ Require less data
+ Have clearer structural interpretation
]

=== Question 17
<question-17>
In panel data with fixed effects, the "within" transformation:

#block[
#set enum(numbering: "a)", start: 1)
+ Adds time-specific effects
+ Removes individual-specific means
+ Differences all variables
+ Standardizes variables
]

=== Question 18
<question-18>
The Hausman test in panel data compares:

#block[
#set enum(numbering: "a)", start: 1)
+ Fixed effects vs pooled OLS
+ Random effects vs pooled OLS
+ Fixed effects vs random effects
+ Static vs dynamic models
]

=== Question 19
<question-19>
In SVAR identification using the Blanchard-Perotti approach:

#block[
#set enum(numbering: "a)", start: 1)
+ Only short-run restrictions are used
+ Institutional knowledge provides parameter values
+ All variables must be ordered recursively
+ Long-run restrictions are necessary
]

=== Question 20
<question-20>
When VAR residuals show autocorrelation, the most likely issue is:

#block[
#set enum(numbering: "a)", start: 1)
+ Too many lags included
+ Insufficient lag length
+ Non-stationarity
+ Heteroskedasticity
]

#horizontalrule

] // End columns for Section A
#pagebreak()

// Start columns for Section B
#columns(2, gutter: 1em)[
= Part B: Short Answer Questions (30 marks)
<part-b-short-answer-questions-30-marks>
\[… all your content …\]

] // End columns for Section B
= Part B: Short Answer Questions (30 marks)
<part-b-short-answer-questions-30-marks-1>
#emph[Answer all questions. Write concisely but completely.]

== Question 1 (5 marks)
<question-1-5-marks>
Explain the difference between reduced-form VAR, recursive VAR, and structural VAR. Under what circumstances would you prefer using each type? Provide a concrete example from macroeconomic policy analysis.

== Question 2 (5 marks)
<question-2-5-marks>
You estimate an ARDL(2,4,0,0,0,4,4,1) model for a relationship between multiple variables.

#strong[\(a)] What do the numbers in parentheses represent?

#strong[\(b)] The bounds test F-statistic = 6.45 (critical values: I(0) = 2.45, I(1) = 3.61 at 5% level). What do you conclude about cointegration?

#strong[\(c)] If cointegration exists, what is your next step in the analysis?

== Question 3 (5 marks)
<question-3-5-marks>
Consider a three-variable VECM for money (M), prices (P), and GDP (Y) for Pakistan.

The Johansen test indicates r = 1 cointegrating relationship. The estimated cointegrating vector is:

$ bold(beta)' = [1 \, - 0.85 \, - 0.35] upright(" for ") [M \, P \, Y] $

#strong[\(a)] Interpret this long-run relationship economically.

#strong[\(b)] What does r = 1 tell you about the system?

#strong[\(c)] How would you test if money (M) is weakly exogenous?

== Question 4 (15 marks)
<question-4-15-marks>
You are analyzing monetary policy transmission in Pakistan using quarterly data from 2000Q1 to 2023Q4 for the following variables:

- GDP growth rate (Y)
- CPI inflation (π)
- Policy interest rate (R)
- Real effective exchange rate (REER)

=== Part (a): Preliminary Analysis (5 marks)
<part-a-preliminary-analysis-5-marks>
#strong[\(i)] Explain the steps you would take to prepare the data before estimating a VAR model.

#strong[\(ii)] What unit root tests would you apply and why?

#strong[\(iii)] How would you handle the different orders of integration if some variables are I(0) and others are I(1)?

=== Part (b): VAR Specification (5 marks)
<part-b-var-specification-5-marks>
#strong[\(i)] Explain how you would select the optimal lag length for the VAR.

#strong[\(ii)] Would you include deterministic terms (constant, trend)? Justify your answer.

#strong[\(iii)] Write out the VAR equations in matrix form for lag order p=2.

=== Part (c): Structural Identification (5 marks)
<part-c-structural-identification-5-marks>
#strong[\(i)] Propose a recursive ordering for these variables based on institutional knowledge of Pakistan's economy. Justify your ordering.

#strong[\(ii)] Alternatively, propose one short-run restriction and one long-run restriction for SVAR identification.

#strong[\(iii)] What economic shock are you trying to identify, and why does it matter for policy?
