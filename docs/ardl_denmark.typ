// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

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
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
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
  if type(it.kind) != "string" {
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
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
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
          block(fill: white, width: 100%, inset: 8pt, body))
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
  font: "linux libertine",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "linux libertine",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
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
  title: [ARDL Model for Denmark],
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

== Basic Example
<basic-example>
This is a basic example which shows how to use the main functions of the ARDL package.

Assume that we want to model the LRM (logarithm of real money, M2) as a function of LRY, IBO and IDE (see ?denmark). The problem is that applying an OLS regression on non-stationary data would result into a spurious regression. The estimated parameters would be consistent only if the series were cointegrated.

#block[
```r
library(ARDL)
```

#block[
```
To cite the ARDL package in publications:

Use this reference to refer to the validity of the ARDL package.

  Natsiopoulos, Kleanthis, and Tzeremes, Nickolaos G. (2022). ARDL
  bounds test for cointegration: Replicating the Pesaran et al. (2001)
  results for the UK earnings equation using R. Journal of Applied
  Econometrics, 37(5), 1079-1090. https://doi.org/10.1002/jae.2919

Use this reference to cite this specific version of the ARDL package.

  Kleanthis Natsiopoulos and Nickolaos Tzeremes (2023). ARDL: ARDL, ECM
  and Bounds-Test for Cointegration. R package version 0.2.4.
  https://CRAN.R-project.org/package=ARDL
```

]
```r
data(denmark)
```

]
First, we find the best ARDL specification. We search up to order 5.

#block[
```r
models <- auto_ardl(LRM ~ LRY + IBO + IDE, data = denmark, max_order = 5)

# The top 20 models according to the AIC
models$top_orders
```

#block[
```
   LRM LRY IBO IDE       AIC
1    3   1   3   2 -251.0259
2    3   1   3   3 -250.1144
3    2   2   0   0 -249.6266
4    3   2   3   2 -249.1087
5    3   2   3   3 -248.1858
6    2   2   0   1 -247.7786
7    2   1   0   0 -247.5643
8    2   2   1   1 -246.6885
9    3   3   3   3 -246.3061
10   2   2   1   2 -246.2709
11   2   1   1   1 -245.8736
12   2   2   2   2 -245.7722
13   1   1   0   0 -245.6620
14   2   1   2   2 -245.1712
15   3   1   2   2 -245.0996
16   1   0   0   0 -244.4317
17   1   1   0   1 -243.7702
18   5   5   5   5 -243.3120
19   4   1   3   2 -243.0728
20   4   1   3   3 -242.4378
```

]
]
The best model is the one with the lowest AIC. We can also plot the AIC values for each model.

#block[
```r
# The best model was found to be the ARDL(3,1,3,2)
ardl_3132 <- models$best_model
ardl_3132$order
```

#block[
```
LRM LRY IBO IDE 
  3   1   3   2 
```

]
```r
#> LRM LRY IBO IDE 
#>   3   1   3   2
summary(ardl_3132)
```

#block[
```

Time series regression with "zooreg" data:
Start = 1974 Q4, End = 1987 Q3

Call:
dynlm::dynlm(formula = full_formula, data = data, start = start, 
    end = end)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.029939 -0.008856 -0.002562  0.008190  0.072577 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.6202     0.5678   4.615 4.19e-05 ***
L(LRM, 1)     0.3192     0.1367   2.336 0.024735 *  
L(LRM, 2)     0.5326     0.1324   4.024 0.000255 ***
L(LRM, 3)    -0.2687     0.1021  -2.631 0.012143 *  
LRY           0.6728     0.1312   5.129 8.32e-06 ***
L(LRY, 1)    -0.2574     0.1472  -1.749 0.088146 .  
IBO          -1.0785     0.3217  -3.353 0.001790 ** 
L(IBO, 1)    -0.1062     0.5858  -0.181 0.857081    
L(IBO, 2)     0.2877     0.5691   0.505 0.616067    
L(IBO, 3)    -0.9947     0.3925  -2.534 0.015401 *  
IDE           0.1255     0.5545   0.226 0.822161    
L(IDE, 1)    -0.3280     0.7213  -0.455 0.651847    
L(IDE, 2)     1.4079     0.5520   2.550 0.014803 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.0191 on 39 degrees of freedom
Multiple R-squared:  0.988, Adjusted R-squared:  0.9843 
F-statistic: 266.8 on 12 and 39 DF,  p-value: < 2.2e-16
```

]
]
Then we can estimate the UECM (Unrestricted Error Correction Model) of the underlying ARDL(3,1,3,2).

#block[
```r
uecm_3132 <- uecm(ardl_3132)
summary(uecm_3132)
```

#block[
```

Time series regression with "zooreg" data:
Start = 1974 Q4, End = 1987 Q3

Call:
dynlm::dynlm(formula = full_formula, data = data, start = start, 
    end = end)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.029939 -0.008856 -0.002562  0.008190  0.072577 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.62019    0.56777   4.615 4.19e-05 ***
L(LRM, 1)    -0.41685    0.09166  -4.548 5.15e-05 ***
L(LRY, 1)     0.41538    0.11761   3.532  0.00108 ** 
L(IBO, 1)    -1.89172    0.39111  -4.837 2.09e-05 ***
L(IDE, 1)     1.20534    0.44690   2.697  0.01028 *  
d(L(LRM, 1)) -0.26394    0.10192  -2.590  0.01343 *  
d(L(LRM, 2))  0.26867    0.10213   2.631  0.01214 *  
d(LRY)        0.67280    0.13116   5.129 8.32e-06 ***
d(IBO)       -1.07852    0.32170  -3.353  0.00179 ** 
d(L(IBO, 1))  0.70701    0.46874   1.508  0.13953    
d(L(IBO, 2))  0.99468    0.39251   2.534  0.01540 *  
d(IDE)        0.12546    0.55445   0.226  0.82216    
d(L(IDE, 1)) -1.40786    0.55204  -2.550  0.01480 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.0191 on 39 degrees of freedom
Multiple R-squared:  0.7458,    Adjusted R-squared:  0.6676 
F-statistic: 9.537 on 12 and 39 DF,  p-value: 3.001e-08
```

]
]
And also the RECM (Restricted Error Correction Model) of the underlying ARDL(3,1,3,2), allowing the constant to join the long-run relationship (case 2), instead of the short-run (case 3).

#block[
```r
recm_3132 <- recm(uecm_3132, case = 2)
summary(recm_3132)
```

#block[
```

Time series regression with "zooreg" data:
Start = 1974 Q4, End = 1987 Q3

Call:
dynlm::dynlm(formula = full_formula, data = data, start = start, 
    end = end)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.029939 -0.008856 -0.002562  0.008190  0.072577 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
d(L(LRM, 1)) -0.26394    0.09008  -2.930 0.005405 ** 
d(L(LRM, 2))  0.26867    0.09127   2.944 0.005214 ** 
d(LRY)        0.67280    0.11591   5.805 7.03e-07 ***
d(IBO)       -1.07852    0.30025  -3.592 0.000837 ***
d(L(IBO, 1))  0.70701    0.44359   1.594 0.118300    
d(L(IBO, 2))  0.99468    0.36491   2.726 0.009242 ** 
d(IDE)        0.12546    0.48290   0.260 0.796248    
d(L(IDE, 1)) -1.40786    0.48867  -2.881 0.006160 ** 
ect          -0.41685    0.07849  -5.311 3.63e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.01819 on 43 degrees of freedom
  (0 observations deleted due to missingness)
Multiple R-squared:  0.7613,    Adjusted R-squared:  0.7113 
F-statistic: 15.24 on 9 and 43 DF,  p-value: 9.545e-11
```

]
]
Finally, we can test the stability of the coefficients of the ARDL(3,1,3,2) model. As discussed earlier #cite(<fig>, form: "prose");-

Let’s test if there is a long-run levels relationship (cointegration) using the bounds test from Pesaran et al.~(2001).

= The bounds F-test (under the case 2) rejects the NULL hypothesis (let’s say, assuming alpha = 0.01) with p-value = 0.004418.
<the-bounds-f-test-under-the-case-2-rejects-the-null-hypothesis-lets-say-assuming-alpha-0.01-with-p-value-0.004418.>
#block[
```r
bounds_f_test(ardl_3132, case = 2)
```

#block[
```

    Bounds F-test (Wald) for no cointegration

data:  d(LRM) ~ L(LRM, 1) + L(LRY, 1) + L(IBO, 1) + L(IDE, 1) + d(L(LRM,     1)) + d(L(LRM, 2)) + d(LRY) + d(IBO) + d(L(IBO, 1)) + d(L(IBO,     2)) + d(IDE) + d(L(IDE, 1))
F = 5.1168, p-value = 0.004418
alternative hypothesis: Possible cointegration
null values:
   k    T 
   3 1000 
```

]
]
The bounds F-test rejects the NULL hypothesis of no cointegration. Therefore, we can conclude that there is a long-run relationship between the series.

#block[
```r
# The bounds F-test (under the case 3) rejects the NULL hypothesis (let's say, assuming alpha = 0.01) with p-value = 0.004418.
bounds_f_test(ardl_3132, case = 3)
```

#block[
```

    Bounds F-test (Wald) for no cointegration

data:  d(LRM) ~ L(LRM, 1) + L(LRY, 1) + L(IBO, 1) + L(IDE, 1) + d(L(LRM,     1)) + d(L(LRM, 2)) + d(LRY) + d(IBO) + d(L(IBO, 1)) + d(L(IBO,     2)) + d(IDE) + d(L(IDE, 1))
F = 6.2059, p-value = 0.004146
alternative hypothesis: Possible cointegration
null values:
   k    T 
   3 1000 
```

]
```r
# The bounds t-test (under the case 3) rejects the NULL hypothesis (let's say, assuming alpha = 0.01) with p-value = 0.005538.
# We also provide the critical value bounds for alpha = 0.01.
tbounds <- bounds_t_test(uecm_3132, case = 3, alpha = 0.01)
tbounds
```

#block[
```

    Bounds t-test for no cointegration

data:  d(LRM) ~ L(LRM, 1) + L(LRY, 1) + L(IBO, 1) + L(IDE, 1) + d(L(LRM,     1)) + d(L(LRM, 2)) + d(LRY) + d(IBO) + d(L(IBO, 1)) + d(L(IBO,     2)) + d(IDE) + d(L(IDE, 1))
t = -4.5479, Lower-bound I(0) = -3.4430, Upper-bound I(1) = -4.3799,
p-value = 0.005538
alternative hypothesis: Possible cointegration
null values:
   k    T 
   3 1000 
```

]
]
#block[
```r
tbounds$tab
```

#block[
```
  statistic Lower-bound I(0) Upper-bound I(1) alpha     p.value
t -4.547939        -3.442978        -4.379886  0.01 0.005538316
```

]
]
Here we have the short-run and the long-run multipliers (with standard errors, t-statistics and p-values).

#block[
```r
multipliers(ardl_3132, type = "sr")
```

#block[
```
         Term   Estimate Std. Error    t value     Pr(>|t|)
1 (Intercept)  2.6201916  0.5677679  4.6148990 4.186867e-05
2         LRY  0.6727993  0.1311638  5.1294603 8.317401e-06
3         IBO -1.0785180  0.3217011 -3.3525465 1.790030e-03
4         IDE  0.1254643  0.5544522  0.2262852 8.221614e-01
```

]
]
We can also estimate and visualize the delay multipliers along with their standard errors.

```r
mult15 <- multipliers(ardl_3132, type = 15, se = TRUE)
plot_delay(mult15, interval = 0.95)
```

#box(image("ardl_denmark_files/figure-typst/unnamed-chunk-10-1.svg"))

Now let’s graphically check the estimated long-run relationship (cointegrating equation) against the dependent variable LRM.

```r
ce <- coint_eq(ardl_3132, case = 2)
plot_lr(ardl_3132, coint_eq = ce, show.legend = TRUE)
```

#box(image("ardl_denmark_files/figure-typst/unnamed-chunk-11-1.svg"))

Finally, we can forecast the dependent variable LRM using the ARDL(3,1,3,2) model.

Forecasting and using an ardl, uecm, or recm model in other functions are easy as they can be converted in regular lm models.

```r
ardl_3132_lm <- to_lm(ardl_3132)

# Forecast using the in-sample data
insample_data <- ardl_3132$model
predicted_values <- predict(ardl_3132_lm, newdata = insample_data)

# Convert to ts class for the plot
predicted_values <- ts(predicted_values, start = c(1974,4), frequency=4)
plot(denmark$LRM, lwd=2) #The input dependent variable
lines(predicted_values, col="red", lwd=2) #The predicted values
```

#figure([
#box(image("ardl_denmark_files/figure-typst/fig-multipliers-1.svg"))
], caption: figure.caption(
position: bottom, 
[
The red line represents the forecasted values of the dependent variable LRM using the ARDL(3,1,3,2) model.
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-multipliers>


As per @fig-multipliers, the red line represents the forecasted values of the dependent variable LRM using the ARDL(3,1,3,2) model. Similarly we do this in #strong[?\@sec-ease-of-use];.

== Ease of use
<ease-of-use>
Let’s see what it takes to build the above ARDL(3,1,3,2) model.

Using the ARDL package (literally one line of code):

#block[
```r
ardl_model <- ardl(LRM ~ LRY + IBO + IDE, data = denmark, order = c(3,1,3,2))
```

]
Without the ARDL package: (Using the dynlm package, because striving with the lm function would require extra data transformation to behave like time-series)

#block[
```r
library(dynlm)
```

#block[
```
Loading required package: zoo
```

]
#block[
```

Attaching package: 'zoo'
```

]
#block[
```
The following objects are masked from 'package:base':

    as.Date, as.Date.numeric
```

]
```r
dynlm_ardl_model <- dynlm(LRM ~ L(LRM, 1) + L(LRM, 2) + L(LRM, 3) + LRY + L(LRY, 1) +
                           IBO + L(IBO, 1) + L(IBO, 2) + L(IBO, 3) +
                           IDE + L(IDE, 1) + L(IDE, 2), data = denmark)
identical(ardl_model$coefficients, dynlm_ardl_model$coefficients)
```

#block[
```
[1] TRUE
```

]
]
An ARDL model has a relatively simple structure, although the difference in typing effort is noticeable. @fig-multipliers shows the multipliers of the ARDL(3,1,3,2) model.

Not to mention the complex transformation for an ECM. The extra typing is the least of your problems trying to do this. First you would need to figure out the exact structure of the model!

Using the ARDL package (literally one line of code):

#block[
```r
uecm_model <- uecm(ardl_model)
```

]
Without the ARDL package:

#block[
```r
dynlm_uecm_model <- dynlm(d(LRM) ~ L(LRM, 1) + L(LRY, 1) + L(IBO, 1) +
                                   L(IDE, 1) + d(L(LRM, 1)) + d(L(LRM, 2)) +
                                   d(LRY) + d(IBO) + d(L(IBO, 1)) + d(L(IBO, 2)) +
                                   d(IDE) + d(L(IDE, 1)), data = denmark)
identical(uecm_model$coefficients, dynlm_uecm_model$coefficients)
```

#block[
```
[1] TRUE
```

]
]




