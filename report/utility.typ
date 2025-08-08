#import "@preview/cetz:0.3.4"
#import cetz.draw: *

#let big-brace(from-x, to-x, from-y, to-y) = {
  let half-x = (from-x + to-x) / 2
  let half-y = (from-y + to-y) / 2
  let dx = calc.min(0.05, (to-x - from-x) * 0.05)

  let a = (from-x + dx, from-y)
  let b = (from-x + dx, half-y)
  let c = (half-x - dx, half-y)
  let d = (half-x,      to-y)
  let e = (half-x + dx, half-y)
  let f = (to-x   - dx, half-y)
  let g = (to-x   - dx, from-y)

  line(a, b, c, d, e, f, g)
}

#let code(
  contents,
  caption: none,
  tag:     "") = [
    #show figure: set block(breakable: true)
    #figure(
    [
      #show raw: set text(size: 8.2pt)
      #contents
    ],
    caption: caption)
    #label(tag)
  ]
