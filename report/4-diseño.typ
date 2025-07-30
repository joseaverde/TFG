#import "@preview/cetz:0.3.4"
#import cetz.draw: *
#import "@preview/fletcher:0.5.7" as fletcher: diagram, node, edge

= Diseño
Una señal

#let signal-part(index, y, size, name:"") = {
  rect((index * size, y), ((index + 1) * size, y + 1), name: name)
}


#cetz.canvas({
  set-style(mark: (end: ">"))   // style for arrow
  let width = 0.2

  for i in range(20) {
    signal-part(i, 0, width)
  }

  signal-part(20, 0, width, name:"the-chosen-one")

  for i in range(21, 50) {
    signal-part(i, 0, width)
  }

  rect((1, -2), (2, -1), name: "the-message")
  line("the-chosen-one", "the-message")

})
