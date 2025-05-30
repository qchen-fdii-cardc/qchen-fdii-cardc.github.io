#set page(width: auto, height: auto, margin: -0.1cm)
#import "@preview/cetz:0.3.2"
#import "@preview/cetz-venn:0.1.3": venn2

#cetz.canvas({
  import cetz.draw: *

  scale(1.6)
  venn2(name: "venn", ab-fill: gray)

  content("venn.a", [$S_1$])
  content("venn.b", [$S_2$])
  content("venn.ab", [$S_(12)$])
})