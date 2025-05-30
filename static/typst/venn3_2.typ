#set page(width: auto, height: auto, margin: -0.1cm)
#import "@preview/cetz:0.3.2"
#import "@preview/cetz-venn:0.1.3": venn3

#cetz.canvas({
  import cetz.draw: *

  scale(1.6)
  venn3(name: "venn", a-fill: gray, b-fill: white, c-fill: white,
    ac-fill: gray, ab-fill: gray, abc-fill: gray, bc-fill: white)

  content("venn.a", [$S_1$])
  content("venn.b", [$S_2$])
  content("venn.c", [$S_3$])
  content("venn.ab", [$S_(12)$])
  content("venn.bc", [$S_(23)$])
  content("venn.ac", [$S_(13)$])
  content("venn.abc", [$S_(123)$])
})