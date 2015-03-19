**Kicadocaml** is a small project that reads and writes Kicad board (.brd) files produced by Kicad's pcbnew software. It is a full layout editor in its own right, and includes DRC support, push routing, infinite zoom, ratsnest computation, triangle-mesh zone filling, netlist read/update, BOM generation, connectivity testing, hiding modules based on schematic sheet, module placement based on schematic location or simulated annealing, and arraying of components that are on duplicated schematic sub-sheets. To do the last this kicadocaml reads and understands both kicad's .brd files and the .sch files.

Kicadocaml features a fast, interactive OpenGL interface, replete with z-sorting and alpha-blending. It allows grids to place modules and zone edges, and allows free placement of tracks, either in unrestrained mode or 135deg mode.

It aims for complementary features to kicad's pcbnew - notably, at present you cannot start boards in kicadocaml, nor can you generate gerber files.

Here is a screenshot from a recent board design, showing the z-sorting, alpha blending, and antialiasing:

![http://m8ta.com/images/812_1.jpg](http://m8ta.com/images/812_1.jpg)