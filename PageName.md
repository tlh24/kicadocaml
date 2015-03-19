![http://m8ta.com/images/812_3_thumb_20.jpg](http://m8ta.com/images/812_3_thumb_20.jpg)

What is kicadocaml?  It is a OpenGL-based PCB editor written in Ocaml (obviously) which reads and writes Pcbnew's board files.  It also reads Eeschema's schematic files, which are then used to understand the overall structure of a project, and do good things like hide modules based on sheet, arrange modules based on schematic location (simulated annealing is also available), and array duplicated sub-sheets (including associated tracks).

Currently, you must start a board in Pcbnew then open it in Kicadocaml and proceed from there.  I see no reason to duplicate something that works perfectly well.  However, once the board is started, you can read the netlist to update netnames/numbers and component counts / texts (provided footprint is already present on the board).


Once a board is started you can edit with 4 modes:
  * move modules (hotkey 'm'),
  * add tracks ('a'),
  * move tracks ('t'),
  * edit text ('x').
Standard stuff.  Left mouse button does basically what you think, **right mouse button pans**, scroll wheel zooms (base 1.2), middle mouse button is multifunction:
  * rotates module in "module" mode
  * rotates text in "text" mode
  * switches layer, track size, and via size in track move/add mode (highly used!)

Other useful commands/hotkeys:
  * 'b' - break track or break zone edge into two tracks / two zone edges.
  * 'v' - add a via
  * 'r' - rotate, in any mode
  * 'h' - hide/unhide text in text mode
  * 'e' - edit text
  * Enter - cross probe transmit to Eeschema (note: eeschema must be running before Kicadocaml starts)
  * Backspace - remove track segment or zone edge
  * Delete - remove all connected segments
  * Ctrl-F - find, regex enabled
  * F3 - find next

DRC is online and robust, and tracks push/exclude eachother. There is push routing, and it works well, but you need to push slowly/gently.  I'll get around to making it more robust, I promise!  It is also possible to DRC check the whole board.

Zone fill is with a custom triangularization algorithm (use "Options->Zones->Show zone fill algorithm window" to see it in action), comparable performance to Pcbnew Kbool, with the added benefit that the generated zones render very quickly in Opengl. As Pcbnew understands polygons, which triangles are a subset of, for zone filling & gerber file generation are interoperable.

Ratsnest is about what you would expect.  Note that Pcbnew doesn't always seem to save the netnumber of all tracks, so when opening a board that has been previously saved there it is often necessary to propagate netnumbers to all tracks (Options -> Ratsnest -> Propagate netcodes).  This feature doubles as an efficient check to see if all pads are properly connected.

As the graphical interface is through OpenGL, you'll need a hardware-accelerated video card to use it well.  That said, it means that beautiful full-screen anti-aliasing comes 'for free' - just adjust the settings on your video card.  It also allows proper Z-sorting and alpha-blending, which makes interacting with a complicated multi-layer board much easier.  It also means that the screen refreshes whenever the cursor moves, which permits interactive net and module highlighting, and for you to see where a cursor snaps to when starting/editing a track.

I hope this is enough (or too much?) of an outline for people to get started.  There are still a number of bugs, of course, and I've only compiled it for Linux.  If I get my hands on a Macintosh, I'm pretty sure it'll compile there too, so can post some binaries.