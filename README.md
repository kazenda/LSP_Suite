# LSP Suite

AutoLISP commands for interior drafting in AutoCAD: numbering sheets, keeping layout titles in sync with the table of contents, and spec leaders that stay linked to their labels.

**Guide:** https://kazenda.github.io/LSP_Suite/ ([Bahasa Indonesia](https://kazenda.github.io/LSP_Suite/id.html))

## Folders

| Folder | Commands |
| --- | --- |
| `Drawing Tools` | AD, CC, DIVV / DIVH, DS, NUMGRID, PF |
| `TOC and Layouts` | PT, CT, CP, CPP, CW, CTB, WQB, WS, WSS |
| `Specs and Leaders` | SL / SLSCALE, RR, RZ, EE, CS, SY |

## Install

1. Download the zip (**Code → Download ZIP**) and unzip it to a permanent folder.
2. In AutoCAD, run `APPLOAD` and load the `.lsp` files, or add them to the Startup Suite.

Most TOC and spec commands need blocks (`TOC`, `SPEC`, the title block) from a starter drawing that isn't in this repo. The guide lists every block and attribute they expect.

Made by Yeza. Shared as-is; try the commands on a copy of a drawing first.
