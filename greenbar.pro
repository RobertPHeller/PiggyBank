%#-*- postscript -*-############################################################
%
%  System        : 
%  Module        : 
%  Object Name   : $RCSfile$
%  Revision      : $Revision$
%  Date          : $Date$
%  Author        : $Author$
%  Created By    : Robert Heller
%  Created       : Sat Jul 13 15:32:17 2024
%  Last Modified : <240713.1708>
%
%  Description	
%
%  Notes
%
%  History
%	
%  $Log$
%
%#############################################################################
%
%    Copyright (C) 2024  Robert Heller D/B/A Deepwoods Software
%			51 Locke Hill Road
%			Wendell, MA 01379-9728
%
%    This program is free software; you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation; either version 2 of the License, or
%    (at your option) any later version.
%
%    This program is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with this program; if not, write to the Free Software
%    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
% 
%
%#############################################################################
Documentation
Fake Green Bar
EndDocumentation
% -- code follows this line --
%%IncludeResource: file base.ps
%%IncludeResource: file color.hdr
%%BeginResource: procset greenbar 1.0
%%DocumentProcessColors: Red Green Blue

/water { %def
 pop pop
 gsave 
   x v get 3.904048 add sx cw mul add y v get bfs  sub bfs .3 mul add 
   newpath moveto currentpoint
   sw 0 rlineto 0 bfs 3 mul rlineto sw neg 0 rlineto closepath 0 .75 0 setrgbcolor fill
   9 {
   bfs 6 mul sub newpath moveto currentpoint
   sw 0 rlineto 0 bfs 3 mul rlineto sw neg 0 rlineto closepath 0 .75 0 setrgbcolor fill} repeat
   pop pop
  grestore
} bind def
%% Definition of the color faces.
/p {
  0 0 0 FG
  false BG
  false UL
  false BX
%Face: Plain Courier bfs
  Show
} bind def

/sy {
  0 0 0 FG
  false BG
%Face: Symbol Symbol bfs
  Show
} bind def

/k {
  false BG
  false UL
  false BX
  0 0 0.9 FG
%Face: Keyword Courier bfs
  Show
} bind def

/K {
  false BG
  false UL
  false BX
  0 0 0.8 FG
%Face: Keyword_strong Courier-Bold bfs
  Show
} bind def

/c {
  false BG
  false UL
  false BX
  0.8 0 0 FG
%Face: Comment Courier bfs
  Show
} bind def

/C {
  false BG
  false UL
  false BX
  0.8 0 0 FG
%Face: Comment_strong Courier-Bold bfs
  Show
} bind def

/l {
  0 0 0 FG
  0.8 0.8 0 true BG
  false UL
  false BX
%Face: Label Courier bfs
  Show
} bind def

/L {
  0 0 0 FG
  1 1 0 true BG
  false UL
  false BX
%Face: Label_strong Courier-Bold bfs
  Show
} bind def

/str {
  false BG
  false UL
  false BX
  0 0.5 0 FG
%Face: String Times-Roman bfs
  Show
} bind def

/e{
  1 0 0 true BG
  false UL
  true BX
  1 1 1 FG
%Face: Error Helvetica-Bold bfs
  Show
} bind def

% Function print line number (<string> # -)
/# {
  gsave
    sx cw mul 2 div neg 0 rmoveto
    f# setfont
    0.8 0.1 0.1 FG
    c-show
  grestore
} bind def
   

%%EndResource
%%BeginSetup
% The font for line numbering
/f# /Helvetica findfont bfs .6 mul scalefont def
%%EndSetup

