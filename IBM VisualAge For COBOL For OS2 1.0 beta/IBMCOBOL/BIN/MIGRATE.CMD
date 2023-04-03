/******************************************************************************
*
*  Program Number 5763-CD1 (C) Copyright IBM Corp. 1992, 1994.
*  Program Number 5688-194 (C) Copyright IBM Corp. 1992, 1994.
*  All rights reserved.
*
******************************************************************************/

parse arg oldlxl newlxl
if newlxl = '' then exit

/* if newlxl not found, then nothing to migrate */
new = stream(newlxl, 'c', 'query exists')
if new = '' then exit

/* if oldlxl not found, then just copy newlxl to oldlxl, and done */
old = stream(oldlxl, 'c', 'query exists')
if old = '' then do
   'copy' newlxl oldlxl
   exit
   end

/* search through the old lxl file */
i = 0
tabs = ''
do until lines(oldlxl) = 0
   l = linein(oldlxl)
   parse value l with start'.'f col '"'
   if start = '''SET FONT' then do
      font.i = f
      color.i = col
      i = i + 1
   end
   parse value l with '''SET TABS' rest
   if rest <> '' then do
      tabs = l
   end
end
state = stream(oldlxl,'c','close')

/* merge new file into old file */
'@del' oldlxl
rc = lineout(oldlxl,,1)
if rc <> 0 then do
   say 'Unable to write to' oldlxl
   exit 1
end
do until lines(newlxl) = 0
   l = linein(newlxl)
   parse value l with start'.'f' 'col'"'rest
   if start = '''SET FONT' then do
      do j = 0 to i - 1 until font.j = f
      end
      if font.j = f then do
         l = start'.'f color.j||'"'rest
      end
   end
   parse value l with '''SET TABS' rest
   if (rest <> '') & (tabs <> '') then do
      l = tabs
      tabs = ''
   end
   rc = lineout(oldlxl,l)
   if rc <> 0 then do
      say 'Unable to write to' oldlxl
      exit 1
   end
end
if tabs <> '' then
   rc = lineout(oldlxl, tabs)
state = stream(newlxl,'c','close')
state = stream(oldlxl,'c','close')

exit
