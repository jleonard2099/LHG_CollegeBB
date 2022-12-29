#### SIDENOTE: Do any of these fall into the "Issues" realm? If so, create GitHub issue items ####

[ ] Paths / folders
- add "Boxes" subfolder to structure and adapt code
- add "Leaders" subfolder to structure and adapt code
- add "Schedules" subfolder to structure and adapt code

[ ] SCHEDULE.BAS - menu needs to come down some

[ ] SCHEDULE.BAS - Box Score editor disabled because (uses QEdit); just remove it!

[ ] SCHEDULE.BAS - playing games should return back to the menu, not DOS

[ ] Integrate team file selection, stat file name functions
- College BB already has "STATCONVERT" subroutine
- Team file functions already built, just not tested

[ ] Review all file selection routines
- Are they overly wordy?
- Are they "centered" reasonably?

[ ] "Does this team have saved stats"??? - update to just detect it!!!

[ ] Develop "Select File" function (for LDR/LIB/etc)
- should only take a file as a parameter
- set as function to just return the file name

[ ] AutoPlay global variable
- If AP=1 and u5 = 6 or u5...
- Use this conditional once to set a flag
	- Use that flag later in the game to make decisions
	- Give the flag a user-friendly name like silentPBP

[ ] Update RECORDS file(s) using real-world data

[ ] How to handle teams before 3-pointer was implemented?
- Currently they won't attempt 3-pointers
- Should we develop a way for them to adapt?
