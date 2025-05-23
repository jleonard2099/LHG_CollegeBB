# LH Games Courtside College Basketball #

### Game Description ###
Courtside College Basketball is a fast-paced simulator, allowing you to recreate original seasons or conjure up the tournament match of your dreams! Several styles of offense and defense give you the choice to strategize each moment. Save and compile stats, input your own teams or draft or trade players. Play against a friend, against the computer, or watch simulations at your own pace. Teams and players are rated in several categories using real-world stats, so the outcomes are not only possible - they feel real.

**· YOU determine starting lineup, substitutions, shot selection, passing, offensive and defensive styles of play and more! · Host a full 68-team tournament with play-in games - or build a mega tournament of up to 256 teams!! · Each player contributes as they did in real life! · Track team stats and records across a full season or an entire career! · Compare replays against real stats to gauge accuracy ·**

### About Project ###
With permission from Lance Haffner himself, this effort is being made to bring his original source code from the DOS world into the modern world. This is due in part to cross-platform compilers for BASIC such as [QB64pe](https://github.com/QB64-Phoenix-Edition/QB64pe). But this effort aims to restore his original vision for a cross-platform sports sim empire, while also opening the door for improvements to accuracy, usability and portability.

Several functions from the original code were linked from Crescent Software libraries. To make a faster route to compilation of the code, most of these were re-written as copycats (where code was not available or ASM routines were used). Functions which relied on packing color into 1 byte were updated to use FG/BG separately. This helped to reduce the number of 'helper' routines as possible.

### About LH Games ###
Since 1983 Lance Haffner Games has been publishing the finest quality text-based computer sports games. Recognized for their statistical accuracy; national media outlets have used Haffner's games to play matchups between great teams of the century. The Ft. Worth Star-Telegram, WTVF-TV (Nashville), The Nashville Banner, and ESPN have used Lance Haffner's games to simulate real and "what if" matchups using current teams and great teams from the past.

Lance Haffner Games were some of the premier sports simulators of the 80's. They were a staple of of the mail order days and served as one of the earliest computer sports simulations. Available for several platforms at the time thanks to their use of BASIC, they fast became known for their impressive accuracy and surprisingly quick speed. Not only that, they allowed very detailed and thorough team, conference and season management - yet somehow maintained an entertaining feel with gameplay that was never overly detailed. 

### Visuals ###
<img src="cbb-manual.png" width=394 height=344>
<img src="cbb-screen1.png">

# The Game #

### Installation ###

No formal install process is necessary. Teams files are now provided as well! Files provided are NOT backwards-compatible with the DOS versions of the game.
1) Extract zip folder contents to folder on your hard drive (preferrably not in user folders!)
2) Extract the leaders, schedules or teams you are interested in to the same folder

If you intend to manage several seasons or files:
1) Create folders for Leaders, Schedules, Stats and Teams on your system.
2) Open HELLO and check the "CONFIGURE GAME FOLDERS" option
3) Click each "Folder" icon to browse to the folders you created
4) Move or extract your team, leader and schedule files to the new folders.

### Linux Dependencies ####
- File dialog manager
  - applescript [Mac OS]
  - kdialog	[KDE]
  - Xdialog [X-Win]
  - zenity [Gnome] and variants
    - yad (GTK+)
    - matedialog (MATE)
   - shellementary (SUSE)
   - qarma (Qt)
- ALSA (libasound2 / alsa-lib)
- Mesa (libglu1-mesa / mesa-libGLU)
- OpenGL (libopengl0 / libglvnd-opengl)

### Usage
Launch HELLO binary compiled for your system directly from the folder!

### Support ###
Contact support@lhgames.net with inquiries for now.
Bugs and/or feature requests can be submitted using GitHub (see the "Issues" tab at the top of page). This will be the easiest way for me to implement ideas.

# Project Status #
## Current ##
Code now compiles in a modern environment and has been mostly re-factored. All known GUI bugs have been fixed, menu navigation is improved and code was consolidated into a single EXE to speed up game play. First successful compiles have been completed in Linux / Mac OS, so builds should be available for major distros with the next release.

All code features have been thoroughly tested with known bugs documented (including those from the original game - that I am aware of) and being worked on.

Code has been documented everywhere possible and cleaned up - including   global file and team selection routine. Game code is consistent in flow and naming across eacch sport. Improvements in logic and variable names have been made to code and error messages and handling are now more consistent.

A few accuracy improvements have been made and support files have been created. Some user interface improvements are under way. 

## Road Map ##
### Phase 1 ###
This phase will get the games available to the general public in a steady and reliable shape.
**Completed.**

### Phase 2 ###
This phase will start to identify these as new games, making improvements to the flow and logic without impacting the styling.
**Completed.**

### Phase 3 ###
This phase will see the games take on updated rules and correct accuracy. All known bugs will be squashed and user files will be prepared to distribute with the game.
**Completed.**

### The Future ###
- Updating code to work with databases instead of flat files.
- Bringing modern interface concepts to the design (including GUI) [IN PROGRESS!]

# The Fine Print #
### License ###
Affero GNU GPL v3

### Contributing ###
Contact me directly if interested in contributing. Only considering people who share my vision for these games and are able to honor Lance's work and legacy.

### Authors / Acknowledgment ###
- Manual images created by [Cool Clips](http://search.coolclips.com/)
- Game icons created by [Joseph Gan (Icon-Icons)](https://icon-icons.com/users/XHgnBHIfJk0ZlajSy0A0x/icon-sets/)
- Basketball-court icons created by [Freepik - Flaticon](https://www.flaticon.com/free-icons/basketball-court)
- Save icons created by [Those Icons - Flaticon](https://www.flaticon.com/free-icons/save)
- File explorer icons created by [juicy_fish - Flaticon](https://www.flaticon.com/free-icons/file-explorer)
- Buzzer, Whistle and Swish sounds courtesy of [Audio Jungle / Envato] (https://audiojungle.com)
- Crowd sound courtesy of [Mike_Koenig] (https://soundbible.com)
- Backboard sound retrieved from [Pixabay](https://pixabay.com)
- I am responsible for maintaining / updating most of the code and will spend equal effort to code cleanup as to code improvement.
- Lance Haffner, for his hard work over 2 decades giving people endless hours of playable sports games and making dreams come true.
- [Lance Haffner Sports Sims](https://www.facebook.com/groups/115923978469802) Facebook group admin Philip Lupi, for his hard work keeping Lance's games alive and active with fans old and new.
- Jason Lamb, for lending his expertise in BASIC to helping me learn some standards and rewriting the menu functions.
- Randy Dudley, original author of the stats viewer, for his thorough beta testing and attention to detail with stats / leaders
- Thomas Humphrey, for being persistent in bringing up issues with the no-shot-clock era and being patient for me to work on it
